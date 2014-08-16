(ns codefactory.editor
  (:require-macros
   [cljs.core.async.macros :refer [go]]
   [thi.ng.macromath.core :as mm])
  (:require
   [cljs.core.async :as cas :refer [>! <! alts! chan put! close! timeout]]
   [codefactory.config :as config]
   [codefactory.color :as col]
   [codefactory.webgl :as webgl]
   [codefactory.common :as common]
   [codefactory.tree :as tree]
   [codefactory.operators :as ops]
   [codefactory.treeviz :as viz]
   [thi.ng.cljs.async :as async]
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.route :as route]
   [thi.ng.cljs.utils :as utils :refer [->px]]
   [thi.ng.cljs.dom :as dom]
   [thi.ng.cljs.gestures :as gest]
   [thi.ng.cljs.detect :as detect]
   [thi.ng.geom.webgl.core :as gl]
   [thi.ng.geom.webgl.animator :as anim]
   [thi.ng.geom.webgl.buffers :as buf]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.core.matrix :as mat :refer [M44]]
   [thi.ng.geom.core.vector :as v :refer [vec2 vec3]]
   [thi.ng.geom.rect :as r]
   [thi.ng.geom.ui.arcball :as arcball]
   [thi.ng.common.math.core :as m]))

(defn submit-model
  [bus local]
  (let [{:keys [tree seed-id history]} @local]
    (async/publish bus :broadcast-tree [tree seed-id history])
    (route/set-route! "objects" "submit")))

(defn relaunch-selector
  [bus local]
  (route/set-route! "select" (:seed-id @local)))

(defn render-scene
  [local]
  (when (:active? @local)
    (let [{:keys [gl arcball shaders proj display-meshes bounds
                  selection sel-type start-time sel-time bg-col]} @local
                  now         (utils/now)
                  time        (mm/subm now start-time 0.001)
                  view        (arcball/get-view arcball)
                  shared-unis {:view view
                               :model (g/translate M44 (g/- (g/centroid bounds)))
                               :proj proj
                               :normalMat (-> (g/invert view) (g/transpose))}]
      (apply gl/clear-color-buffer gl bg-col)
      (gl/clear-depth-buffer gl 1.0)
      (if selection
        (webgl/render-with-selection
         gl
         shaders
         shared-unis
         (vals (dissoc display-meshes selection))
         [(display-meshes selection)]
         (col/hex->rgb (config/operator-color sel-type))
         time sel-time)
        (webgl/render-meshes
         gl (shaders 1) (vals display-meshes) shared-unis nil))
      (when (:show-axes? @local)
        (webgl/render-axes
         gl (shaders 1) shared-unis (:axes @local) bounds)))))

(defn resize-canvas
  [local]
  (let [{:keys [gl canvas arcball]} @local
        [w h]     (dom/size (dom/parent canvas))
        view-rect (r/rect 0 0 w h)
        icon-size (get-in config/app [:editor :toolbar-icon-size 0])]
    (dom/set-attribs! canvas {:width w :height h})
    ;; (dom/set-style! (config/dom-component :preview-label) #js {:width (->px (- w icon-size 30))})
    (dom/set-style! (config/dom-component :toolbar-label)
                    #js {:width (->px (- w 20)) :top (->px (- h 19))})
    (swap!
     local assoc
     :canvas-width w :canvas-height h
     :view-rect view-rect
     :proj (gl/perspective 45 view-rect 0.1 30))
    (gl/set-viewport gl view-rect)
    (arcball/resize arcball w h)))

(defn handle-resize
  [ch bus local]
  (go
    (loop []
      (let [[_ size] (<! ch)]
        (when size
          (resize-canvas local)
          (async/publish bus :render-scene nil)
          (async/publish bus :update-toolbar-pos (-> @local :tools :offset))
          (recur))))))

(defn handle-arcball
  [canvas ball events bus local]
  (go
    (loop [state nil]
      (let [[[e data] ch] (alts! events)]
        (when e
          (recur
           (case e
             :drag-start (let [[x y] (:p data)
                               h (.-clientHeight canvas)]
                           (arcball/down ball x (- h y))
                           (swap! local assoc :view-tween-cancel? true)
                           state)
             :drag-move  (let [[x y] (:p data)
                               h (.-clientHeight canvas)]
                           (when (arcball/drag ball x (- h y))
                             (async/publish bus :render-scene nil))
                           state)

             :dual-start [(:dist data) 0]

             :dual-move (let [[start-dist delta] state
                              abs-delta (- (:dist data) start-dist)
                              delta' (mm/subm delta abs-delta 0.1)]
                          (arcball/zoom-delta ball delta')
                          (async/publish bus :render-scene nil)
                          [start-dist delta'])
             :mouse-wheel (let [delta (:delta data)]
                            (arcball/zoom-delta ball delta)
                            (async/publish bus :render-scene nil)
                            state)

             :gesture-end (do
                            (arcball/up ball)
                            (swap! local assoc :view-tween-cancel? false)
                            state)
             state)))))))

(defn handle-toolbar-scroll
  [toolbar events bus local]
  (go
    (loop [state nil]
      (let [[[e data] ch] (alts! events)]
        (when e
          (recur
           (case e
             :drag-start (let [offset (get-in @local [:tools :offset] 0)
                               target (common/next-parent-id (:target data))]
                           (swap! local assoc-in [:tools :active?] true)
                           [offset (:p data) (vec2) (keyword target)])
             :drag-move  (if state
                           (let [[offset p _ target] state
                                 delta (g/- (:p data) p)
                                 offset' (mm/madd (:x delta) 2 offset)]
                             (async/publish bus :update-toolbar-pos offset')
                             [offset p delta target])
                           state)
             :gesture-end (when state
                            (let [[_ p delta target] state
                                  dist (g/mag delta)]
                              (when (and (:touch? data) (< dist 20))
                                ;;(debug :end-touch target dist)
                                (if (= target :undo)
                                  (async/publish bus :undo-triggered target)
                                  (async/publish bus :op-triggered target)))
                              (swap! local assoc-in [:tools :active?] false)
                              nil))
             nil)))))))

(defn handle-toolbar-update
  [bus local toolbar]
  (let [update (async/subscribe bus :update-toolbar)
        pos (async/subscribe bus :update-toolbar-pos)]
    (go
      (loop []
        (<! update)
        (let [{:keys [offset curr-offset]} (:tools @local)
              curr-offset (m/mix curr-offset offset (-> config/app :editor :toolbar-speed))]
          (swap! local assoc-in [:tools :curr-offset] curr-offset)
          (dom/set-style! toolbar (clj->js {:marginLeft (->px curr-offset)}))
          (if (> (m/abs-diff curr-offset offset) 0.5)
            (when-not (:toolbar-frame @local)
              (swap!
               local assoc
               :toolbar-frame (anim/animframe-provider #(async/publish bus :update-toolbar nil)))))
          (swap! local dissoc :toolbar-frame))
        (recur)))

    (go
      (loop []
        (let [[_ offset] (<! pos)
              {max :toolbar-margin-left
               right :toolbar-margin-right} (:editor config/app)
               {:keys [width]} (:tools @local)
               min (- (.-innerWidth js/window) width right)
               offset (m/clamp offset min max)]
          (swap! local assoc-in [:tools :offset] offset)
          (when-not (:toolbar-frame @local)
            (async/publish bus :update-toolbar nil)))
        (recur)))))

(defn handle-buttons
  [bus local module-timeout]
  (let [[continue] (async/event-channel "#edit-continue" "click")
        [cancel]   (async/event-channel "#edit-cancel" "click")]
    (go
      (loop []
        (let [delay (- module-timeout (- (utils/now) (:last-action @local)))
              [_ ch] (alts! [continue cancel (timeout delay)])]
          (cond
           (= continue ch)
           (submit-model bus local)

           (= cancel ch)
           (relaunch-selector bus local)

           (>= (- (utils/now) (:last-action @local)) module-timeout)
           (route/set-route! "home")

           :else (recur)))))))

(defn handle-reset-timeout
  [ch local]
  (go
    (loop []
      (when (<! ch)
        (swap! local assoc :last-action (utils/now))
        (recur)))))

(def tooltip-element (memoize #(dom/by-id (str (name %) "-tip"))))

(defn add-tooltip-buttons
  [el bus local]
  (when (:intro-active? @local)
    (let [div (dom/create! "div" el)
          skip (dom/create! "input" div {:type "button" :value "skip"})
          next (dom/create! "input" div {:type "button" :value "next"})]
      (dom/add-listeners
       [[skip "click" #(async/publish bus :intro-done nil)]
        [next "click" #(async/publish bus :intro-next nil)]]))))

(defn handle-tooltip-display
  [bus local]
  (let [show     (async/subscribe bus :show-tooltip)
        hide     (async/subscribe bus :hide-tooltip)
        tooltips (-> config/app :editor :tooltips)
        tip-body (memoize #(dom/query % ".tooltip-content"))]
    (go
      (loop []
        (let [[_ [el id]] (<! show)
              tip (tooltip-element id)
              {:keys [offset intro-offset content auto?]} (tooltips id)
              body (tip-body tip)
              intro? (:intro-active? @local)
              offset (if intro? (or intro-offset offset) offset)
              [x y] (g/+ (vec2 (dom/offset el))
                         (if (fn? offset) (offset el) offset))]
          (dom/set-html! body content)
          (add-tooltip-buttons body bus local)
          (-> tip
              (dom/set-style! (clj->js {:display "block" :left (->px x) :top (->px y)}))
              (dom/remove-class! "hidden"))
          (when auto?
            (js/setTimeout
             #(async/publish bus :hide-tooltip id)
             (config/timeout :tooltip)))
          (recur))))
    (go
      (loop []
        (let [[_ id] (<! hide)]
          (dom/set-style! (tooltip-element id) #js {:display "none"})
          (recur))))))

(defn handle-tooltips
  [bus local]
  (let [tooltips (-> config/app :editor :tooltips)
        tips     (->> tooltips
                      (filter (comp :user? val))
                      keys
                      (mapv #(-> % name dom/by-id (dom/query "svg"))))
        channels (fn [ev] (set (map #(first (async/event-channel % ev)) tips)))
        on       (channels "mouseenter")
        off      (channels "mouseleave")
        touch    (channels "touchstart")
        all      (vec (concat on off touch))]
    (go
      (loop [state {}]
        (let [[e ch] (alts! all)
              el (.-target e)
              id (-> el dom/parent (dom/get-attribs ["id"]) first)
              kid (keyword id)
              show? (or (on ch) (and (touch ch) (not (state kid))))]
          (when (and id (not (:intro-active? @local)))
            (if show?
              (async/publish bus :show-tooltip [el kid])
              (async/publish bus :hide-tooltip kid)))
          (async/publish bus :user-action nil)
          (recur (assoc state kid show?)))))))

(defn hide-tooltips
  []
  (->> (-> config/app :editor :tooltips)
       keys
       (map #(-> % tooltip-element (dom/add-class! "hidden")))
       (dorun)))

(defn handle-intro
  [bus local]
  (let [next (async/subscribe bus :intro-next)
        done (async/subscribe bus :intro-done)
        tips (-> config/app :editor :intro)]
    (go
      (loop []
        (<! next)
        (let [id  (:intro-id @local)
              id' (inc id)]
          (when (>= id 0)
            (async/publish bus :hide-tooltip (tips id)))
          (if (< id' (count tips))
            (let [kid (tips id')
                  el  (if-not (= :edit-canvas kid)
                        (-> kid name dom/by-id (dom/query "svg"))
                        (-> kid name dom/by-id))]
              (swap! local assoc-in [:intro-id] id')
              (async/publish bus :show-tooltip [el kid]))
            (async/publish bus :intro-done nil)))
        (async/publish bus :user-action nil)
        (recur)))

    (go
      (loop []
        (<! done)
        (let [id  (:intro-id @local)]
          (when (>= id 0)
            (async/publish bus :hide-tooltip (tips id)))
          (swap! local assoc :intro-active? false)
          ;; TODO reset camera
          (async/publish bus :regenerate-scene nil))
        (recur)))))

(defn handle-release
  [bus local]
  (let [ch (async/subscribe bus :release-editor)]
    (go
      (loop []
        (<! ch)
        (let [{:keys [subs events tools gl display-meshes]} @local]
          (debug :release-editor display-meshes)
          (swap! local assoc :active? false)
          (async/unsubscribe-and-close-many bus subs)
          (dorun (map async/destroy-event-channel events))
          (ops/disable-presets (:specs tools))
          (hide-tooltips)
          (webgl/delete-meshes gl (vals display-meshes))
          (recur))))))

(defn handle-tree-broadcast
  [bus local]
  (let [ch (async/subscribe bus :broadcast-tree)]
    (go
      (loop []
        (let [[_ [tree seed history]] (<! ch)]
          (debug :editor-tree-received tree seed history)
          (swap! local assoc :tree tree :seed-id seed :history history)
          (when-not tree
            (swap! local assoc :history []))
          (recur))))))

(defn handle-tree-backup
  [bus local]
  (let [ch (async/subscribe bus :backup-tree)]
    (go
      (loop []
        (let [[_ tree] (<! ch)]
          (swap! local update-in [:history] conj tree)
          (dom/remove-class! (dom/by-id "undo") "disabled")
          (debug :backup-tree (count (:history @local)) tree)
          (recur))))))

(defn init-view-tween
  [local ball target]
  (swap! local assoc
         :view {:start (arcball/get-rotation ball)
                :target target
                :phase 0}))

(defn end-view-tween
  [local]
  (swap!
   local assoc
   :view-tween-cancel? false
   :view-tween? false))

(defn tween-view!
  [ball local]
  (go
    (loop []
      (<! (timeout 16))
      (let [{{:keys [start target phase]} :view
             cancel? :view-tween-cancel?} @local]
        (if-not cancel?
          (let [phase (if (>= phase 0.99) 1.0 (m/mix phase 1.0 0.15))]
            (arcball/set-rotation ball (g/mix start target phase))
            (swap! local assoc-in [:view :phase] phase)
            (if (>= phase 0.995)
              (end-view-tween local)
              (recur)))
          (end-view-tween local))))))

(defn handle-view-update
  [ch ball bus local]
  (go
    (loop []
      (let [[_ target] (<! ch)]
        (when target
          (init-view-tween local ball target)
          (when-not (:view-tween? @local)
            (swap! local assoc :view-tween? true)
            (tween-view! ball local))
          (recur))))))

(defn render-loop
  [bus local]
  (let [ch (async/subscribe bus :render-scene)
        render-fn (fn render*
                    [& _]
                    (render-scene local)
                    (swap!
                     local assoc
                     :render-frame
                     (if (or (and (:selection @local) (not detect/mobile?))
                             (:view-tween? @local))
                       (anim/animframe-provider render*))))]
    (go
      (loop []
        (let [_ (<! ch)]
          (when-not (:render-frame @local)
            (swap! local assoc :render-frame (anim/animframe-provider render-fn)))
          (recur))))))

(defn init-arcball
  [params]
  (let [id (keyword (:seed-id params))
        {:keys [view dist]} (-> config/seeds id :initial-view)]
    (arcball/make-arcball :init view :dist dist)))

(defn init-tree
  [state tree seed]
  (-> (if tree
        (tree/recompute-tree-with-seed state tree seed)
        (tree/init-tree-with-seed state seed))
      (tree/update-meshes false)))

(defn init-extra-tools
  [bus local]
  (let [tools (dom/query nil "#editor .tools-extra")
        icons (:icons config/app)
        {size :toolbar-icon-size delta :zoom-delta} (:editor config/app)
        zoom (fn [dir]
               (fn []
                 (arcball/zoom-delta (:arcball @local) (* dir delta))
                 (async/publish bus :render-scene nil)))
        paths #(-> icons % :paths)]
    (common/icon-button
     tools nil size (paths :fullscreen) nil
     (fn [] (dom/request-fullscreen))
     "fs-toggle")
    (common/icon-button
     tools "axis-toggle" size (paths :axis) nil
     (fn []
       (swap! local update-in [:show-axes?] not)
       (async/publish bus :render-scene nil)))
    (common/icon-button
     tools "zoom-in" size (paths :zoom-in) nil (zoom -1))
    (common/icon-button
     tools "zoom-out" size (paths :zoom-out) nil (zoom 1))))

(defn init
  [bus]
  (let [canvas  (config/dom-component :edit-canvas)
        toolbar (config/dom-component :tools)
        init    (async/subscribe bus :init-editor)
        local   (atom {:tools (ops/init-op-triggers bus toolbar)})]

    (go
      (loop []
        (let [[_ [_ params]] (<! init)
              subs     (async/subscription-channels
                        bus [:window-resize
                             :user-action
                             :camera-update])
              c-specs (mapv
                       #(apply async/event-channel %)
                       [[canvas "mousedown" gest/mouse-gesture-start]
                        [canvas "mousemove" gest/mouse-gesture-move]
                        [js/window "mouseup" gest/gesture-end]
                        [js/window (dom/wheel-event-type) gest/mousewheel-proxy]
                        [canvas "touchstart" gest/touch-gesture-start]
                        [canvas "touchmove" gest/touch-gesture-move]
                        [js/window "touchend" gest/gesture-end]])
              t-specs  (mapv
                        #(apply async/event-channel %)
                        [[toolbar "mousedown" gest/mouse-gesture-start]
                         [toolbar "mousemove" gest/mouse-gesture-move]
                         [js/window "mouseup" gest/gesture-end]
                         [toolbar "touchstart" gest/touch-gesture-start]
                         [toolbar "touchmove" gest/touch-gesture-move]
                         [js/window "touchend" gest/gesture-end]])
              c-events (mapv first c-specs)
              t-events (mapv first t-specs)
              arcball  (init-arcball params)
              glconf   (:webgl config/app)
              aconf    (:axis glconf)
              now      (utils/now)
              state    (webgl/init-webgl canvas glconf)
              axes     (webgl/axis-meshes (:gl state) (:radius aconf) (:length aconf))
              t-offset (-> config/app :editor :toolbar-margin-left)
              intro?   (not (:tree @local))]
          (debug :init-editor params)
          (reset!
           local
           (-> state
               (merge
                {:bg-col      (:bg-col glconf)
                 :subs        subs
                 :events      (concat c-specs t-specs)
                 :arcball     arcball
                 :selection   nil
                 :sel-type    nil
                 :last-action now
                 :start-time  now
                 :sel-time    now
                 :time        now
                 :active?     true
                 :history     (or (:history @local) [])
                 :tools       (:tools @local)
                 :axes        axes
                 :show-axes?  false
                 :intro-active? intro?
                 :intro-id -1
                 :axis-hint-shown? false})
               (init-tree (:tree @local) (:seed-id params))
               (assoc-in [:tools :curr-offset] t-offset)))
          (viz/init local bus)
          (arcball/update-zoom-range arcball (:bounds @local))
          (resize-canvas local)
          (render-scene local)

          (handle-resize         (:window-resize subs) bus local)
          (handle-reset-timeout  (:user-action subs) local)
          (handle-arcball        canvas arcball c-events bus local)
          (handle-toolbar-scroll toolbar t-events bus local)
          (handle-view-update    (:camera-update subs) arcball bus local)
          (handle-buttons        bus local (config/timeout :editor))

          (async/publish bus :update-toolbar-pos t-offset)
          (js/setTimeout
           (fn []
             (resize-canvas local)
             (if intro? (async/publish bus :intro-next nil)))
           850)

          (recur))))

    (render-loop            bus local)
    (handle-release         bus local)
    (handle-tree-broadcast  bus local)
    (handle-tree-backup     bus local)
    (handle-toolbar-update  bus local toolbar)
    (handle-tooltips        bus local)
    (handle-tooltip-display bus local)
    (handle-intro           bus local)
    (init-extra-tools       bus local)))
