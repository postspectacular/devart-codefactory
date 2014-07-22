(ns codefactory.editor
  (:require-macros
   [cljs.core.async.macros :refer [go]]
   [thi.ng.macromath.core :as mm])
  (:require
   [cljs.core.async :as cas :refer [>! <! alts! chan put! close! timeout]]
   [codefactory.config :as config]
   [codefactory.color :as col]
   [codefactory.webgl :as webgl]
   [codefactory.shared :as shared]
   [codefactory.tree :as tree]
   [codefactory.operators :as ops]
   [codefactory.treeviz :as viz]
   [thi.ng.cljs.async :as async]
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.route :as route]
   [thi.ng.cljs.utils :as utils :refer [->px]]
   [thi.ng.cljs.dom :as dom]
   [thi.ng.cljs.io :as io]
   [thi.ng.cljs.gestures :as gest]
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
  (let [{:keys [tree seed-id]} @local]
    (async/publish bus :broadcast-tree [tree seed-id])
    (route/set-route! "objects" "submit")))

(defn load-model
  [bus id]
  (io/request
   :uri     (str (config/api-route :get-object) id)
   :method  :get
   :edn?    true
   :success (fn [_ data]
              (async/publish
               bus :editor-get-model-success
               {:uuid id
                :seed-id (:seed data)
                :tree (:tree data)}))
   :error   (fn [status body]
              (async/publish bus :editor-get-model-fail [status body]))))

(defn init-model
  [bus {:keys [id seed-id]}]
  (if id
    (load-model bus id)
    (async/publish bus :editor-select-seed seed-id)))

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
         time
         sel-time)
        (webgl/render-meshes
         gl (shaders 1) (vals display-meshes) shared-unis nil)))))

(defn resize-canvas
  [local]
  (let [{:keys [gl canvas arcball]} @local
        [w h] (dom/size (dom/parent canvas))
        view-rect (r/rect 0 0 w h)]
    (dom/set-attribs! canvas {:width w :height h})
    (dom/set-style! (config/dom-component :preview-label)
                    #js {:width (->px (- w 20))})
    (dom/set-style! (config/dom-component :toolbar-label)
                    #js {:width (->px (- w 20)) :top (->px (- h 19))})
    (swap!
     local assoc
     :canvas-width w :canvas-height h
     :view-rect view-rect
     :proj (gl/perspective 45 view-rect 0.1 10))
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
          (async/publish bus :update-toolbar (-> @local :tools :offset))
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
                               target (loop [el (:target data)]
                                        (if-let [id (first (dom/get-attribs el ["id"]))]
                                          id (recur (dom/parent el))))]
                           (swap! local assoc-in [:tools :active?] true)
                           [offset (:p data) (vec2) (keyword target)])
             :drag-move  (if state
                           (let [[offset p _ target] state
                                 delta (g/- (:p data) p)
                                 offset' (mm/madd (:x delta) 2 offset)]
                             (async/publish bus :update-toolbar offset')
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
  (let [ch (async/subscribe bus :update-toolbar)]
    (go
      (loop []
        (let [[_ offset] (<! ch)
              {max :toolbar-margin-left
               right :toolbar-margin-right} (:editor config/app)
               min (- (.-innerWidth js/window) (-> @local :tools :width) right)
               offset (m/clamp offset min max)]
          (swap! local assoc-in [:tools :offset] offset)
          (dom/set-style! toolbar (clj->js {:marginLeft (->px offset)})))
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
           (route/set-route! "select" (:seed-id @local))

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

(defn handle-release
  [bus local]
  (let [ch (async/subscribe bus :release-editor)]
    (go
      (loop []
        (let [_ (<! ch)
              {:keys [subs events tools]} @local]
          (debug :release-editor)
          (swap! local assoc :active? false)
          (async/unsubscribe-and-close-many bus subs)
          (dorun (map async/destroy-event-channel events))
          (ops/disable-presets (:specs tools))
          (recur))))))

(defn handle-tree-broadcast
  [bus local]
  (let [ch (async/subscribe bus :broadcast-tree)]
    (go
      (loop []
        (let [[_ [tree seed]] (<! ch)]
          (debug :editor-tree-received tree seed)
          (swap! local assoc :tree tree :seed-id seed)
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

(defn handle-view-update
  [ch ball bus local]
  (go
    (loop []
      (let [[_ target] (<! ch)]
        (when target
          (init-view-tween local ball target)
          (when-not (:view-tween? @local)
            (swap! local assoc :view-tween? true)
            (go
              (loop []
                (<! (timeout 16))
                (let [{{:keys [start target phase]} :view
                       cancel? :view-tween-cancel?} @local]
                  (if-not cancel?
                    (do
                      (arcball/set-rotation ball (g/mix start target (min phase 1.0)))
                      (swap! local assoc-in [:view :phase] (m/mix phase 1.0 0.1))
                      ;;(render-scene local)
                      (if (>= phase 0.9995)
                        (end-view-tween local)
                        (recur)))
                    (end-view-tween local))))))
          (recur))))))

(defn handle-tooltips
  [tooltips]
  (let [tips      (->> tooltips keys (mapv #(-> % name dom/by-id (dom/query "svg"))))
        chan      (fn [ev] #(first (async/event-channel % ev)))
        on-chans  (mapv (chan "mouseenter") tips)
        off-chans (mapv (chan "mouseleave") tips)]
    (go
      (loop []
        (let [[e] (alts! on-chans)
              id (-> (.-target e) dom/parent (dom/get-attribs ["id"]) first)
              tip (dom/by-id (str id "-tip"))
              {:keys [offset content]} (tooltips (keyword id))
              [x y] (g/+ (vec2 (dom/offset (.-target e))) offset)]
          (dom/set-text! (dom/query tip ".popover-content") content)
          (dom/set-style! tip (clj->js {:display "block" :left (->px x) :top (->px y)}))
          (recur))))
    (go
      (loop []
        (let [[e] (alts! off-chans)
              id (-> (.-target e) dom/parent (dom/get-attribs ["id"]) first)]
          (dom/set-style! (dom/by-id (str id "-tip")) #js {:display "none"})
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
                     (if (or (:selection @local)
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
  [state local seed]
  (-> (if-let [tree (:tree @local)]
        (tree/recompute-tree-with-seed state tree seed)
        (tree/init-tree-with-seed state seed))
      (tree/update-meshes false)))

(defn init
  [bus]
  (let [canvas     (config/dom-component :edit-canvas)
        toolbar    (config/dom-component :tools)
        init       (async/subscribe bus :init-editor)
        local      (atom {:tools (ops/init-op-triggers bus toolbar)})]
    ;;(debug :tools (:specs (:tools @local)))

    (go
      (loop []
        (let [[_ [_ params]] (<! init)
              subs     (async/subscription-channels
                        bus [:window-resize
                             :user-action
                             :camera-update])
              c-specs  [(async/event-channel canvas "mousedown" gest/mouse-gesture-start)
                        (async/event-channel canvas "mousemove" gest/mouse-gesture-move)
                        (async/event-channel js/window "mouseup" gest/gesture-end)
                        (async/event-channel js/window (dom/wheel-event-type)
                                             gest/mousewheel-proxy)
                        (async/event-channel canvas "touchstart" gest/touch-gesture-start)
                        (async/event-channel canvas "touchmove" gest/touch-gesture-move)
                        (async/event-channel js/window "touchend" gest/gesture-end)]
              t-specs  [(async/event-channel toolbar "mousedown" gest/mouse-gesture-start)
                        (async/event-channel toolbar "mousemove" gest/mouse-gesture-move)
                        (async/event-channel js/window "mouseup" gest/gesture-end)
                        (async/event-channel toolbar "touchstart" gest/touch-gesture-start)
                        (async/event-channel toolbar "touchmove" gest/touch-gesture-move)
                        (async/event-channel js/window "touchend" gest/gesture-end)]
              c-events (mapv first c-specs)
              t-events (mapv first t-specs)
              arcball  (init-arcball params)
              now      (utils/now)
              glconf   (:webgl config/app)]
          (debug :init-editor params)
          (reset!
           local
           (-> (webgl/init-webgl canvas glconf)
               (merge
                {:bg-col      (:bg-col glconf)
                 :subs        subs
                 :events      (concat c-specs t-specs)
                 :arcball     arcball
                 :last-action now
                 :start-time  now
                 :selection   nil
                 :sel-type    nil
                 :sel-time    now
                 :time        now
                 :active?     true
                 :history     []
                 :tools       (:tools @local)})
               (init-tree local (:seed-id params))))
          (viz/init local bus)
          (resize-canvas local)
          (render-scene local)

          (handle-resize         (:window-resize subs) bus local)
          (handle-reset-timeout  (:user-action subs) local)
          (handle-arcball        canvas arcball c-events bus local)
          (handle-toolbar-scroll toolbar t-events bus local)
          (handle-view-update    (:camera-update subs) arcball bus local)
          (handle-buttons        bus local (config/timeout :editor))

          (async/publish bus :update-toolbar (-> config/app :editor :toolbar-margin-left))
          (go (<! (timeout 800)) (resize-canvas local))

          (recur))))

    (render-loop bus local)
    (handle-release bus local)
    (handle-tree-broadcast bus local)
    (handle-tree-backup bus local)
    (handle-toolbar-update bus local toolbar)
    (handle-tooltips (get-in config/app [:editor :tooltips]))))
