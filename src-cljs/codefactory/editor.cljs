(ns codefactory.editor
  (:require-macros
   [cljs.core.async.macros :refer [go]]
   [thi.ng.macromath.core :as mm])
  (:require
   [cljs.core.async :as cas :refer [>! <! alts! chan put! close! timeout]]
   [codefactory.editor.tree :as tree]
   [codefactory.editor.operators :as ops]
   [codefactory.editor.node-edit :as nedit]
   [codefactory.editor.render :as render]
   [codefactory.editor.tooltips :as tips]
   [codefactory.editor.toolbar :as tools]
   [codefactory.config :as config]
   [codefactory.color :as col]
   [codefactory.webgl :as webgl]
   [codefactory.common :as common]
   [thi.ng.cljs.async :as async]
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.route :as route]
   [thi.ng.cljs.utils :as utils :refer [->px]]
   [thi.ng.cljs.dom :as dom]
   [thi.ng.cljs.gestures :as gest]
   [thi.ng.geom.ui.arcball :as arcball]
   [thi.ng.common.math.core :as m]))

(defn submit-model
  [bus local]
  (let [{:keys [tree seed-id history parent-id]} @local]
    (async/publish bus :broadcast-tree [tree seed-id history parent-id])
    (route/set-route! "objects" "submit")))

(defn relaunch-selector
  [bus local]
  (route/set-route! "select" (:seed-id @local)))

(defn handle-resize
  [ch bus local]
  (go
    (loop []
      (let [[_ size] (<! ch)]
        (when size
          (render/resize-canvas local)
          (async/publish bus :render-scene nil)
          (async/publish bus :update-toolbar-pos (-> @local :tools :offset))
          (recur))))))

(defn handle-buttons
  [bus local module-timeout]
  (let [[continue] (async/event-channel "#edit-continue" "click")
        [cancel]   (async/event-channel "#edit-cancel" "click")]
    (go
      (loop []
        (let [delay  (- module-timeout (- (utils/now) (:last-action @local)))
              [_ ch] (alts! [continue cancel (timeout delay)])]
          (cond
           (= continue ch)
           (submit-model bus local)

           (= cancel ch)
           (relaunch-selector bus local)

           (and (:active? @local)
                (>= (- (utils/now) (:last-action @local)) module-timeout))
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
        (<! ch)
        (let [{:keys [subs events tools gl display-meshes axes]} @local]
          (debug :release-editor display-meshes)
          (swap! local assoc :active? false)
          (async/unsubscribe-and-close-many bus subs)
          (dorun (map async/destroy-event-channel events))
          (tools/disable-presets (:specs tools))
          (tips/hide-tooltips)
          (webgl/delete-meshes gl (vals display-meshes))
          (webgl/delete-meshes gl (mapcat identity axes))
          (recur))))))

(defn handle-tree-broadcast
  [bus local]
  (let [ch (async/subscribe bus :broadcast-tree)]
    (go
      (loop []
        (let [[_ [tree seed history parent-id]] (<! ch)]
          (debug :editor-tree-received tree seed history parent-id)
          (swap!
           local assoc
           :tree tree
           :seed-id seed
           :history history
           :parent-id parent-id)
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
        local   (atom {:tools (tools/init bus toolbar)})]

    (go
      (loop []
        (let [[_ [_ params]] (<! init)
              subs    (async/subscription-channels
                       bus [:window-resize :user-action :camera-update])
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
              arcball  (render/init-arcball params)
              glconf   (:webgl config/app)
              aconf    (:axis glconf)
              now      (utils/now)
              state    (webgl/init-webgl canvas glconf)
              axes     (webgl/axis-meshes (:gl state) (:radius aconf) (:length aconf))
              t-offset (-> config/app :editor :toolbar-margin-left)
              intro?   (not (:tree @local))]
          (reset!
           local
           (-> state
               (merge
                {:bg-col           (:bg-col glconf)
                 :subs             subs
                 :events           (concat c-specs t-specs)
                 :arcball          arcball
                 :selection        nil
                 :sel-type         nil
                 :last-action      now
                 :start-time       now
                 :sel-time         now
                 :time             now
                 :active?          true
                 :history          (or (:history @local) [])
                 :parent-id        (:parent-id @local)
                 :tools            (:tools @local)
                 :axes             axes
                 :show-axes?       false
                 :intro-active?    intro?
                 :intro-id         -1
                 :axis-hint-shown? false})
               (init-tree (:tree @local) (:seed-id params))
               (assoc-in [:tools :curr-offset] t-offset)))
          (nedit/init local bus)
          (arcball/update-zoom-range arcball (:bounds @local))
          (render/resize-canvas local)
          (render/render-scene local)

          (handle-resize               (:window-resize subs) bus local)
          (handle-reset-timeout        (:user-action subs) local)
          (render/handle-arcball       canvas arcball c-events bus local)
          (render/handle-view-update   (:camera-update subs) arcball bus local)
          (tools/handle-toolbar-scroll toolbar t-events bus local)
          (handle-buttons              bus local (config/timeout :editor))

          (async/publish bus :update-toolbar-pos t-offset)
          (js/setTimeout
           (fn []
             (render/resize-canvas local)
             (if intro? (async/publish bus :intro-next nil)))
           850)

          (recur))))

    (render/render-loop          bus local)
    (handle-release              bus local)
    (handle-tree-broadcast       bus local)
    (handle-tree-backup          bus local)
    (init-extra-tools            bus local)
    (tools/handle-toolbar-update bus local toolbar)
    (tips/handle-tooltips        bus local)
    (tips/handle-tooltip-display bus local)
    (tips/handle-intro           bus local)))
