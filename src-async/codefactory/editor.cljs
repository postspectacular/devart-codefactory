(ns codefactory.editor
  (:require-macros
   [cljs.core.async.macros :as asm :refer [go]]
   [thi.ng.macromath.core :as mm])
  (:require
   [cljs.core.async :as cas :refer [>! <! alts! chan put! close! timeout]]
   [codefactory.config :as config]
   [codefactory.color :as col]
   [codefactory.webgl :as webgl]
   [codefactory.shared :as shared]
   [codefactory.tree :as tree]
   [codefactory.treedit :as tedit]
   [thi.ng.cljs.async :as async]
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.route :as route]
   [thi.ng.cljs.utils :as utils]
   [thi.ng.cljs.dom :as dom]
   [thi.ng.cljs.io :as io]
   [thi.ng.geom.webgl.core :as gl]
   [thi.ng.geom.webgl.animator :as anim]
   [thi.ng.geom.webgl.buffers :as buf]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.core.matrix :as mat :refer [M44]]
   [thi.ng.geom.core.vector :as v :refer [vec2 vec3]]
   [thi.ng.geom.rect :as r]
   [thi.ng.geom.ui.arcball :as arcball]
   [thi.ng.common.math.core :as m]))

;; FIXME pass model/scene to next controller
(defn submit-model
  [local] (route/set-route! "objects" "submit"))

(defn load-model
  [bus config id]
  (io/request
   :uri     (str (get-in config [:api-routes :get-object]) id)
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
  [bus config {:keys [id seed-id]}]
  (if id
    (load-model bus config id)
    (async/publish bus :editor-select-seed seed-id)))

(defn render-scene
  [local]
  (if (:active? @local)
    (let [{:keys [gl arcball shaders proj display-meshes bounds
                  selection sel-type start-time sel-time bg-col config]} @local
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
         (col/hex->rgb (config/operator-color config sel-type))
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
    (swap!
     local merge
     {:canvas-width w :canvas-height h
      :view-rect view-rect
      :proj (gl/perspective 45 view-rect 0.1 10)})
    (gl/set-viewport gl view-rect)
    (arcball/resize arcball w h)))

(defn handle-resize
  [ch local]
  (go
    (loop []
      (let [[_ size] (<! ch)]
        (when size
          (resize-canvas local)
          (render-scene local)
          (recur))))))

(defn handle-arcball
  [canvas arcball events bus]
  (go
    (loop []
      (let [[e ch] (alts! events)]
        (when e
          (cond

           (or (= ch (events 0))
               (= ch (events 4)))
           (let [x (.-clientX e)
                 y (.-clientY e)
                 h (.-clientHeight canvas)]
             (when (m/in-range? 0 h y)
               (arcball/down arcball x (- h y))))

           (or (= ch (events 1))
               (= ch (events 5)))
           (let [x (.-clientX e)
                 y (.-clientY e)
                 h (.-clientHeight canvas)]
             (when (and (m/in-range? 0 h y)
                        (arcball/drag arcball x (- h y)))
               (async/publish bus :render-scene nil)))

           (or (= ch (events 2))
               (= ch (events 6)))
           (arcball/up arcball)

           (events 3) (let [delta (or (aget e "deltaY") (aget e "wheelDeltaY"))]
                        (arcball/zoom-delta arcball delta)
                        (async/publish bus :render-scene nil))

           :else (debug :ev e))
          (recur))))))

(defn handle-buttons
  [continue cancel module-timeout local]
  (go
    (loop []
      (let [delay (- module-timeout (- (utils/now) (:last-action @local)))
            [_ ch] (alts! [continue cancel (timeout delay)])]
        (cond
         (= continue ch)
         (submit-model local)

         (= cancel ch)
         (route/set-route! "select" (:seed-id @local))

         (>= (- (utils/now) (:last-action @local)) module-timeout)
         (route/set-route! "home")

         :else (recur))))))

(defn handle-reset-timeout
  [ch local]
  (go
    (loop []
      (when (<! ch)
        (swap! local assoc :last-action (utils/now))
        (recur)))))

(defn handle-release
  [ch bus local]
  (go
    (loop []
      (let [[_ [local]] (<! ch)
            {:keys [resize]} @local]
        (debug :release-editor)
        (swap! local assoc :active? false)
        (async/unsubscribe-and-close-many bus (:subs @local))
        (dorun (map dom/destroy-event-channel (:events @local)))
        (recur)))))

(defn render-loop
  [ch local]
  (let [render-fn (fn [& _] (render-scene local))]
    (go
      (loop []
        (let [_ (<! ch)]
          (anim/animframe-provider render-fn)
          (recur))))))

(defn init-arcball
  [config params]
  (let [{:keys [view dist]} (get-in config [:seeds (:seed-id params) :initial-view])]
    (arcball/make-arcball :init view :dist dist)))

(defn init
  [bus config]
  (let [canvas     (dom/by-id "edit-canvas")
        init       (async/subscribe bus :init-editor)
        release    (async/subscribe bus :release-editor)
        render     (async/subscribe bus :render-scene)
        [continue] (dom/event-channel "#edit-submit" "click")
        [cancel]   (dom/event-channel "#edit-cancel" "click")
        local      (atom nil)
        module-timeout (get-in config [:timeouts :editor])]

    (go
      (loop []
        (let [[_ [local params]] (<! init)
              canvas  (dom/by-id "edit-canvas")
              subs    {:resize (async/subscribe bus :window-resize)
                       :action (async/subscribe bus :user-action)}
              e-specs [(dom/event-channel canvas "mousedown")
                       (dom/event-channel canvas "mousemove")
                       (dom/event-channel canvas "mouseup")
                       (dom/event-channel js/window (dom/wheel-event-type))
                       (dom/event-channel canvas "touchstart" dom/touch-handler)
                       (dom/event-channel canvas "touchmove" dom/touch-handler)
                       (dom/event-channel canvas "touchend" dom/touch-handler)]
              events  (mapv first e-specs)
              arcball (init-arcball config params)
              now     (utils/now)]
          (debug :init-editor params)
          (reset!
           local
           (-> (webgl/init-webgl canvas (:webgl config))
               (merge
                {:config config
                 :bg-col (get-in config [:webgl :bg-col])
                 :subs subs
                 :events e-specs
                 :arcball arcball
                 :last-action now
                 :start-time now
                 :selection nil
                 :sel-time now
                 :time now
                 :active? true})
               (tree/init-tree-with-seed (:seed-id params))
               (tree/update-meshes false)))
          (tedit/init local bus config)
          (resize-canvas local)
          (render-scene local)
          (handle-resize (:resize subs) local)
          (handle-reset-timeout (:action subs) local)
          (handle-arcball canvas arcball events bus)
          (handle-buttons continue cancel module-timeout local)
          (recur))))

    (render-loop render local)
    (handle-release release bus local)
    ))
