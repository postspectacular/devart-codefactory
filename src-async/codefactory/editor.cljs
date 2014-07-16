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
   [codefactory.treeviz :as viz]
   [thi.ng.cljs.async :as async]
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.route :as route]
   [thi.ng.cljs.utils :as utils]
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

;; FIXME pass model/scene to next controller
(defn submit-model
  [bus local]
  (let [{:keys [tree seed-id]} @local]
    (async/publish bus :broadcast-tree [tree seed-id])
    (route/set-route! "objects" "submit")))

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
  (when (:active? @local)
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
  [canvas ball events bus]
  (go
    (loop [state nil]
      (let [[[e data] ch] (alts! events)]
        (when e
          (recur
           (case e
             :drag-start (let [[x y] (:p data)
                               h (.-clientHeight canvas)]
                           (when (m/in-range? 0 h y)
                             (arcball/down ball x (- h y)))
                           state)
             :drag-move  (let [[x y] (:p data)
                               h (.-clientHeight canvas)]
                           (when (and (m/in-range? 0 h y)
                                      (arcball/drag ball x (- h y)))
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

             :gesture-end (do (arcball/up ball) state)
             state)))))))

(defn handle-buttons
  [bus local module-timeout]
  (let [[continue] (async/event-channel "#edit-submit" "click")
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
              {:keys [subs events]} @local]
          (debug :release-editor)
          (swap! local assoc :active? false)
          (async/unsubscribe-and-close-many bus subs)
          (dorun (map dom/destroy-event-channel events))
          (recur))))))

(defn handle-tree-broadcast
  [bus local]
  (let [ch (async/subscribe bus :broadcast-tree)]
    (go
      (loop []
        (let [[_ [tree seed]] (<! ch)]
          (debug :editor-tree-received tree seed)
          (swap! local assoc :tree tree :seed-id seed)
          (recur))))))

(defn render-loop
  [bus local]
  (let [ch (async/subscribe bus :render-scene)
        render-fn (fn [& _] (render-scene local))]
    (go
      (loop []
        (let [_ (<! ch)]
          (anim/animframe-provider render-fn)
          (recur))))))

(defn init-arcball
  [config params]
  (let [id (keyword (:seed-id params))
        {:keys [view dist]} (get-in config [:seeds id :initial-view])]
    (arcball/make-arcball :init view :dist dist)))

(defn init-tree
  [state local seed]
  (-> (if-let [tree (:tree @local)]
        (tree/recompute-tree-with-seed state tree seed)
        (tree/init-tree-with-seed state seed))
      (tree/update-meshes false)))

(defn init
  [bus config]
  (let [canvas     (dom/by-id "edit-canvas")
        init       (async/subscribe bus :init-editor)
        local      (atom {})]

    (go
      (loop []
        (let [[_ [_ params]] (<! init)
              canvas  (dom/by-id "edit-canvas")
              subs    (async/subscription-channels
                       bus [:window-resize :user-action])
              e-specs [(async/event-channel canvas "mousedown" gest/mouse-gesture-start)
                       (async/event-channel canvas "mousemove" gest/mouse-gesture-move)
                       (async/event-channel js/window "mouseup" gest/gesture-end)
                       (async/event-channel js/window (dom/wheel-event-type)
                                          gest/mousewheel-proxy)
                       (async/event-channel canvas "touchstart" gest/touch-gesture-start)
                       (async/event-channel canvas "touchmove" gest/touch-gesture-move)
                       (async/event-channel js/window "touchend" gest/gesture-end)]
              events  (mapv first e-specs)
              arcball (init-arcball config params)
              now     (utils/now)]
          (debug :init-editor params)
          (reset!
           local
           (-> (webgl/init-webgl canvas (:webgl config))
               (merge
                {:config      config
                 :bg-col      (get-in config [:webgl :bg-col])
                 :subs        subs
                 :events      e-specs
                 :arcball     arcball
                 :last-action now
                 :start-time  now
                 :selection   nil
                 :sel-type    nil
                 :sel-time    now
                 :time        now
                 :active?     true})
               (init-tree local (:seed-id params))))
          (viz/init local bus config)
          (resize-canvas local)
          (render-scene local)

          (handle-resize        (:window-resize subs) local)
          (handle-reset-timeout (:user-action subs) local)
          (handle-arcball       canvas arcball events bus)
          (handle-buttons       bus local (get-in config [:timeouts :editor]))
          (recur))))

    (render-loop bus local)
    (handle-release bus local)
    (handle-tree-broadcast bus local)
    ))
