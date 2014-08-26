(ns codefactory.editor.render
  (:require-macros
   [cljs.core.async.macros :refer [go]]
   [thi.ng.macromath.core :as mm])
  (:require
   [cljs.core.async :refer [<! alts! timeout]]
   [codefactory.config :as config]
   [codefactory.color :as col]
   [codefactory.webgl :as webgl]
   [thi.ng.cljs.async :as async]
   [thi.ng.cljs.log :refer [debug info warn]]
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

(defn render-scene
  [state]
  (when (:active? @state)
    (let [{:keys [gl arcball shaders proj display-meshes bounds
                  selection sel-type start-time sel-time bg-col]} @state
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
      (when (:show-axes? @state)
        (webgl/render-axes
         gl (shaders 1) shared-unis (:axes @state) bounds)))))

(defn render-loop
  [bus state]
  (let [ch (async/subscribe bus :render-scene)
        render-fn (fn render*
                    [& _]
                    (render-scene state)
                    (swap!
                     state assoc
                     :render-frame
                     (if (or (and (:selection @state) (not detect/mobile?))
                             (:view-tween? @state))
                       (anim/animframe-provider render*))))]
    (go
      (loop []
        (let [_ (<! ch)]
          (when-not (:render-frame @state)
            (swap! state assoc :render-frame (anim/animframe-provider render-fn)))
          (recur))))))

(defn resize-canvas
  [state]
  (let [{:keys [gl canvas arcball]} @state
        [w h]     (dom/size (dom/parent canvas))
        view-rect (r/rect 0 0 w h)
        icon-size (get-in config/app [:editor :toolbar-icon-size 0])]
    (dom/set-attribs! canvas {:width w :height h})
    (dom/set-style! (config/dom-component :toolbar-label)
                    {:width (->px (- w 20)) :top (->px (- h 19))})
    (swap!
     state assoc
     :canvas-width w :canvas-height h
     :view-rect view-rect
     :proj (gl/perspective 45 view-rect 0.1 30))
    (gl/set-viewport gl view-rect)
    (arcball/resize arcball w h)))

(defn init-view-tween
  [state ball target]
  (swap!
   state assoc
   :view {:start (arcball/get-rotation ball)
          :target target
          :phase 0}))

(defn end-view-tween
  [state]
  (swap!
   state assoc
   :view-tween-cancel? false
   :view-tween? false))

(defn tween-view!
  [ball state]
  (go
    (loop []
      (<! (timeout 16))
      (let [{{:keys [start target phase]} :view
             cancel? :view-tween-cancel?} @state]
        (if-not cancel?
          (let [phase (if (>= phase 0.99) 1.0 (m/mix phase 1.0 0.15))]
            (arcball/set-rotation ball (g/mix start target phase))
            (swap! state assoc-in [:view :phase] phase)
            (if (>= phase 0.995)
              (end-view-tween state)
              (recur)))
          (end-view-tween state))))))

(defn handle-view-update
  [ch ball bus state]
  (go
    (loop []
      (let [[_ target] (<! ch)]
        (when target
          (init-view-tween state ball target)
          (when-not (:view-tween? @state)
            (swap! state assoc :view-tween? true)
            (tween-view! ball state))
          (recur))))))

(defn handle-arcball
  [canvas ball events bus state]
  (go
    (loop [gesture-state nil]
      (let [[[e data] ch] (alts! events)]
        (when e
          (recur
           (case e
             :drag-start (let [[x y] (:p data)
                               h (.-clientHeight canvas)]
                           (arcball/down ball x (- h y))
                           (swap! state assoc :view-tween-cancel? true)
                           gesture-state)
             :drag-move  (let [[x y] (:p data)
                               h (.-clientHeight canvas)]
                           (when (arcball/drag ball x (- h y))
                             (async/publish bus :render-scene nil))
                           gesture-state)

             :dual-start [(:dist data) 0]

             :dual-move (let [[start-dist delta] gesture-state
                              abs-delta (- (:dist data) start-dist)
                              delta' (mm/subm delta abs-delta 0.1)]
                          (arcball/zoom-delta ball delta')
                          (async/publish bus :render-scene nil)
                          [start-dist delta'])
             :mouse-wheel (let [delta (:delta data)]
                            (arcball/zoom-delta ball delta)
                            (async/publish bus :render-scene nil)
                            gesture-state)

             :gesture-end (do
                            (arcball/up ball)
                            (swap! state assoc :view-tween-cancel? false)
                            gesture-state)
             gesture-state)))))))

(defn init-arcball
  [params]
  (let [id (keyword (:seed-id params))
        {:keys [view dist]} (-> config/seeds id :initial-view)]
    (arcball/make-arcball :init view :dist dist)))
