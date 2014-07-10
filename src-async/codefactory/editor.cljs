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
   [thi.ng.geom.basicmesh :as bm]
   [thi.ng.geom.ui.arcball :as arcball]
   [thi.ng.morphogen.core :as mg]
   [thi.ng.common.math.core :as m]))

;; FIXME pass model/scene to next controller
(defn submit-model
  [state] (route/set-route! "objects" "submit"))

(defn load-model
  [bus id]
  (io/request
   :uri     (str (:get-object config/api-routes) id)
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
  [state]
  (if (:active? @state)
    (let [{:keys [gl arcball shaders proj meshes selection sel-type start-time sel-time]} @state
          now         (utils/now)
          time        (mm/subm now start-time 0.001)
          ;;view        (mat/look-at (vec3 0 1 -4) (vec3) (vec3 0 1 0))
          view        (arcball/get-view arcball)
          shared-unis {:view view
                       :model M44
                       :proj proj
                       :normalMat (-> (g/invert view) (g/transpose))}]
      (apply gl/clear-color-buffer gl (:bg-col config/webgl))
      (gl/clear-depth-buffer gl 1.0)
      (if selection
        (webgl/render-with-selection
         gl shaders shared-unis
         (vals (dissoc meshes selection))
         (vals (select-keys meshes [selection]))
         (col/hex->rgb (config/operator-color sel-type))
         time sel-time)
        (webgl/render-meshes gl (shaders 1) (vals meshes) shared-unis nil)))))

(defn resize-canvas
  [state]
  (let [{:keys [gl canvas arcball]} @state
        [w h] (dom/size (dom/parent canvas))
        view-rect (r/rect 0 0 w h)]
    (set! (.-width canvas) w)
    (set! (.-height canvas) h)
    (swap!
     state merge
     {:canvas-width w :canvas-height h
      :view-rect view-rect
      :proj (gl/perspective 45 view-rect 0.1 10)})
    (gl/set-viewport gl view-rect)
    (arcball/resize arcball w h)))

(defn init
  [bus]
  (let [canvas     (dom/by-id "edit-canvas")
        init       (async/subscribe bus :init-editor)
        release    (async/subscribe bus :release-editor)
        render     (async/subscribe bus :render-scene)
        [continue] (dom/event-channel "#edit-submit" "click")
        [cancel]   (dom/event-channel "#edit-cancel" "click")
        local      (atom {})
        module-timeout (:editor config/timeouts)]

    (go
      (loop []
        (let [[_ [state params]] (<! init)
              resize  (async/subscribe bus :window-resize)
              mdown   (dom/event-channel js/window "mousedown")
              mup     (dom/event-channel js/window "mouseup")
              mmove   (dom/event-channel js/window "mousemove")
              mwheel  (dom/event-channel js/window (dom/wheel-event-type))
              inputs  (mapv first [mdown mmove mup mwheel])
              arcball (arcball/make-arcball :init (:initial-view config/editor))
              now     (utils/now)]
          (debug :init-editor params)
          (reset!
           local
           (-> (webgl/init-webgl (dom/by-id "edit-canvas"))
               (merge
                {:subs {:window-resize resize}
                 :events [mdown mup mmove mwheel]
                 :arcball arcball
                 :last-click now
                 :start-time now
                 :selection nil
                 :sel-time now
                 :time now
                 :active? true})
               (tree/init-tree-with-seed (:seed-id params))
               (tree/update-meshes)))
          (tedit/init local bus)
          (resize-canvas local)
          (render-scene local)

          ;; window/canvas resize
          (go
            (loop []
              (let [[_ size] (<! resize)]
                (when size
                  (resize-canvas local)
                  (render-scene local)
                  (recur)))))

          (go
            (loop []
              (let [[e ch] (alts! inputs)]
                (when ch
                  (condp = ch
                    (inputs 0) (let [x (.-clientX e)
                                     y (.-clientY e)
                                     h (.-clientHeight canvas)]
                                 (when (m/in-range? 0 h y)
                                   (arcball/down arcball x (- h y))))

                    (inputs 1) (let [x (.-clientX e)
                                     y (.-clientY e)
                                     h (.-clientHeight canvas)]
                                 (when (and (m/in-range? 0 h y)
                                            (arcball/drag arcball x (- h y)))
                                   (async/publish bus :render-scene nil)))
                    
                    (inputs 2) (arcball/up arcball)

                    (inputs 3) (let [delta (or (aget e "deltaY") (aget e "wheelDataY"))]
                                 (arcball/zoom-delta arcball delta)
                                 (async/publish bus :render-scene nil))
                    
                    (debug :ev e))
                  (recur)))))

          ;; continue/cancel buttons & user timeout
          (go
            (loop []
              (let [delay (- module-timeout (- (utils/now) (:last-click @local)))
                    [_ ch] (alts! [continue cancel (timeout delay)])]
                (cond
                 (= continue ch)
                 (submit-model local)

                 (= cancel ch)
                 (route/set-route! "select" (:seed-id @local))
                 ;;(route/set-route! "home")

                 (>= (- (utils/now) (:last-click @local)) module-timeout)
                 (route/set-route! "home")

                 :else (recur)))))

          (recur))))

    (go
      (let [render-fn (fn [& _] (render-scene local))]        
        (loop []
          (let [_ (<! render)]
            (anim/animframe-provider render-fn)
            (recur)))))

    (go
      (loop []
        (let [[_ [state]] (<! release)
              {:keys [resize]} @local]
          (debug :release-editor)
          (swap! local assoc :active? false)
          (async/unsubscribe-and-close-many bus (:subs @local))
          (dorun (map dom/destroy-event-channel (:events @local)))
          (recur))))

    ))
