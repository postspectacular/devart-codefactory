(ns codefactory.selector
  (:require-macros
   [cljs.core.async.macros :refer [go]]
   [thi.ng.macromath.core :as mm])
  (:require
   [cljs.core.async :as cas :refer [>! <! alts! chan put! close! timeout]]
   [codefactory.config :as config]
   [codefactory.color :as col]
   [codefactory.webgl :as webgl]
   [codefactory.common :as common]
   [thi.ng.cljs.async :as async]
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.route :as route]
   [thi.ng.cljs.utils :as utils]
   [thi.ng.cljs.dom :as dom]
   [thi.ng.cljs.detect :as detect]
   [thi.ng.geom.webgl.core :as gl]
   [thi.ng.geom.webgl.animator :as anim]
   [thi.ng.geom.webgl.buffers :as buf]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.core.matrix :as mat :refer [M44]]
   [thi.ng.geom.core.vector :as v :refer [vec2 vec3]]
   [thi.ng.geom.rect :as r]
   [thi.ng.geom.basicmesh :as bm]
   [thi.ng.morphogen.core :as mg]
   [thi.ng.common.math.core :as m]))

(defn seed->index
  [seeds seed-name]
  (let [id (keyword seed-name)]
    (loop [seeds seeds, i 0]
      (if seeds
        (if (= (ffirst seeds) id)
          i (recur (next seeds) (inc i)))))))

(defn init-meshes*
  [gl seeds]
  (reduce
   (fn [acc [id {:keys [seed]}]]
     (->> {:id id
           :mesh (-> (g/into (bm/basic-mesh) (g/faces seed))
                     (gl/as-webgl-buffer-spec {:tessellate true :fnormals true})
                     (buf/make-attribute-buffers-in-spec gl gl/static-draw))}
          (conj acc)))
   [] seeds))

(defn init-meshes
  [state seeds]
  (assoc state :meshes (init-meshes* (:gl state) seeds)))

(defn mesh-spec-for-id
  [meshes id]
  (meshes (m/wrap-range id (count meshes))))

(defn render-scene
  [state]
  (if (:active? @state)
    (let [{:keys [gl shaders proj meshes selection camx start-time bg-col module-config]} @state
          {:keys [space camy camz cam-up cam-offset rot-speed scroll-speed falloff]} module-config
          [xray lambert] shaders
          now         (utils/now)
          time        (mm/subm now start-time 0.001)
          camx        (m/mix camx (* selection space) scroll-speed)
          view        (mat/look-at
                       (vec3 camx (+ camy cam-offset) camz)
                       (vec3 camx cam-offset 0)
                       cam-up)
          shared-unis {:view view :proj proj}
          num         (count meshes)
          sel         (m/wrap-range selection num)
          end-sel     (+ sel 2)]
      (apply gl/clear-color-buffer gl bg-col)
      (gl/clear-depth-buffer gl 1.0)
      (loop [i (- sel 2)
             x (mm/msub selection space (* 2 space))]
        (if (<= i end-sel)
          (let [{:keys [mesh]} (meshes (if (neg? i) (+ i num) (rem i num)))
                sel?           (== i sel)
                model-mat      (g/translate M44 x 0 0)
                model-mat      (if sel?
                                 (g/rotate-y model-mat (* time rot-speed))
                                 model-mat)
                norm-mat       (-> (g/* view model-mat) g/invert g/transpose)
                shared-unis    (assoc shared-unis :model model-mat :normalMat norm-mat)
                alpha          (falloff (Math/abs (- sel i)))]
            (if sel?
              (webgl/render-meshes gl lambert [mesh] shared-unis nil)
              (webgl/render-meshes gl xray [mesh] shared-unis {:alpha alpha}))
            (recur (inc i) (+ x space)))))
      (swap! state assoc :camx camx)
      (anim/animframe-provider (fn [& _] (render-scene state))))))

(defn resize-canvas
  [state]
  (let [{:keys [gl canvas]} @state
        [w h] (dom/size (dom/parent canvas))
        view-rect (r/rect 0 0 w h)]
    (set! (.-width canvas) w)
    (set! (.-height canvas) h)
    (swap!
     state merge
     {:canvas-width w :canvas-height h
      :view-rect view-rect
      :proj (gl/perspective 45 view-rect 0.1 10)})
    (gl/set-viewport gl view-rect)))

(defn start-editor
  [state]
  (let [{:keys [meshes selection]} @state
        spec (mesh-spec-for-id meshes selection)]
    (swap! state assoc :active? false)
    (route/set-route! "objects" "new" (name (:id spec)))))

(defn center-click?
  [c1 c2 el e radius]
  (when (= c1 c2)
    (let [p (vec2 (.-clientX e) (.-clientY e))
          c (g/* (vec2 (dom/size el)) 0.5)
          d (g/dist c p)]
      (<= d radius))))

(defn init
  [bus]
  (let [init       (async/subscribe bus :init-selector)
        release    (async/subscribe bus :release-selector)
        [left]     (async/event-channel "#seed-left" "click")
        [right]    (async/event-channel "#seed-right" "click")
        [continue] (async/event-channel "#seed-continue" "click")
        [select]   (async/event-channel "#seed-canvas" "click")
        ;;[cancel]   (async/event-channel "#seed-cancel" "click")
        canvas     (config/dom-component :seed-canvas)
        glconf     (:webgl config/app)
        mconf      (:seed-select config/app)
        seeds      (:seeds config/app)
        local      (-> (webgl/init-webgl canvas glconf)
                       (init-meshes seeds)
                       (assoc :module-config mconf
                              :bg-col (:bg-col glconf))
                       atom)
        module-timeout (config/timeout :selector)]

    ;; init
    (go
      (loop []
        (let [[_ [state params]] (<! init)
              sel (or (seed->index seeds (:seed-id params)) 0)
              resize (async/subscribe bus :window-resize)
              now (utils/now)]
          (debug :init-selector)
          (swap!
           local merge
           {:subs {:window-resize resize}
            :last-click now
            :start-time now
            :selection sel
            :camx (* sel (:space mconf))
            :active? true})
          (resize-canvas local)
          (render-scene local)

          ;; window/canvas resize
          (go
            (loop []
              (let [[_ size] (<! resize)]
                (when size
                  (resize-canvas local)
                  (recur)))))

          ;; continue/cancel buttons & user timeout
          (go
            (loop []
              (let [delay (- module-timeout (- (utils/now) (:last-click @local)))
                    [e ch] (alts! [continue select (timeout delay)])]
                (debug :timeout)
                (cond
                 (or (= continue ch) (center-click? select ch canvas e 120))
                 (start-editor local)
                 (or #_(= cancel ch)
                     (>= (- (utils/now) (:last-click @local)) module-timeout))
                 (route/set-route! "home")
                 :else (recur)))))

          (recur))))

    ;; release
    (go
      (loop []
        (let [[_ [state]] (<! release)
              {:keys [resize]} @local]
          (debug :release-selector)
          (swap! local assoc :active? false)
          (async/unsubscribe-and-close-many bus (:subs @local))
          (recur))))

    ;; selection toggles
    (go
      (loop []
        (let [[_ ch] (alts! [left right])]
          (swap!
           local merge
           {:selection ((if (= ch left) inc dec) (:selection @local))
            :last-click (utils/now)})
          (recur))))

    ))
