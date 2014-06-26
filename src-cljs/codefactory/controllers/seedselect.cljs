(ns codefactory.controllers.seedselect
  (:require-macros
   [thi.ng.macromath.core :as mm])
  (:require
   [codefactory.config :as config]
   [codefactory.color :as col]
   [codefactory.webgl :as webgl]
   [codefactory.protocols :as proto]
   [codefactory.controllers.shared :as shared]
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.app :as app :refer [handle-event emit]]
   [thi.ng.cljs.dom :as dom]
   [thi.ng.cljs.io :as io]
   [thi.ng.cljs.route :as route]
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
   [thi.ng.common.math.core :as m]
   ))

(declare instance)

(defn init-meshes
  [state]
  (let [{:keys [gl tree computed-tree selected-path meshes]} state
        root (get computed-tree selected-path)
        sub-tree (get-in tree (mg/child-path selected-path))
        ;; TODO remove old attrib buffers
        ;; TODO delete all paths in computed-tree below selected root
        branch (->> selected-path
                    (mg/compute-tree-map* root sub-tree (transient {}))
                    (persistent!))
        meshes (->> branch
                    (reduce
                     (fn [acc [path node]]
                       (if (= :leaf (mg/classify-node-at tree path))
                         (assoc!
                          acc path
                          (-> (g/into (bm/basic-mesh) (g/faces node))
                              (gl/as-webgl-buffer-spec {:tessellate true :fnormals true})
                              (buf/make-attribute-buffers-in-spec gl gl/static-draw)))
                         acc))
                     (transient meshes))
                    (persistent!))]
    (debug :mkeys (sort (keys meshes)))
    (merge
     state
     {:computed-tree (merge computed-tree branch)
      :meshes meshes})))

(defn init-state
  [state queue initial opts]
  (let [resize-window (shared/resize-window* state webgl/render-scene)
        dom-listeners [["#seed-cancel" "click" shared/cancel-module]
                       ["$window" "resize" resize-window]]]
    (reset!
     state
     (-> initial
         (merge
          {:dom-listeners dom-listeners
           :selection nil
           :sel-time 0
           :time 0})
         (webgl/init-arcball nil (fn [_] (emit queue :seed-redraw-canvas nil)))
         (init-meshes)))
    (resize-window)
    (let [{:keys [arcball canvas-width canvas-height]} @state]
      (arcball/resize arcball canvas-width canvas-height))
    (app/add-listeners dom-listeners)))


(defmethod handle-event :seed-redraw-canvas
  [_ _ _]
  (webgl/render-scene (.-state instance)))

(deftype SeedController
    [state ^:mutable shared ^:mutable queue]
  proto/PController
  (init [_ opts]
    (debug :init-seedsel)
    (set! shared (:state opts))
    (set! queue (:queue opts))
    (if-let [initial (webgl/init-webgl (dom/by-id "seed-canvas"))]
      (init-state state queue initial opts)
      (app/emit queue :webgl-missing nil)))
  (release [_]
    (debug :release-seedselect)
    (app/remove-listeners (:dom-listeners @state))
    (arcball/unlisten! (:arcball @state))
    (reset! state nil)
    (set! shared nil)))

(def instance (SeedController. (atom nil) nil nil))
