(ns codefactory.controllers.editor
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
   [thi.ng.geom.webgl.shaders :as sh]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.core.matrix :as mat :refer [M44]]
   [thi.ng.geom.core.quaternion :as quat]
   [thi.ng.geom.core.vector :as v :refer [vec2 vec3]]
   [thi.ng.geom.aabb :as a]
   [thi.ng.geom.cuboid :as cub]
   [thi.ng.geom.rect :as r]
   [thi.ng.geom.basicmesh :as bm]
   [thi.ng.geom.ui.arcball :as arcball]
   [thi.ng.morphogen.core :as mg]
   [thi.ng.common.math.core :as m]
   ))

(def mg-trees
  {:box {:seed :box
         :tree {}
         :sel []}
   :alu {:seed :box
         :tree (let [branch (fn [[dir lpos]]
                              (mg/subdiv-inset
                               :dir :y :inset 0.05
                               :out {lpos (mg/subdiv dir 3 :out {1 nil}) 4 nil}))
                     module (mg/subdiv-inset
                             :dir :y :inset 0.4
                             :out (mapv branch [[:cols 0] [:cols 1] [:slices 2] [:slices 3]]))]
                 (mg/subdiv
                  :rows 3
                  :out [module
                        (mg/subdiv
                         :rows 3 :out {1 (mg/subdiv :cols 3 :out [nil {} nil])})
                        module]))
         :sel [[2 0 0 0] [2 0 0 2] [2 1 1 0] [2 1 1 2] [2 2 2 0] [2 2 2 2] [2 3 3 0] [2 3 3 2]]}
   :hex2 {:seed :hex2
          :tree (let [hex (mg/apply-recursively (mg/reflect :dir :e) 5 [1] 1)
                      reflected-hex (mg/reflect :dir :n :out [{} hex])
                      inject #(-> hex
                                  (assoc-in (mg/child-path [1 1 0]) %)
                                  (assoc-in (mg/child-path [1 1 1 1 0]) %))
                      seed-clone (mg/reflect :dir :s :out [{} (inject reflected-hex)])]
                  (mg/reflect :dir :s :out [(inject seed-clone) (inject reflected-hex)]))
          :sel [[1 1 1 1 0]]}})

(declare instance)

(defn load-model
  [queue id]
  (app/emit queue :editor-show-loader nil)
  (io/request
   :uri (str "/api/models/" id)
   :method :get
   :edn? true
   :success (fn [_ data]
              (app/emit
               queue :editor-get-model-success
               {:uuid id
                :seed-id (:seed data)
                :tree (:tree data)}))
   :error   (fn [status body]
              (app/emit queue :editor-get-model-fail [status body]))))

(defn init-model
  [queue id]
  (if id
    (load-model queue id)
    (app/emit queue :editor-select-seed 0)))

(defn init-tree
  [state id]
  (let [{:keys [tree seed]} (mg-trees id)]
    (assoc state
      :tree tree
      :computed-tree {[] (:seed (config/seeds seed))}
      :meshes {}
      :seed-id seed
      :selected-path [])))

(defn update-meshes
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

(defmethod handle-event :editor-redraw-canvas
  [_ _ _]
  (webgl/render-scene (.-state instance)))

(defn init-state
  [state queue initial opts]
  (let [resize-window (shared/resize-window* state initial webgl/render-scene)
        dom-listeners [["#edit-cancel" "click" (shared/cancel-module "select-seed")]
                       ["$window" "resize" resize-window]]]
    (reset!
     state
     (-> initial
         (merge
          {:dom-listeners dom-listeners
           :selection (get-in mg-trees [:alu :sel])
           :sel-time 0
           :time 0})
         (webgl/init-arcball nil (fn [_] (emit queue :editor-redraw-canvas nil)))
         (init-tree :alu)
         (update-meshes)))
    (resize-window)
    (let [{:keys [arcball canvas-width canvas-height]} @state]
      (arcball/resize arcball canvas-width canvas-height))
    (app/add-listeners dom-listeners)
    (init-model queue (get-in opts [:params :id]))))

(deftype EditorController
    [state ^:mutable shared ^:mutable queue]
  proto/PController
  (init [_ opts]
    (debug :init-editor)
    (set! shared (:state opts))
    (set! queue (:queue opts))
    (if-let [initial (webgl/init-webgl (dom/by-id "edit-canvas"))]
      (init-state state queue initial opts)
      (app/emit queue :webgl-missing nil)))
  (release [_]
    (debug :release-editor)
    (app/remove-listeners (:dom-listeners @state))
    (arcball/unlisten! (:arcball @state))
    (reset! state nil)
    (set! shared nil)))

(def instance (EditorController. (atom nil) nil nil))
