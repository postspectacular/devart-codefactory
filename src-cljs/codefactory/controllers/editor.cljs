(ns codefactory.controllers.editor
  (:require-macros
   [thi.ng.macromath.core :as mm])
  (:require
   [codefactory.config :as config]
   [codefactory.color :as col]
   [codefactory.shaders :as shaders]
   [codefactory.protocols :as proto]
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.app :as app]
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

(def seeds
  (->> [(a/aabb 1)
        (cub/cuboid (mg/circle-lattice-seg 6 1 0.2))
        (cub/cuboid (mg/sphere-lattice-seg 6 0.25 0.0955 0.2))]
       (mapv (comp mg/seed-box g/center))))

(def mg-trees
  {:box {:seed 0
         :tree {}
         :sel []}
   :alu {:seed 0
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
   :hex2 {:seed 1
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
               queue :get-model-success
               {:uuid id
                :seed-id (:seed data)
                :tree (:tree data)}))
   :error   (fn [status body]
              (app/emit queue :get-model-fail [status body]))))

(defn init-model
  [queue id]
  (if id
    (load-model queue id)
    (app/emit queue :editor-select-seed 0)))

(defn init-shader
  [gl preset-id]
  (let [preset (shaders/presets preset-id)
        shader (sh/make-shader-from-spec gl (:spec preset))]
    {:shader shader
     :preset-id preset-id
     :preset preset}))

(defn init-shaders
  [{:keys [gl] :as state} preset-ids]
  (assoc state
    :shaders (mapv #(init-shader gl %) preset-ids)))

(defn init-arcball
  [state]
  (let [a (arcball/make-arcball :init (or (:initial-view config/webgl) [0 0 0 1]))]
    (arcball/listen! a (:canvas state) nil)
    (assoc state :arcball a)))

(defn init-tree
  [state id]
  (let [{:keys [tree seed]} (mg-trees id)]
    (assoc state
      :tree tree
      :computed-tree {[] (seeds seed)}
      :meshes {}
      :seed-id id
      :selected-path [])))

(defn init-webgl
  [canvas]
  (try
    (let [aa? (not (dom/match-media (str "(max-width: " (:min-aa-res config/webgl) "px)")))
          gl (gl/gl-context canvas {:antialias aa?})]
      (-> {:canvas canvas :gl gl}
          (init-shaders (:shader-preset-ids config/webgl))))
    (catch js/Error e false)))

(defn render-meshes
  [gl shader meshes shared uniforms]
  (shaders/prepare-state gl (:state (:preset shader)))
  (shaders/draw-meshes
   gl meshes (:shader shader)
   (merge (:uniforms (:preset shader)) shared uniforms)))

(defn render-with-selection
  [gl shaders shared-uniforms meshes sel-meshes color time sel-time]
  (let [[s1 s2] shaders
        color (col/pulsate 0.5 color time)
        alpha (m/mix 1.0 (get-in s2 [:preset :uniforms :alpha])
                     (min (mm/subm time sel-time 0.2) 1.0))]
    (render-meshes
     gl s1 sel-meshes shared-uniforms {:lightCol color})
    (render-meshes
     gl s2 meshes shared-uniforms {:alpha alpha})))

(defn render-scene
  [state]
  (let [{:keys [gl arcball shaders proj meshes time seed-id use-selection?]} @state
        view (arcball/get-view arcball)
        shared-unis {:view view
                     :model M44
                     :proj proj
                     :normalMat (-> (g/invert view) (g/transpose))}
        sel (get-in mg-trees [seed-id :sel])
        op-col (col/hex->rgb (get-in config/operators [:scale-side :col]))]
    (apply gl/clear-color-buffer gl (:bg-col config/webgl))
    (gl/clear-depth-buffer gl 1.0)
    (if use-selection?
      (render-with-selection
       gl shaders shared-unis meshes (select-keys meshes sel) op-col time 0)
      (render-meshes
       gl (shaders 1) meshes shared-unis nil))))

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
    (merge state
           {:computed-tree (merge computed-tree branch)
            :meshes meshes})))

(defn cancel-editor
  [] (route/set-route! :home))

(defn resize-window
  []
  (let [{:keys [gl canvas]} @(.-state instance)
        [w h] (dom/size (dom/parent canvas))
        view-rect (r/rect 0 0 w h)
        state (.-state instance)]
    (set! (.-width canvas) w)
    (set! (.-height canvas) h)
    (app/merge-state
     state
     {:canvas-width w :canvas-height h
      :view-rect view-rect
      :proj (gl/perspective 45 view-rect 0.1 10)})
    (gl/set-viewport gl view-rect)
    (render-scene state)))

(def dom-listeners
  [["#edit-cancel" "click" cancel-editor]
   ["$window" "resize" resize-window]])

(defn init-state
  [state queue initial opts]
  (reset!
   state
   (-> initial
       (init-arcball)
       (init-tree :hex2)
       (update-meshes)))
  (resize-window)
  (app/add-listeners dom-listeners)
  (init-model queue (get-in opts [:params :id])))

(deftype EditorController
    [state ^:mutable shared ^:mutable queue]
  proto/PController
  (init [_ opts]
    (prn :init-editor)
    (set! shared (:state opts))
    (set! queue (:queue opts))
    (let [canvas (dom/by-id "edit-canvas")
          initial (init-webgl canvas)]
      (if state
        (init-state state queue initial opts)
        (app/emit queue :webgl-missing nil))))
  (release [_]
    (prn :release-editor)
    (app/remove-listeners dom-listeners)
    (reset! state nil)
    (set! shared nil)))

(def instance (EditorController. (atom nil) nil nil))
