(ns codefactory.editor
  (:require
   [codefactory.config :as config]
   [codefactory.shader :as shader]
   [thi.ng.angular :as ng]
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
   [thi.ng.morphogen.core :as mg]
   ))

(def controller-id "EditObjectController")

(def event-render-gl "render-gl")

(def seeds
  (->> [(a/aabb 1)
        (cub/cuboid (mg/circle-lattice-seg 6 1 0.2))]
       (mapv (comp mg/seed-box g/center))))

(def mg-tree
  (let [branch (fn [[dir lpos]]
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
           module])))

(def module-spec
  {:controllers
   [{:id controller-id
     :spec
     #js
     ["$scope" "$routeParams" "$window"
      (fn [$scope $routeParams $window]
        (prn :init "EditObjectController" $routeParams)
        (let [state (atom {})]
          (reset! state
                  {:init
                   (fn [evt canvas]
                     (let [desktop? (.-matches (.matchMedia $window "(min-width: 800px)"))]
                       (try
                         (let [ctx (gl/gl-context canvas {:antialias desktop?})
                               shader (sh/make-shader-from-spec ctx shader/lambert-shader-spec)]
                           (ng/merge-state
                            state
                            {:inited? true
                             :canvas canvas
                             :ctx ctx
                             :shader shader
                             :cam (mat/look-at (vec3 0 1 2) (vec3) (vec3 0 1 0))
                             :tree mg-tree
                             :computed-tree {[] (seeds 0)}
                             :meshes {}
                             :seed-id 0
                             :selected-path []
                             :time 0
                             :animating? false})
                           ((:update-meshes @state)))
                         (catch js/Error e
                           (js/alert (str "WebGL not supported: " e)))))
                     (.stopPropagation evt))

                   :resize
                   (fn [evt canvas]
                     (let [{:keys [inited? ctx render-gl animating?]} @state]
                       (when inited?
                         (let [view-rect (r/rect 0 0 (.-width canvas) (.-height canvas))]
                           (ng/merge-state state
                                           {:view-rect view-rect
                                            :proj (gl/perspective 45 view-rect 0.1 10)})
                           (gl/set-viewport ctx view-rect)
                           (when-not animating?
                             (ng/assoc-state state [:animating?] true)
                             (render-gl))))))

                   :render-gl
                   (fn []
                     (let [{:keys [ctx shader cam proj meshes time animating?]} @state]
                       (apply gl/clear-color-buffer ctx config/canvas-bg)
                       (gl/clear-depth-buffer ctx 1.0)
                       (gl/enable ctx gl/depth-test)
                       (shader/draw-meshes
                        ctx (vals meshes) shader
                        {:view (g/rotate-y cam (* time 0.1))
                         :model M44
                         :proj proj
                         :normalMat (-> (g/invert cam) (g/transpose))
                         :ambientCol [0.3 0.3 0.3]
                         :lightCol [0.8 0.8 0.8]
                         :lightDir (g/normalize (vec3 0 1 1))
                         :alpha 1.0})
                       (when animating?
                         (ng/update-state state [:time] #(+ % 0.16666))
                         (anim/animframe-provider (:render-gl @state)))))

                   :update-meshes
                   (fn []
                     (let [{:keys [ctx tree computed-tree selected-path meshes]} @state
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
                                            (assoc! acc path (-> (g/into (bm/basic-mesh) (g/faces node))
                                                                 (gl/as-webgl-buffer-spec {:tessellate true :fnormals true})
                                                                 (buf/make-attribute-buffers-in-spec ctx gl/static-draw)))
                                            acc))
                                        (transient meshes))
                                       (persistent!))]
                       (ng/merge-state state
                                       {:computed-tree (merge computed-tree branch)
                                        :meshes meshes})))})

          (set! (.-state $scope) state)
          (.$on $scope ng/event-canvas-ready (:init @state))
          (.$on $scope ng/event-resize-canvas (:resize @state))))]}]})
