(ns codefactory.editor
  (:require
   [codefactory.config :as config]
   [codefactory.shader :as shader]
   [thi.ng.angular :as ng]
   [thi.ng.arcball :as arcball]
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
(def event-tree-editor-ready "tree-editor-ready")
(def event-op-tree-ready "op-tree-ready")

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

(defn init-shader
  [ctx preset-id]
  (let [preset (shader/presets preset-id)
        shader (sh/make-shader-from-spec ctx (:spec preset))]
    {:shader shader
     :preset-id preset-id
     :preset preset}))

(defn init-shaders
  [{:keys [ctx] :as state} preset-ids]
  (let [presets (mapv #(init-shader ctx %) preset-ids)]
    (assoc state :shaders presets)))

(defn init-camera
  [state]
  (assoc state
    :cam (arcball/make-arcball)))

(defn init-tree
  [state seed-id]
  (assoc state
    :tree mg-tree
    :computed-tree {[] (seeds seed-id)}
    :meshes {}
    :seed-id seed-id
    :selected-path []))

(defn opnode-color
  [node]
  (if (nil? node) "#999999" (get-in config/operators [(:op node) :col])))

(defn hex->rgb
  [hex]
  (let [h (js/parseInt (subs hex 1) 16)
        i (/ 255.0)]
    [(* (bit-and (bit-shift-right h 16) 0xff) i)
     (* (bit-and (bit-shift-right h 8) 0xff) i)
     (* (bit-and h 0xff) i)]))

(defn draw-tree
  [node ctx x y w]
  (let [num-children (count (:out node))
        w' (if (pos? num-children)
             (/ (- w (* (dec num-children) 5)) num-children))
        y' (- y 15)]
    (set! (.-strokeStyle ctx) (opnode-color node))
    (.strokeRect ctx x (- y 10) w 10)
    (loop [x x, c (:out node)]
      (when c
        (draw-tree (first c) ctx x y' w')
        (recur (+ (+ x w') 5) (next c))))))

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
                     (let [desktop? (.-matches (.matchMedia $window "(min-width: 480px)"))]
                       (try
                         (let [ctx (gl/gl-context canvas {:antialias desktop?})]
                           (ng/merge-state
                            state
                            (-> {:inited? true
                                 :animating? false
                                 :canvas canvas
                                 :ctx ctx
                                 :time 0}
                                (init-shaders config/shader-preset-ids)
                                (init-camera)
                                (init-tree 0)))
                           (arcball/listen! (:cam @state) nil)
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

                   :tree-editor-ready
                   (fn [evt]
                     (.stopPropagation evt)
                     (.$broadcast $scope event-op-tree-ready (:tree @state)))

                   :render-gl
                   (fn []
                     (let [{:keys [ctx cam shaders cam proj meshes time animating?]} @state
                           shader1 (shaders 0)
                           shader2 (shaders 1)
                           view (arcball/get-view cam)
                           shared-unis {:view view
                                        :model M44
                                        :proj proj
                                        :normalMat (-> (g/invert view) (g/transpose))}
                           sel [[2 0 0 0] [2 0 0 2] [2 1 1 0] [2 1 1 2] [2 2 2 0] [2 2 2 2] [2 3 3 0] [2 3 3 2]]
                           op-col (hex->rgb (get-in config/operators [:scale-side :col]))]
                       (apply gl/clear-color-buffer ctx config/canvas-bg)
                       (gl/clear-depth-buffer ctx 1.0)

                       (shader/prepare-state ctx (:state (:preset shader2)))
                       (shader/draw-meshes
                        ctx
                        (select-keys meshes sel)
                        (:shader shader2)
                        (merge (:uniforms (:preset shader2)) shared-unis
                               {:lightCol (g/mix (vec3 0.5) op-col (+ 0.5 (* 0.5 (Math/sin (* time 0.5)))))}))

                       (shader/prepare-state ctx (:state (:preset shader1)))
                       (shader/draw-meshes
                        ctx
                        (apply dissoc meshes sel)
                        (:shader shader1)
                        (merge (:uniforms (:preset shader1)) shared-unis))

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
                                            (assoc!
                                             acc path
                                             (-> (g/into (bm/basic-mesh) (g/faces node))
                                                 (gl/as-webgl-buffer-spec {:tessellate true :fnormals true})
                                                 (buf/make-attribute-buffers-in-spec ctx gl/static-draw)))
                                            acc))
                                        (transient meshes))
                                       (persistent!))]
                       (ng/merge-state state
                                       {:computed-tree (merge computed-tree branch)
                                        :meshes meshes})
                       (prn :mkeys (sort (keys meshes)))))})

          (set! (.-state $scope) state)
          (.$on $scope ng/event-canvas-ready (:init @state))
          (.$on $scope ng/event-resize-canvas (:resize @state))
          (.$on $scope event-tree-editor-ready (:tree-editor-ready @state))
          (.$on $scope "$destroy"
                (fn []
                  (arcball/unlisten! (:cam @state))
                  (ng/assoc-state state [:animating?] false)))
          ))]}

    {:id "TreeEditController"
     :spec
     #js
     ["$scope"
      (fn [$scope]
        (prn :init "TreeEditController")
        (let [state (atom nil)]
          (reset! state
                  {:init
                   (fn [evt canvas]
                     (ng/merge-state
                      state
                      {:inited? false
                       :canvas canvas
                       :ctx (.getContext canvas "2d")})
                     (.stopPropagation evt))

                   :resize
                   (fn [evt canvas]
                     (let [width (.-width canvas)
                           height (.-height canvas)]
                       (ng/merge-state state {:width width :height height})
                       (.stopPropagation evt)
                       (if (:inited? @state)
                         ((:render @state))
                         (.$emit $scope event-tree-editor-ready))))

                   :render
                   (fn []
                     (let [{:keys [tree tree-depth ctx width height]} @state]
                       (.clearRect ctx 0 0 width height)
                       (draw-tree tree ctx 15 (dec height) (- width 30))))

                   :tree-ready
                   (fn [_ tree]
                     (prn :op-tree-ready)
                     (ng/merge-state state {:inited? true :tree tree})
                     ((:render @state)))})

          (set! (.-state $scope) state)
          (.$on $scope ng/event-canvas-ready (:init @state))
          (.$on $scope ng/event-resize-canvas (:resize @state))
          (.$on $scope event-op-tree-ready (:tree-ready @state))))]}]})
