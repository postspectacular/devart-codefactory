(ns codefactory.exhibit.core
  "The code in this ns creates a 3d mockup scene of the exhibition space
  at the Barbican. Exports STL meshes of all items and he resulting LXS
  file can be rendered straight away with Luxrender."
  (:require
   [codefactory.exhibit.utils :refer :all]
   [codefactory.exhibit.canopy :refer :all]
   [codefactory.exhibit.plinths :refer :all]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.core.utils :as gu]
   [thi.ng.geom.core.vector :as v :refer [vec3 V3Y V3Z]]
   [thi.ng.geom.core.matrix :refer [M44]]
   [thi.ng.geom.core.quaternion :as quat]
   [thi.ng.geom.aabb :as a]
   [thi.ng.geom.bezier :as b]
   [thi.ng.geom.circle :as c]
   [thi.ng.geom.polygon :as p]
   [thi.ng.geom.plane :as pl]
   [thi.ng.geom.quad :as q]
   [thi.ng.geom.rect :as r]
   [thi.ng.geom.gmesh :as gm]
   [thi.ng.geom.mesh.io :as mio]
   [thi.ng.geom.mesh.csg :as csg]
   [thi.ng.luxor.core :refer :all]
   [thi.ng.luxor.io :as lio]
   [thi.ng.common.data.core :as d]
   [thi.ng.common.math.core :as m :refer [HALF_PI PI TWO_PI]]
   [clojure.java.io :as io]))

;; 6000x4000mm
(def floor
  (-> (pl/plane V3Z 0) (g/as-mesh {:width 6.0 :height 4.0})))

;; circular truss, 1200mm center radius, 40mm round cross section
(def truss
  (-> (torus-mesh 1.175 1.225 12 40)
      (g/transform (g/translate M44 [0 -0.5 3.025]))))

;; cross bar for truss support, 2400mm length, 50mm square tube
(def crossbar
  (let [b (-> (a/aabb [2.4 0.05 0.05]) (g/center (vec3 {:z 3.025})) (g/as-mesh))]
    (-> (->> (-> M44 (g/rotate-z HALF_PI))
             (g/transform b)
             (g/into b))
        (g/transform (g/translate M44 0 -0.5 0)))))

(def printer-plinth
  (-> (a/aabb 0.65 0.65 0.3) (g/center (vec3 0 -0.5 0.15)) (g/as-mesh)))

;; Ilios 3d printer
;; uses boolean operations to carve out frame from bounding boxes
(def printer
  (let [p   (-> (a/aabb [0.6 0.6 1.2]) (g/center (vec3 0 0 0.6)) (g/as-mesh))
        p1  (-> (a/aabb [0.5 0.5 1.2]) (g/center (vec3 0 0 0.55)) (g/as-mesh))
        p2  (-> (a/aabb [0.5 0.7 0.5]) (g/center (vec3 0 0 0.3)) (g/as-mesh))
        p2* (g/transform p2 (g/rotate-z M44 HALF_PI))
        p3  (-> (a/aabb [0.7 0.5 0.7]) (g/center (vec3 0 0.3 0.95)) (g/as-mesh))
        p3* (g/transform p3 (g/translate M44 0 -0.6 0))]
    (-> (->> [p p1 p2 p2* p3 p3*]
             (map csg/mesh->csg)
             (reduce csg/subtract)
             (csg/csg->mesh))
        (g/transform (g/translate M44 0 -0.5 0.3)))))

(def projector
  (-> (a/aabb [0.15 0.4 0.3]) (g/center (vec3 {:y -0.5 :z 0.6}))
      (g/as-mesh)))

(def tablets
  (->> (map
        (fn [i z]
          (-> (a/aabb (vec3 -0.13 0 0) (vec3 0.26 0.19 0.01))
              (g/as-mesh)
              (g/transform (-> M44
                               (g/rotate-z HALF_PI)
                               (g/rotate-x (/ m/PI -6))))
              (g/center (vec3 -1.5 (- (* i 0.75) 0.5) (+ z 0.01)))))
        (range -1 2) [p-height-xl p-height-sm p-height-xl])
       (reduce g/into)))

(def shelves
  (let [wall (-> (a/aabb (vec3 -3 1.7 0) (vec3 6 0.5 6)) (g/as-mesh) (csg/mesh->csg))
        wall (->> (for [i (range 10)
                        x (range -3 4)]
                    (-> (a/aabb [0.3 0.3 0.3])
                        (g/center (vec3 (* x 0.325) 1.85 (+ 0.9625 (* i 0.325))))
                        (g/as-mesh)
                        (csg/mesh->csg)))
                  (reduce #(csg/subtract % %2) wall)
                  (csg/csg->mesh))]
    wall))

(def lcds
  (->> (for [x (range -3 4)]
         (-> (pl/plane (g/- V3Y) 1.699)
             (g/as-mesh {:p (vec3 (* x 0.325) 0 0.78) :width 0.1 :height 0.03})))
       (reduce g/into)))

(def scene-model
  (->> [truss crossbar
        printer projector printer-plinth
        floor
        canopy
        plinths tablets plinth-canopies
        shelves lcds]
       (reduce g/into)
       (g/tessellate)))

(with-open [o (io/output-stream "exhibit.stl")] (mio/write-stl o scene-model))

;;;;; LXS

(defn save-lxs
  []
  (-> (lux-scene)
      (renderer-sampler)
      (sampler-ld {})
      (integrator-bidir {})
      (camera {:eye [-2.5 -4 1.8] :target [0 0.5 1.5] :up [0 0 1] :fov 50})
      (film
       {;; :width 1280 :height 720
        :width 640 :height 360
        :response :agfachrome-rsx2-200cd
        :display-interval 5 :halt-spp 1000})
      (tonemap-linear
       {:iso 100 :exposure 0.5 :f-stop 8 :gamma 2.2})
      (volume
       :glass {:type :clear :absorb [1.0 0.905 0.152] :abs-depth 0.01 :ior 1.488})
      (material-matte
       :white {:diffuse [0.8 0.8 0.8]})
      (material-matte
       :black {:diffuse [0.1 0.1 0.1]})
      (material-matte
       :blue {:diffuse [0.025 0.02 0.2]})
      (material-matte
       :yellow__ {:diffuse [0.8 0.8 0.5] :alpha 0.2})
      (material-glass2
       :yellow {:interior :glass})
      (area-light
       :lcd {:mesh lcds :mesh-type :stl :color [1.0 0.13 0.08] :gain 0.1 :power 1})
      (area-light
       :tablets {:mesh tablets :mesh-type :stl :color [1 1 1] :gain 1 :power 1})
      (spot-light
       :spot {:from [0 0 0] :to [0 0 -1] :cone-angle 45 :cone-delta 5
              :tx {:translate [0 -0.5 0.46] :rx 180}})
      (stl-mesh
       :metal {:mesh (reduce g/into [truss crossbar plinths])
               :material :black})
      (stl-mesh
       :black {:mesh floor :material :white})
      (stl-mesh
       :walls {:mesh shelves :material :blue})
      (stl-mesh
       :canopy {:mesh (reduce g/into [canopy]) :material :yellow})
      (stl-mesh
       :ilios {:mesh (reduce g/into [printer projector]) :material :white})
      (lio/serialize-scene "exhibit" false)
      (lio/export-scene)))

(save-lxs)
