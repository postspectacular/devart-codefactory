(ns codefactory.geom
  (:require
   [codefactory.geom.viewfinder :as vf]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.core.utils :as gu]
   [thi.ng.geom.types.utils :as tu]
   [thi.ng.geom.core.vector :refer [vec2 vec3 V3X]]
   [thi.ng.geom.core.matrix :as mat :refer [M44 M32]]
   [thi.ng.geom.aabb :as a]
   [thi.ng.geom.cuboid :as cub]
   [thi.ng.geom.mesh.io :as mio]
   [thi.ng.luxor.core :as lux]
   [thi.ng.luxor.io :as luxio]
   [thi.ng.morphogen.core :as mg]
   [thi.ng.common.math.core :as m :refer [HALF_PI SIXTH_PI]]
   [clojure.java.io :as io])
  (:import
   [java.io ByteArrayOutputStream]))

(def norm-box (a/aabb (vec3 -0.5) 1))

(defn normalize-mesh
  [mesh]
  (first (tu/fit-all-into-bounds norm-box [mesh])))

(def seeds
  (let [c     #(cub/cuboid (apply mg/circle-lattice-seg %))
        s     #(g/rotate-z (cub/cuboid (apply mg/sphere-lat %)) (- HALF_PI))
        seeds {:box   {:seed (a/aabb 1)}
               :pent3 {:seed (s [5 5 0.25])}
               :hex3  {:seed (s [6 12 0.25])}
               :oct3  {:seed (s [8 8 0.25])}
               :pent2 {:seed (c [5 1 0.5])}
               :hex2  {:seed (c [6 1 0.5])}
               :oct2  {:seed (c [8 1 0.5])}
               :tri2  {:seed (c [3 1 0.4])}}]
    (reduce-kv
     (fn [acc k _]
       (update-in acc [k :seed] #(-> % normalize-mesh mg/seed-box)))
     seeds seeds)))

(defn generate-mesh
  [tree seed-id]
  (prn :generate-mesh tree seed-id)
  (if-let [seed (get-in seeds [(keyword seed-id) :seed])]
    (-> seed
        (mg/walk tree)
        (mg/union-mesh)
        (normalize-mesh)
        (g/tessellate))
    (prn :error "invalid seed id" seed-id)))

(defn mesh->stl-bytes
  [mesh]
  (with-open [out (ByteArrayOutputStream. 0x10000)]
    (mio/write-stl out mesh)
    (.toByteArray out)))

(def infinity-curve
  (delay
   (try
     (let [res (io/resource "infinity-curve-2.stl")]
       (prn :resource res)
       (with-open [in (io/input-stream res)]
         (let [mesh (mio/read-stl in)]
           (prn :infinity (count (g/faces mesh)))
           mesh)))
     (catch Exception e
       (prn :warn-infinity (.getMessage e))))))

(defn generate-lux-scene
  [mesh {:keys [width height fov margin halt-spp]}]
  (let [aspect        (double (/ width height))
        proj          (mat/perspective fov aspect 0.1 10)
        model         (vf/optimize-rotation mesh)
        vtx           (vf/viewport-transform 1 1)
        compute-view  (vf/compute-view* mesh model proj vtx margin)
        [ex ey ez ty] (vf/optimize-view compute-view [0 0.85 2 0] 2)
        eye           (vec3 ex ey ez)
        target        (vec3 ex ty 0)
        up            (gu/ortho-normal (g/- eye target) V3X)]
    (prn :cam eye :-> target)
    (-> (lux/lux-scene)
        (lux/configure-meshes-as-byte-arrays)
        (lux/renderer-sppm)
        (lux/sampler-sobol {})
        (lux/integrator-sppm {})
        (lux/filter-mitchell {})
        (lux/film
         {:width width :height height :halt-spp halt-spp :display-interval 6})
        (lux/tonemap-linear
         {:iso 200 :exposure 0.75 :f-stop 5.6 :gamma 2.2})
        (lux/camera
         {:eye eye :target target :up up :fov fov
          :focal-point (g/mix eye target 0.8) :lens-radius 0.035})
        (lux/light-groups
         {:top {:gain 1} :sides {:gain 0.125}})
        (lux/area-light
         :top {:p [0 3 0] :n [0 -1 0] :size 2 :group :top})
        (lux/area-light
         :left {:p [-3 0.5 0] :n [1 -0.1 0] :size 0.5 :group :sides})
        (lux/area-light
         :right {:p [3 0.5 0] :n [-1 -0.1 0] :size 0.5 :group :sides})
        (lux/material-matte
         :dark {:diffuse [0.01 0.01 0.0105]})
        ;;(lux/material-mirror :mirror {})
        ;;(lux/material-mix :dark-mirror :dark :mirror 0.2)
        (lux/material-matte
         :white {:diffuse [0.8 0.8 0.8]})
        (lux/stl-mesh
         :bg {:mesh @infinity-curve :tx {:rx -90} :material :dark})
        (lux/stl-mesh
         :model {:mesh mesh :material :white :tx {:matrix model}})
        (luxio/serialize-scene "codefactory" false))))

(defn lux->zip-bytes
  [scene]
  (with-open [out (ByteArrayOutputStream. 0x20000)]
    (luxio/export-archived-scene scene out)
    (.toByteArray out)))
