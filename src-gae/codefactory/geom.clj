(ns codefactory.geom
  (:require
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
   [thi.ng.common.math.core :refer [HALF_PI]]
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

(defn generate-lux-scene
  [mesh]
  (-> (lux/lux-scene)
      (lux/configure-meshes-as-byte-arrays)
      (lux/renderer-sppm)
      (lux/sampler-sobol {})
      (lux/integrator-sppm {})
      (lux/film {:width 640 :height 480 :haltspp 200})
      (lux/camera {:eye [0 1 3] :target [0 0 0]})
      (lux/area-light :top {:p [0 3 0] :n [0 -1 0] :size 2})
      (lux/material-matte :white {:diffuse [0.8 0.8 0.8]})
      (lux/shape-disk :floor {:radius 10 :material :white :tx {:translate [0 -1 0]}})
      (lux/stl-mesh :model {:mesh mesh :material :white})
      (luxio/serialize-scene "codefactory" false)))

(defn lux->zip-bytes
  [scene]
  (with-open [out (ByteArrayOutputStream. 0x20000)]
    (luxio/export-archived-scene scene out)
    (.toByteArray out)))
