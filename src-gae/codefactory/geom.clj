(ns codefactory.geom
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.types.utils :as tu]
   [thi.ng.geom.core.vector :refer [vec3 V3Y V3Z]]
   [thi.ng.geom.aabb :as a]
   [thi.ng.geom.cuboid :as cub]
   [thi.ng.geom.mesh.io :as mio]
   [thi.ng.luxor.core :as lux]
   [thi.ng.luxor.io :as luxio]
   [thi.ng.morphogen.core :as mg]
   [thi.ng.common.math.core :refer [HALF_PI]])
  (:import
   [java.io ByteArrayOutputStream]))

(def seeds
  (->>
   {:box   {:seed (a/aabb 1)}
    :pent3 {:seed (g/rotate-z (cub/cuboid (mg/sphere-lat 5 5 0.25)) (- HALF_PI))}
    :hex3  {:seed (g/rotate-z (cub/cuboid (mg/sphere-lat 6 12 0.25)) (- HALF_PI))}
    :oct3  {:seed (g/rotate-z (cub/cuboid (mg/sphere-lat 8 8 0.25)) (- HALF_PI))}
    :pent2 {:seed (cub/cuboid (mg/circle-lattice-seg 5 1 0.5))}
    :hex2  {:seed (cub/cuboid (mg/circle-lattice-seg 6 1 0.5))}
    :oct2  {:seed (cub/cuboid (mg/circle-lattice-seg 8 1 0.5))}
    :tri2  {:seed (cub/cuboid (mg/circle-lattice-seg 3 1 0.4))}}
   (reduce-kv
    (fn [acc k v]
      (assoc
          acc k
          (update-in
           v [:seed]
           #(->> [%]
                 (tu/fit-all-into-bounds (a/aabb 1))
                 (first)
                 (g/center)
                 (mg/seed-box)))))
    {})))

(defn normalize-mesh
  [mesh]
  (first (tu/fit-all-into-bounds (a/aabb 1) [mesh])))

(defn generate-mesh
  [tree seed-id]
  (prn :generate-mesh tree seed-id)
  (if-let [seed (get-in seeds [(keyword seed-id) :seed])]
    (-> seed
        (mg/walk tree)
        (mg/union-mesh)
        (normalize-mesh)
        (g/center)
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
