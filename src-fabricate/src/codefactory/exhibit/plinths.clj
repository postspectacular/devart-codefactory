(ns codefactory.exhibit.plinths
  (:require
   [codefactory.exhibit.utils :refer :all]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.core.utils :as gu]
   [thi.ng.geom.core.vector :as v :refer [vec3 V3Y V3Z]]
   [thi.ng.geom.core.matrix :refer [M44]]
   [thi.ng.geom.aabb :as a]
   [thi.ng.geom.bezier :as b]
   [thi.ng.geom.circle :as c]
   [thi.ng.geom.polygon :as p]
   [thi.ng.geom.quad :as q]
   [thi.ng.geom.gmesh :as gm]
   [thi.ng.geom.mesh.io :as mio]
   [thi.ng.common.data.core :as d]
   [thi.ng.common.math.core :as m :refer [HALF_PI PI TWO_PI]]
   [clojure.java.io :as io]))

(defn make-plinth-surf-points
  [s z]
  (let [points (-> (c/circle (* 0.2 s)) (g/as-polygon 6) (g/vertices))
        t1 (-> M44 (g/rotate-z (/ m/PI 6)))
        t2 (-> M44 (g/rotate-y (/ m/PI -6)))
        t3 (-> M44 (g/translate 0 0 z))]
    (mapv
     #(->> %
           (vec3)
           (g/transform-vector t1)
           (g/transform-vector t2)
           (g/transform-vector t3))
     points)))

(defn make-plinth-surface
  [z]
  (-> (c/circle 0.2) (g/as-polygon 6) (g/extrude {:depth 0.01})
      (g/transform (-> M44 (g/rotate-z (/ m/PI 6))))
      (g/transform (-> M44 (g/rotate-y (/ m/PI -6))))
      (g/transform (-> M44 (g/translate 0 0 z)))))

(defn make-plinth-canopy
  [surf-points]
  (let [strips (mapv #(lathe-to-point 0.03 0.1 6 %) surf-points)]
    (->> (conj strips (first strips))
         (connect-strips (inset-and-extrude-quad 0.002 0.003))
         (mapcat identity)
         (g/into (gm/gmesh)))))

(defn make-plinth-panels
  [surf-points]
  (let [strips (mapv #(lathe-to-point 0.05 0.1 6 %) surf-points)]
    (->> (conj strips (first strips))
         (connect-strips (make-panel-spec 0.002)))))

(def p-height-sm 0.65)
(def p-height-xl 0.8)

(def plinth-surf-sm (make-plinth-surface p-height-sm))
(def plinth-surf-xl (make-plinth-surface p-height-xl))

(def plinth-panels-sm (make-plinth-panels (make-plinth-surf-points 0.95 (- p-height-sm 0.005))))
(def plinth-panels-xl (make-plinth-panels (make-plinth-surf-points 0.95 (- p-height-xl 0.005))))

(def plinth-canopies
  (->> (map
        (fn [i z]
          (-> (make-plinth-canopy (make-plinth-surf-points 0.95 z))
              (g/transform (g/translate M44 (vec3 -1.5 (- (* i 0.75) 0.5) 0)))))
        (range -1 2) [p-height-xl p-height-sm p-height-xl])
       (reduce g/into)))

(def p-master-xl
  (let [pole (-> (a/aabb 0.05 0.05 0.78) (g/center (vec3 0 0 0.39)) (g/as-mesh))]
    (reduce g/into [pole plinth-surf-xl])))

(def p-master-sm
  (let [pole (-> (a/aabb 0.05 0.05 0.63) (g/center (vec3 0 0 0.315)) (g/as-mesh))]
    (reduce g/into [pole plinth-surf-sm])))

(def plinths
  (->> (map
        #(g/transform %2 (g/translate M44 (vec3 -1.5 (- (* % 0.75) 0.5) 0)))
        (range -1 2) [p-master-xl p-master-sm p-master-xl])
       (reduce g/into)))
