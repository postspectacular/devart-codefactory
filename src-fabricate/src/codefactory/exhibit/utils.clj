(ns codefactory.exhibit.utils
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.core.utils :as gu]
   [thi.ng.geom.core.vector :as v :refer [vec3 V3Y V3Z]]
   [thi.ng.geom.core.matrix :as mat :refer [M44]]
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
   [thi.ng.common.data.core :as d]
   [thi.ng.common.math.core :as m :refer [HALF_PI PI TWO_PI]]
   [clojure.java.io :as io]))

(defn make-wires
  "Takes a mesh and creates a new mesh of thin strips from each face's
  centroid to given point. Only faces with a z-centroid > thresh are
  considered."
  [mesh p thresh thick]
  (->> mesh
       (g/faces)
       (mapcat
        (fn [f]
          (let [c (gu/centroid f)
                n (vec3 (g/normalize (g/normal (:xy c)) thick))]
            (if (> (:z c) thresh)
              [[(g/- c n) (g/+ c n) (g/+ p n) (g/- p n)]]))))
       (g/into (gm/gmesh))))

(defn point-towards*
  [tx from to]
  (let [axis (gu/ortho-normal from to)
        theta (Math/acos (g/dot (g/normalize from) (g/normalize to)))
        q (quat/quat-from-axis-angle axis theta)]
    (g/transform tx q)))

(defn quad-normal
  [[a b c d]]
  (g/normalize (g/cross (g/- c a) (g/- d b))))

(defn point-towards2
  [tx from to right to-right]
  (let [axis (g/cross from to)
        theta (+ (Math/sqrt (* (g/mag-squared from) (g/mag-squared to))) (g/dot from to))
        q (g/normalize (quat/quat axis theta))
        r' (g/transform-vector q right)
        rt (+ m/PI (Math/acos (g/dot r' to-right)))
        qr (quat/quat-from-axis-angle to rt)]
    (g/transform tx q)))

(defn point-towards3
  [e ref from to up]
  (let [[fx fy fz :as f] from
        [sx sy sz :as s] (gu/ortho-normal up f)
        [tx ty tz :as t] (gu/ortho-normal f s)
        m (mat/matrix44
           sx sy sz (- (g/dot s ref))
           tx ty tz (- (g/dot t ref))
           fx fy fz (- (g/dot f ref))
           0.0 0.0 0.0 1.0)
        m2 (g/rotate-around-axis M44 (gu/ortho-normal V3Z to) (g/angle-between to V3Z))]
    (g/transform e (g/* m2 m))))

(defn lathe-to-point
  [r1 z1 num-seg p]
  (let [q (-> p :xy (g/normalize r1) (get :xyz z1))
        qc (assoc q :z (m/mix (q :z) (p :z) 0.25))
        pc (-> p :xy (g/* 0.5) (vec3) (assoc :z (m/mix (p :z) z1 0.25)))]
    (conj (vec (b/sample-segment [q qc pc p] num-seg)) p)))

(defn connect-strips
  ([strips]
     (connect-strips nil strips))
  ([face-fn strips]
     (let [make-face (fn [[a1 a2] [b1 b2]]
                       (let [f (cond
                                (< (count (hash-set a1 a2 b1 b2)) 3) nil
                                (= a1 b1) [b1 b2 a2]
                                (= a2 b2) [b1 a2 a1]
                                :default [b1 b2 a2 a1])]
                         (if (and f face-fn) (face-fn f) [f])))]
       (->> strips
            (d/successive-nth 2)
            (mapcat
             (fn [[sa sb]]
               (map make-face
                    (d/successive-nth 2 sa)
                    (d/successive-nth 2 sb))))))))

(defn lathe-raw
  [points res phi rot-fn & [face-fn]]
  (let [strips (mapv
                (fn [i]
                  (let [theta (* i phi)]
                    (mapv #(let [p (rot-fn % theta)]
                             (if (m/delta= p % m/*eps*)
                               % p))
                          points)))
                (butlast (m/norm-range res)))
        strips (if (m/delta= phi m/TWO_PI)
                 (conj strips (first strips))
                 strips)]
    (connect-strips face-fn strips)))

(defn torus-mesh
  [r1 r2 res-u res-v]
  (-> (c/circle (m/mix r1 r2 0.5) 0 (/ (- r1 r2) 2))
      (g/vertices res-u)
      (as-> x (mapv #(:xzy % 0) x) (conj x (first x)))
      (gm/lathe-mesh res-v TWO_PI g/rotate-z)))

(defn inset-and-extrude-quad
  [inset depth]
  #(-> (q/inset-quad % inset)
       (q/quad3)
       (g/extrude {:offset (g/* (gu/ortho-normal (% 0) (% 1) (% 2)) depth)})
       (g/faces)))

(defn make-panel-spec
  [inset]
  #(let [[a b c d :as points] (q/inset-quad % inset)
         q (q/quad3 points)
         n (quad-normal points)
         r (g/normalize (g/- (g/mix a b) (g/mix c d)))]
     [points n r]))

(defn save-meshes
  ([meshes]
     (save-meshes (format "p-%d.stl" (System/currentTimeMillis)) meshes))
  ([path meshes]
     (let [write (if (.endsWith path ".ply") mio/write-ply mio/write-stl)]
       (with-open [o (io/output-stream path)]
         (->> meshes
              (reduce g/into)
              (g/tessellate)
              (write o))))))
