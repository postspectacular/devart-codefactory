(ns codefactory.exhibit.panelgen
  (:require
   [codefactory.exhibit.utils :refer :all]
   [codefactory.exhibit.canopy :refer :all]
   [codefactory.exhibit.plinths :refer :all]
   [codefactory.exhibit.svg :as svg]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.core.utils :as gu]
   [thi.ng.geom.core.vector :as v :refer [vec3 V3X V3Y V3Z]]
   [thi.ng.geom.core.matrix :as mat :refer [M44]]
   [thi.ng.geom.aabb :as a]
   [thi.ng.geom.gmesh :as gm]
   [thi.ng.geom.basicmesh :as bm]
   [thi.ng.geom.quad :as q]
   [thi.ng.geom.types.utils :as tu]
   [thi.ng.geom.mesh.io :as mio]
   [thi.ng.morphogen.core :as mg]
   [thi.ng.common.data.core :as d]
   [thi.ng.common.math.core :as m :refer [*eps* HALF_PI PI TWO_PI]]
   [thi.ng.macromath.core :as mm]
   [clojure.java.io :as io])
  (:import
   [thi.ng.geom.core.vector Vec3]))

(defn make-pedals
  [ai al]
  (mg/subdiv-inset
   :dir :z :inset ai
   :out {4 (mg/extrude
            :dir :f :len al
            :out [(mg/subdiv
                   :slices 2
                   :out [(mg/subdiv-inset :dir :z :inset 0.0012 :out {4 nil})
                         {:op :scale-side
                          :args {:side :f :scale 0.25}
                          :out [(mg/subdiv
                                 :slices 2
                                 :out [(mg/subdiv-inset :dir :z :inset 0.0012 :out {4 nil})
                                       {}])]}])])}))

(defn make-tree
  [o1 o2 i1 i2 i3 i4 ncols nrows el leaf]
  (let [inner (fn [id i l2]
                (let [l1 (if (pos? i) (mg/subdiv-inset :dir :z :inset i :out {4 nil}) {})]
                  (mg/subdiv id 3 :out [l2 l1 l2])))
        s41 (mg/subdiv-inset
             :dir :z :inset i2
             :out {4 (inner :cols i3 (inner :rows i4 nil))})
        s42 (mg/subdiv-inset
             :dir :z :inset i2
             :out {4 (inner :rows i3 (inner :cols i4 nil))})
        ext (fn [dir1 n dir2 elen]
              (let [leaf (if (and (= :cols dir1) leaf)
                           (mg/extrude
                            :dir :e :len 0.001
                            :out [(mg/extrude
                                   :dir :w :len 0.001
                                   :out [leaf])])
                           leaf)]
                (mg/extrude
                 :dir :f :len 0.005
                 :out [(mg/subdiv
                        :slices 3
                        :out [{} {}
                              (mg/subdiv
                               dir1 n
                               :out {(int (/ n 2))
                                     (mg/extrude
                                      :dir dir2 :len elen
                                      :out [(or leaf {})])})])])))
        s3 (mg/subdiv-inset
            :dir :z :inset i1
            :out [(assoc-in s41 [:out 1] (ext :cols ncols :s el))
                  (assoc-in s41 [:out 0] (ext :cols ncols :n el))
                  (assoc-in s42 [:out 3] (ext :rows nrows :w el))
                  (assoc-in s42 [:out 2] (ext :rows nrows :e el))])]
    ;;(mg/split-displace :x :z :offset o1 :out [s3 s3])
    (mg/subdiv :cols 2 :out [s3 s3])))

(defn make-panel-seed
  [[[d h e a :as points] n] depth]
  (let [off (g/* (vec3 n) depth)
        [c g f b] (map #(g/+ off %) points)]
    (mg/seed-box (map vec3 [a b c d e f g h]))))

(defn make-panel
  [[[d h e a] n r] tree depth]
  (let [off (g/* (vec3 n) depth)
        [b c f g] (map #(g/+ off %) [a d e h])
        points [a b c d e f g h]
        seed (mg/seed-box points)
        res (mg/walk seed tree 1e6)]
    #_(doseq [i (range (count res))]
        (with-open [o (io/output-stream (format "p-%04d.stl" i))]
          (prn i)
          (mio/write-stl o (g/tessellate (g/into (bm/basic-mesh) (nth res i))))))
    #_(spit "paneltree.clj" (pr-str res))
    (mg/union-mesh (bm/basic-mesh) 1e-5 res)))

(defn make-seg-panel15
  [flat?]
  (fn [i p]
    (let [maxy 14
          i1 (m/map-interval-clamped i 6 maxy 0.0225 0.05)
          i2 0.003
          i3 0.0025
          i4 (* i3 0.5)
          o1 (if flat? 0 (m/map-interval i 0 maxy 0.025 0.01))
          el (m/map-interval-clamped i 5 maxy 0.005 0.03)
          nr (cond
              (< i 5) 11
              (< i 8) 9
              (< i 10) 7
              (< i 13) 5
              :default 3)
          ai (m/map-interval-clamped i 6 maxy 0.002 0.003)
          al (m/map-interval-clamped i 6 maxy 0 0.04 0 0.03)
          leaf (when (pos? al) (make-pedals ai al))
          t (make-tree o1 0 i1 i2 i3 i4 3 nr el leaf)]
      (make-panel p t 0.003))))

(defn make-seg-panel13
  [flat?]
  (fn [i p]
    (let [maxy 12
          i1 (m/map-interval-clamped i 5 maxy 0.0225 0.05)
          i2 (m/map-interval-clamped i 8 maxy 0.003 0.00425 0.003 0.00425) ; 0.003
          i3 0.0025
          i4 (* i3 0.8)
          o1 (if flat? 0 (m/map-interval i 0 maxy 0.025 0.01))
          el ([0.005 0.005 0.005 0.005 0.005 0.005 0.0085 0.011 0.013 0.016 0.019 0.023 0.026] i)
          nr ([11 11 11 11 9 9 7 7 5 5 5 5 3] i)
          ai (m/map-interval-clamped i 4 maxy 0.00075 0.0025)
          al (m/map-interval-clamped i 4 maxy 0.001 0.022 0 0.022)
          leaf (when (pos? al) (make-pedals ai al))
          t (make-tree o1 0 i1 i2 i3 i4 3 nr el leaf)]
      ;;(prn i :i2 i2 :el el)
      (make-panel p t 0.003))))

(defn make-seg-panel6
  [flat?]
  (fn [i p]
    (let [maxy 5
          i1 ([0.009 0.011 0.013 0.0175 0.02 0.02] i)
          i1 ([0.009 0.011 0.015 0.0175 0.022 0.025] i)
          i2 (m/map-interval-clamped i 0 maxy 0.001 0.0025)
          i3 (m/map-interval-clamped i 0 maxy 0.0012 0.002)
          i4 (* i3 0.85)
          o1 (if flat? 0 (m/map-interval i 0 maxy 0.01 0.005))
          el (m/map-interval-clamped i 2 maxy 0.005 0.015)
          nc ([3 3 3 3 3 3] i)
          nr ([11 11 9 7 7 5] i)
          ai (m/map-interval-clamped i 2 maxy 0.0005 0.001)
          al (m/map-interval-clamped i 0 maxy 0.00 0.005)
          leaf (when (pos? al) (make-pedals ai al))
          t (make-tree o1 0 i1 i2 i3 i4 nc nr el leaf)]
      (make-panel p t 0.003))))

(defn make-seg-panel6-small
  [flat?]
  (fn [i p]
    (let [maxy 5
          i1 ([0.009 0.011 0.013 0.0175 0.02 0.02] i)
          i1 ([0.009 0.011 0.015 0.0175 0.022 0.025] i)
          i2 (m/map-interval-clamped i 0 maxy 0.001 0.0025)
          i3 (m/map-interval-clamped i 0 maxy 0.0012 0.002)
          i4 (* i3 0.85)
          o1 (if flat? 0 (m/map-interval i 0 maxy 0.01 0.005))
          el (m/map-interval-clamped i 2 maxy 0.005 0.015)
          nc ([3 3 3 3 3 3] i)
          nr ([11 11 9 7 5 3] i)
          ai (m/map-interval-clamped i 2 maxy 0.0005 0.001)
          al (m/map-interval-clamped i 0 maxy 0.00 0.005)
          leaf (when (pos? al) (make-pedals ai al))
          t (make-tree o1 0 i1 i2 i3 i4 nc nr el leaf)]
      (make-panel p t 0.003))))

(defn make-segment
  [panel-fn panels]
  (binding [m/*eps* 1e-9]
    (mg/union-mesh
     (map panel-fn (range) panels))))

(defn make-rotated-segment
  [panel-fn panels res i]
  (-> (make-segment panel-fn panels)
      (g/transform (g/rotate-z M44 (* i (/ TWO_PI res))))))

(defn make-rotate-z-fn
  [^double theta]
  (let [s (Math/sin theta) c (Math/cos theta)]
    (fn [^Vec3 p]
      (Vec3. (mm/msub (.-x p) c (.-y p) s)
             (mm/madd (.-x p) s (.-y p) c)
             (.-z p)))))

(defn repeat-segments
  [segment x n]
  (let [d (/ m/TWO_PI x)
        faces (:faces segment)]
    (->> (m/norm-range x)
         (map #(* TWO_PI %))
         (drop 1)
         (take (dec n))
         (map (fn [theta]
                (prn (str (m/degrees theta) "°"))
                (let [rfn (make-rotate-z-fn theta)]
                  (g/into (bm/basic-mesh) (map (fn [f] (mapv rfn f)) faces)))))
         (reduce g/into segment)
         (vector)
         (save-meshes))))

(defn export-single
  [panels make-seg i]
  (binding [*eps* 1e-9]
    (save-meshes
     (format "pseg-%d.stl" i)
     [(make-seg i (nth panels i))])))

(defn make-segment-individual-meshes
  [panel-fn panels]
  (binding [m/*eps* 1e-9]
    (mapv
     (fn [i [p n r :as panel]]
       (let [pmesh (panel-fn i panel)
             pmesh (g/transform pmesh (mat/look-at n (vec3) (gu/ortho-normal n r)))
             pmesh (g/center pmesh)]
         (g/transform pmesh (g/scale M44 1000.0))))
     (range) panels)))

(defn export-panels
  [pmeshes]
  (dorun
   (map-indexed
    (fn [i pm]
      (with-open [o (io/output-stream (format "p%03d.stl" i))]
        (mio/write-stl o pm)))
    pmeshes)))

(defn select-mesh-slice
  [zrange m]
  (tu/map-mesh
   (fn [f]
     (let [c (:z (gu/centroid f))
           d (g/dot V3Z (gu/ortho-normal f))]
       (if (and (m/in-range? zrange c) (m/delta= 1.0 d 0.1))
         [f])))
   m))

(defn mesh->svg
  [m]
  (->> (g/center (g/transform m (g/scale M44 1000)) (vec3 150 150 0))
       (g/faces)
       (map #(svg/path {:fill "black"} %))
       (svg/svg {:width "300mm" :height "300mm" :viewBox "0 0 300 300"})
       (svg/->xml)))

(defn mesh->svg-paths
  [pos m]
  (->> (g/center (g/transform m (g/scale M44 1000)) pos)
       (g/faces)
       (map #(svg/path {:fill "black"} %))
       (vector :g {})))

(defn export-mesh-slices-svg
  [path width m]
  (let [w (* 3 width)]
    (->> {1 [0 0.0048] 2 [0.005 0.007] 3 [0.0075 0.0095]}
         (map
          (fn [[i zr]]
            (->> m
                 (select-mesh-slice zr)
                 (mesh->svg-paths (vec3 (* (- i 0.5) width) 150 0)))))
         (svg/svg {:width (str w "mm") :height "300mm" :viewBox (str "0 0 " w " 300")})
         (svg/->xml)
         (spit path))))

(defn export-segment-slices-svg
  [seg-id width meshes]
  (dorun
   (map-indexed
    (fn [i m]
      (export-mesh-slices-svg (str "seg-" seg-id "-" i ".svg") width m))
    meshes)))

(defn export-all-panels-svg
  [panel-fn stride panels]
  (->> panels
       (partition stride)
       (map-indexed
        (fn [i panels]
          (->> panels
               (make-segment-individual-meshes panel-fn)
               (export-segment-slices-svg i 200))))
       (dorun)))

(comment

  ;; export combined STL of all panels of small plinth
  (->> plinth-panels-sm
       (partition 6)
       (map #(make-segment (make-seg-panel6-small true) %))
       (save-meshes "plinth-panels-sm-master2.stl"))

  (->> plinth-panels-sm
       (partition 6)
       (map #(make-segment-individual-meshes (make-seg-panel6-small true) %))
       (map-indexed
        (fn [i seg-panels]
          (map-indexed
           #(save-meshes (format "plinth-sm-flat-%02d-%02d.stl" i %) [%2])
           seg-panels))))

  ;; export combined STL of all panels of large plinth
  (->> plinth-panels-xl
       (partition 6)
       (map #(make-segment (make-seg-panel6 true) %))
       (save-meshes "plinth-panels-xl-master-new2.stl"))

  (->> plinth-panels-xl
       (partition 6)
       (map #(make-segment-individual-meshes (make-seg-panel6 true) %))
       (map-indexed
        (fn [i seg-panels]
          (map-indexed
           #(save-meshes (format "plinth-xl-master-%02d-%02d.stl" i %) [%2])
           seg-panels))))

  ;; export individual SVG z-slices of all panels of small plinth
  (export-all-panels-svg (make-seg-panel6 true) 6 plinth-panels-sm)

  ;; export individual SVG z-slices of all panels of large plinth
  (export-all-panels-svg (make-seg-panel6 true) 6 plinth-panels-xl)

  ;; export STL of a single vertical segment of main canopy
  (->> canopy-panels
       (take canopy-segments)
       (make-segment (make-seg-panel13 true))
       ;;((fn [x] (repeat-segments x 26 13)))
       ;;(vector)
       (save-meshes "canopy-seg.stl")
       )

  ;; export STLs of invidual panels of a single vertical segment
  (->> canopy-panels
       (take canopy-segments)
       (make-segment-individual-meshes (make-seg-panel13 true))
       (map-indexed #(save-meshes (format "canopy-master-%02d.stl" %) [%2])))

  (binding [m/*eps* 1e-9]
    (->> canopy-panels
         (take canopy-segments)
         (last)
         ((fn [p] ((make-seg-panel13 true) 12 p)))
         (vector)
         (save-meshes)
         ;;((fn [x] (repeat-segments x 26 26)))
         ))
  )
