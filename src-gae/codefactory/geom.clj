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

(comment
  (let [h 0.8
        d 0.584
        eye (g/* (vec3 -0.05 0.85 2) 1 h d)
        target (vec3 0)
        up (gu/ortho-normal (g/- eye target) V3X)]
    (format "LookAt %1.5f %1.5f %1.5f  %1.5f %1.5f %1.5f  %1.5f %1.5f %1.5f"
            (:x eye) (:y eye) (:z eye)
            (:x target) (:y target) (:z target)
            (:x up) (:y up) (:z up))))

(require
 '[thi.ng.geom.core :as g]
 '[thi.ng.geom.core.utils :as gu]
 '[thi.ng.geom.types.utils :as tu]
 '[thi.ng.geom.core.vector :refer [vec2 vec3 V3X]]
 '[thi.ng.geom.core.matrix :as mat :refer [M44 M32]]
 '[thi.ng.common.math.core :as m])

(defn project
  [pp m v p vp]
  (let [p' (g/transform-vector m pp)
        [x y _ w] (g/transform-vector (g/* p v) (conj p' 1))]
    ;;(prn x y w)
    (g/transform-vector vp (g/div (vec2 x y) w))))

(defn make-view
  [width height]
  (let [w2 (/ width 2.0) h2 (/ height 2.0)]
    (-> mat/M32
        (g/translate w2 h2)
        (g/* (g/scale mat/M32 w2 (- h2))))))

(defn make-cam
  [ex ey ez ty]
  (let [e (vec3 ex ey ez)
        t (vec3 ex ty 0)]
    (mat/look-at e t (gu/ortho-normal (g/- e t) V3X))))

(defn scene-bounds
  [model view proj vp w h d]
  (let [w' (- w)
        h' (- h)
        d' (- d)
        [[l] [_ b] [r] [_ t]]
        (mapv
         #(project % model view proj vp)
         [(vec3 w' h d')
          (vec3 w' h' d)
          (vec3 w h d)
          (vec3 w h d')])]
    [l (- 1 r) t (- 1 b)]))

(defn scene-borders-diff
  [[l r t b]]
  [(- l r) (- t b)])

(defn compute-view
  [ex ey ez ty model proj vp w h d]
  (let [view    (make-cam ex ey ez ty)
        borders (scene-bounds model view proj vp w h d)
        diff    (mapv (fn [x] (- x 0.2)) borders)
        a-diff  (mapv (fn [x] (* x x)) diff)
        err     (reduce + a-diff)]
    [borders diff a-diff err]))

(defn refine-view
  [ex ey ez ty model proj vp w h d]
  (let [[borders diff a-diff err] (compute-view ex ey ez ty model proj vp w h d)
        [l r t b] borders
        [dl dr dt db] diff
        zoom? (every? (fn [x] (> x 0.001)) a-diff)]
    (prn diff a-diff err)
    (if zoom?
      (do (prn :zoom err) (recur ex (* ey 0.95) (* ez 0.95) (* ty 0.95) model proj vp w h d))
      (let [[_ _ _ e1] (compute-view ex (+ ey 0.01) ez ty model proj vp w h d)
            [_ _ _ e2] (compute-view ex (- ey 0.01) ez ty model proj vp w h d)
            [_ _ _ e3] (compute-view ex ey ez (+ ty 0.01) model proj vp w h d)
            [_ _ _ e4] (compute-view ex ey ez (- ty 0.01) model proj vp w h d)
            [emin y] (first (sort-by first [[e1 (+ ey 0.01)] [e2 (- ey 0.01)] [e3 (+ ty 0.01)] [e4 (- ty 0.01)]]))]
        (if (< emin (* err 0.9999))
          (if (or (= emin e1) (= emin e2))
            (recur ex y ez ty model proj vp w h d)
            (recur ex ey ez y model proj vp w h d))
          [ex ey ez ty])))))

(def m (g/rotate-y M44 m/SIXTH_PI))
(def p (mat/perspective 60 1.33333 0.1 10))
(def vp (make-view 1 1))

(defn generate-lux-scene
  [mesh]
  (let [bounds (g/bounds mesh)
        h      (g/height bounds)
        d      (g/depth bounds)
        z      (inc (max h d))
        eye    (g/* (vec3 -0.05 0.85 2) 1 h d)
        target (vec3 -0.08 0 0)
        up     (gu/ortho-normal (g/- eye target) V3X)]
    (-> (lux/lux-scene)
        (lux/configure-meshes-as-byte-arrays)
        (lux/renderer-sppm)
        (lux/sampler-sobol {})
        (lux/integrator-sppm {})
        (lux/filter-mitchell {})
        (lux/film
         {:width 480 :height 360 :halt-spp 100 :display-interval 6})
        (lux/tonemap-linear
         {:iso 200 :exposure 0.75 :f-stop 5.6 :gamma 2.2})
        (lux/camera
         {:eye eye :target target :up up :fov 60
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
         :model {:mesh mesh :material :white :tx {:ry 30}})
        (luxio/serialize-scene "codefactory" false))))

(defn lux->zip-bytes
  [scene]
  (with-open [out (ByteArrayOutputStream. 0x20000)]
    (luxio/export-archived-scene scene out)
    (.toByteArray out)))
