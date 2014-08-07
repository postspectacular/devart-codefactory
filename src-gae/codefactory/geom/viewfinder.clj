(ns codefactory.geom.viewfinder
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.core.utils :as gu]
   [thi.ng.geom.types.utils :as tu]
   [thi.ng.geom.core.vector :refer [vec2 vec3 V3X]]
   [thi.ng.geom.core.matrix :as mat :refer [M44 M32]]
   [thi.ng.geom.rect :as r]
   [thi.ng.common.math.core :as m]
   [clojure.java.io :as io])
  (:import
   [java.io ByteArrayOutputStream]))

(defn project-point
  [p mvp vtx]
  (let [[x y _ w] (g/transform-vector mvp (conj (vec p) 1))]
    (g/transform-vector vtx (g/div (vec2 x y) w))))

(defn viewport-transform
  [width height]
  (let [w2 (/ width 2.0)
        h2 (/ height 2.0)]
    (-> mat/M32
        (g/translate w2 h2)
        (g/* (g/scale mat/M32 w2 (- h2))))))

(defn look-at
  [ex ey ez ty]
  (let [e (vec3 ex ey ez)
        t (vec3 ex ty 0)]
    (mat/look-at e t (gu/ortho-normal (g/- e t) V3X))))

(defn scene-bounds
  [mesh model view proj vtx]
  (let [mvp  (g/* proj (g/* view model))
        points (map (fn [p] (project-point p mvp vtx)) (g/vertices mesh))
        b (tu/bounding-rect points)]
    [(r/left b) (- 1 (r/right b)) (r/bottom b) (- 1 (r/top b))]
    ))

(defn compute-view
  [mesh model proj vtx ex ey ez ty margin]
  (let [view    (look-at ex ey ez ty)
        borders (scene-bounds mesh model view proj vtx)
        diff    (mapv (fn [x] (- x margin)) borders)]
    diff))

(defn compute-view*
  [mesh model proj vtx margin]
  (fn [ex ey ez ty]
    (compute-view mesh model proj vtx ex ey ez ty margin)))

(defn optimize-x
  [view ex ey ez ty err]
  (let [delta (* err 0.5)
        x1 (- ex delta)
        x2 (+ ex delta)
        [l1 r1 :as d1] (view x1 ey ez ty)
        [l2 r2 :as d2] (view x2 ey ez ty)
        ad1 (m/abs-diff l1 r1)
        ad2 (m/abs-diff l2 r2)
        [emin ex'] (if (< ad1 ad2) [ad1 x1] [ad2 x2])
        derr (- err emin)]
    ;;(prn :x err derr d1 d2)
    (if (> derr 0.001)
      (recur view ex' ey ez ty emin)
      ex')))

(defn optimize-y
  [view ex ey ez ty err]
  (let [delta (* err 0.5)
        y1 (- ty delta)
        y2 (+ ty delta)
        [_ _ t1 b1 :as d1] (view ex ey ez y1)
        [_ _ t2 b2 :as d2] (view ex ey ez y2)
        ad1 (m/abs-diff t1 b1)
        ad2 (m/abs-diff t2 b2)
        [emin ty'] (if (< ad1 ad2) [ad1 y1] [ad2 y2])
        derr (- err emin)]
    ;;(prn :y err derr d1 d2)
    (if (> derr 0.001)
      (recur view ex ey ez ty' emin)
      ty')))

(defn optimize-zoom
  [ view ex ey ez ty]
  (let [ey' (* ey 0.95)
        ez' (* ez 0.95)
        diff (view ex ey' ez' ty)]
    ;;(prn :zoom diff)
    (if (and (some (fn [x] (> x 0.05)) diff) (every? (fn [x] (> x -0.05)) diff))
      (recur view ex ey' ez' ty)
      [ex ey ez ty])))

(defn optimize-view
  [view [ex ey ez ty] iter]
  (let [ty (optimize-y view ex ey ez ty 1)
        [_ ey ez] (optimize-zoom view ex ey ez ty)
        ex (optimize-x view ex ey ez ty 1)
        state [ex ey ez ty]]
    (if (pos? iter)
      (recur view state (dec iter))
      state)))

(defn optimize-rotation
  [mesh]
  (let [b (g/bounds mesh)
        xz (/ (g/width b) (g/depth b))]
    (g/rotate-y M44 (* m/SIXTH_PI (if (< xz 1) (* (- 1 xz) 3) 1)))))
