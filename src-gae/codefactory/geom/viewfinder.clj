(ns codefactory.geom.viewfinder
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.core.utils :as gu]
   [thi.ng.geom.core.vector :refer [vec2 vec3 V3X]]
   [thi.ng.geom.core.matrix :as mat :refer [M44 M32]]
   [thi.ng.common.math.core :as m]
   [clojure.java.io :as io])
  (:import
   [java.io ByteArrayOutputStream]))

(defn project
  [pp m vp vtx]
  (let [p' (g/transform-vector m pp)
        [x y _ w] (g/transform-vector vp (conj (vec p') 1))]
    (g/transform-vector vtx (g/div (vec2 x y) w))))

(defn viewport-transform
  [width height]
  (let [w2    (/ width 2.0)
        h2    (/ height 2.0)
        scale (g/scale mat/M32 w2 (- h2))]
    (-> mat/M32
        (g/translate w2 h2)
        (g/* scale))))

(defn look-at
  [ex ey ez ty]
  (let [e (vec3 ex ey ez)
        t (vec3 ex ty 0)]
    (mat/look-at e t (gu/ortho-normal (g/- e t) V3X))))

(defn scene-bounds
  [model view proj vtx w h d]
  (let [w' (- w)
        h' (- h)
        d' (- d)
        vp (g/* proj view)
        [[l] [_ b] [r] [_ t]]
        (mapv
         #(project % model vp vtx)
         [(vec3 w' h d')
          (vec3 w' h' d)
          (vec3 w h d)
          (vec3 w h d')])]
    [l (- 1 r) t (- 1 b)]))

(defn compute-view
  [ex ey ez ty model proj vp w h d]
  (let [view    (look-at ex ey ez ty)
        borders (scene-bounds model view proj vp w h d)
        diff    (mapv (fn [x] (- x 0.15)) borders)]
    diff))

(defn find-target-x
  [ex ey ez ty model proj vp w h d err]
  (let [delta      (* err 0.5)
        x1         (- ex delta)
        x2         (+ ex delta)
        [l1 r1]    (compute-view x1 ey ez ty model proj vp w h d)
        [l2 r2]    (compute-view x2 ey ez ty model proj vp w h d)
        ad1        (m/abs-diff l1 r1)
        ad2        (m/abs-diff l2 r2)
        [emin ex'] (if (< ad1 ad2) [ad1 x1] [ad2 x2])
        derr       (- err emin)]
    (if (> derr 0.001)
      (recur ex' ey ez ty model proj vp w h d emin)
      ex')))

(defn find-target-y
  [ex ey ez ty model proj vp w h d err]
  (let [delta       (* err 0.5)
        y1          (- ty delta)
        y2          (+ ty delta)
        [_ _ t1 b1] (compute-view ex ey ez y1 model proj vp w h d)
        [_ _ t2 b2] (compute-view ex ey ez y2 model proj vp w h d)
        ad1         (m/abs-diff t1 b1)
        ad2         (m/abs-diff t2 b2)
        [emin ty']  (if (< ad1 ad2) [ad1 y1] [ad2 y2])
        derr        (- err emin)]
    (if (> derr 0.001)
      (recur ex ey ez ty' model proj vp w h d emin)
      ty')))

(defn find-zoom
  [ex ey ez ty model proj vp w h d]
  (let [ey'  (* ey 0.95)
        ez'  (* ez 0.95)
        diff (compute-view ex ey' ez' ty model proj vp w h d)]
    (if (every? pos? diff)
      (recur ex ey' ez' ty model proj vp w h d)
      [ex ey ez ty])))

(defn optimize-view
  [ex ey ez model proj vp bounds]
  (let [bounds    (g/scale bounds 0.5)
        w         (g/width bounds)
        h         (g/height bounds)
        d         (g/depth bounds)
        ty        (find-target-y ex ey ez 0 model proj vp w h d 1)
        [_ ey ez] (find-zoom ex ey ez ty model proj vp w h d)
        ex        (find-target-x ex ey ez ty model proj vp w h d 1)]
    [ex ey ez ty]))
