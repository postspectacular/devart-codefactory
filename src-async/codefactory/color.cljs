(ns codefactory.color
  (:require-macros
   [thi.ng.macromath.core :as mm])
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.core.vector :as v :refer [vec3]]
   [thi.ng.common.math.core :as m]))

(def hex->rgb
  (memoize
   (fn [hex]
     (let [h (js/parseInt (subs hex 1) 16)
           i (/ 255.0)]
       [(* (bit-and (bit-shift-right h 16) 0xff) i)
        (* (bit-and (bit-shift-right h 8) 0xff) i)
        (* (bit-and h 0xff) i)]))))

(def rgb->hex
  (memoize
   (fn [r g b]
     (let [c (bit-or
              (bit-shift-left (int (* r 255)) 16)
              (bit-shift-left (int (* g 255)) 8)
              (int (* b 255)))
           hex (.toString (js/Number. c) 16)
           len (count hex)]
       (str "#"
            (if (< len 6)
              (str (subs "00000" 0 (- 6 len)) hex)
              hex))))))

(defn gray-offset-hex
  [hex x]
  (->> (hex->rgb hex)
       (mapv (fn [c] (m/clamp (+ c x) 0.0 1.0)))
       (apply rgb->hex)))

(defn pulsate
  [base target t speed]
  (g/mix (vec3 base) target (mm/madd (Math/sin (* t speed)) 0.5 0.5)))
