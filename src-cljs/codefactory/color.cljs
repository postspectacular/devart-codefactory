(ns codefactory.color
  (:require-macros
   [thi.ng.macromath.core :as mm])
  (:require
   [codefactory.config :as config]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.core.vector :as v :refer [vec3]]))

(defn opnode-color-hex
  [node]
  (if (nil? node) "#666666" (get-in config/operators [(:op node) :col])))

(defn hex->rgb
  [hex]
  (let [h (js/parseInt (subs hex 1) 16)
        i (/ 255.0)]
    [(* (bit-and (bit-shift-right h 16) 0xff) i)
     (* (bit-and (bit-shift-right h 8) 0xff) i)
     (* (bit-and h 0xff) i)]))

(defn pulsate-color
  [base target t]
  (g/mix (vec3 base) target (mm/madd (Math/sin (* t 0.5)) 0.5 0.5)))
