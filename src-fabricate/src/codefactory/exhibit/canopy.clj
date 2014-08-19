(ns codefactory.exhibit.canopy
  (:require
   [codefactory.exhibit.utils :refer :all]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.core.utils :as gu]
   [thi.ng.geom.core.vector :as v :refer [vec3 V3Y V3Z]]
   [thi.ng.geom.core.matrix :refer [M44]]
   [thi.ng.geom.bezier :as b]
   [thi.ng.geom.polygon :as p]
   [thi.ng.geom.quad :as q]
   [thi.ng.geom.gmesh :as gm]
   [thi.ng.common.data.core :as d]
   [thi.ng.common.math.core :as m :refer [HALF_PI PI TWO_PI]]))

;; lathe curve for panels/tiles, defined in XZ plane

(def canopy-segments 13)
(def canopy-res 26)

(def lathe
  (let [points (mapv (comp :xzy vec3) [[0.46 0.1] [0.46 1.0] [0.7 2.75] [1.19 3.0]])]
    (-> points
        (b/sample-segment canopy-segments)
        (vec)
        (conj (peek points)))))

#_(def canopy-flat
    (-> (gm/lathe-mesh lathe canopy-res TWO_PI g/rotate-z
                       #(vector (q/inset-quad % 0.003)))
        (g/transform (g/translate M44 0 -0.5 0))))

;; (def wires (make-wires canopy-flat (vec3 0 -0.5 3.05) 1.80 0.0005))

(def canopy
  (-> (gm/lathe-mesh lathe canopy-res TWO_PI g/rotate-z
                     (inset-and-extrude-quad 0.003 0.005))
      (g/transform (g/translate M44 0 -0.5 0))
      (g/tessellate)))

(def canopy-panels
  (->> (lathe-raw lathe canopy-res TWO_PI g/rotate-z (make-panel-spec 0.003))
       (take canopy-segments)))
