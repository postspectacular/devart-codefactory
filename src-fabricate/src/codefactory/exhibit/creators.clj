(ns codefactory.exhibit.creators
  (:require
   [codefactory.exhibit.svg :as svg]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.core.utils :as gu]
   [thi.ng.geom.core.vector :as v :refer [vec3 V3X V3Y V3Z]]
   [thi.ng.geom.core.matrix :as mat :refer [M44]]
   [thi.ng.geom.aabb :as a]
   [thi.ng.geom.gmesh :as gm]
   [thi.ng.geom.mesh.io :as mio]
   [thi.ng.morphogen.core :as mg]
   [thi.ng.common.data.core :as d]
   [thi.ng.common.math.core :as m :refer [*eps* HALF_PI PI TWO_PI]]
   [thi.ng.macromath.core :as mm]
   [clojure.java.io :as io]
   [clojure.walk :refer :all]))

(def character
  (mg/subdiv
   :slices 5
   :out [nil nil
         (mg/subdiv
          :cols 7
          :out [(mg/subdiv :rows 4
                           :out [nil nil {}
                                 (mg/subdiv :rows 3 :out [{}])])
                (mg/subdiv :rows 4
                           :out [nil nil nil (mg/subdiv :rows 3 :out [{}])])
                (mg/subdiv :rows 2
                           :out [(mg/split-displace
                                  :y :z :offset 0.05 :out [{} {}])
                                 (mg/subdiv
                                  :rows 2
                                  :out [{}
                                        (mg/subdiv
                                         :rows 3 :out [{}])])])
                (mg/subdiv :rows 2
                           :out [nil
                                 (mg/subdiv
                                  :rows 2
                                  :out [{}
                                        (mg/subdiv
                                         :rows 3
                                         :out [{} {}
                                               (mg/extrude
                                                :dir :n :len 0.16
                                                :out [(mg/extrude
                                                       :dir :e :len 0.05
                                                       :out [(mg/extrude
                                                              :dir :w :len 0.05
                                                              :out [(mg/subdiv
                                                                     :rows 3
                                                                     :out [(mg/subdiv
                                                                            :slices 4
                                                                            :out [{} {} {}
                                                                                  (mg/subdiv
                                                                                   :cols 7
                                                                                   :rows 4
                                                                                   :out [{} {} {} {} {} {} {}
                                                                                         {} nil nil nil nil nil {}
                                                                                         {} nil {} {} {} nil {}
                                                                                         {} {} {} {} {} {} {}])])
                                                                           (mg/subdiv
                                                                            :slices 4
                                                                            :out [{} {} {}
                                                                                  (mg/subdiv
                                                                                   :cols 7
                                                                                   :rows 4
                                                                                   :out {15 nil 19 nil})])
                                                                           (mg/subdiv
                                                                            :cols 11
                                                                            :out (take 11 (cycle [nil {}])))])])])])])])])
                (mg/subdiv :rows 2
                           :out [{}
                                 (mg/subdiv :rows 2
                                            :out [{}
                                                  (mg/subdiv :rows 3 :out [{}])])])
                (mg/subdiv :rows 4
                           :out [nil nil nil (mg/subdiv :rows 3 :out [{}])])
                (mg/subdiv :rows 4
                           :out [nil
                                 nil
                                 {}
                                 (mg/subdiv :rows 3 :out [{}])])])
         nil nil]))

(mg/save-mesh (mg/seed (a/aabb 1)) character "p.ply" 1000)


(prewalk
 (fn [x]
   (cond
    (nil? x) (do (prn "<empty>") nil)
    (:op x) (do (prn (:op x) (count (:out x)))
                (:out x))
    :default x))
 character)
