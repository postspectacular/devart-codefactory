(ns codefactory.config
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.types.utils :as tu]
   [thi.ng.geom.core.vector :refer [vec3 V3Y V3Z]]
   [thi.ng.geom.aabb :as a]
   [thi.ng.geom.cuboid :as cub]
   [thi.ng.morphogen.core :as mg]
   [thi.ng.cljs.utils :as utils]
   [thi.ng.validate.core :as v]))

(def webgl
  {:min-aa-res 480
   :bg-col [0.2 0.2 0.211 1]
   :shader-preset-ids [:xray-strong :lambert-default]
   :initial-view [0.10196 0.90405 -0.30269 -0.2838]})

(def seeds
  (->> {:box  {:seed (a/aabb 1)}
        :hex2 {:seed (cub/cuboid (mg/circle-lattice-seg 6 1 0.5))}
        :oct2 {:seed (cub/cuboid (mg/circle-lattice-seg 8 1 0.5))}
        :tri2 {:seed (cub/cuboid (mg/circle-lattice-seg 3 1 0.4))}}
       (reduce-kv
        (fn [acc k v]
          (assoc
              acc k
              (update-in
               v [:seed]
               #(->> [%]
                     (tu/fit-all-into-bounds (a/aabb 1))
                     first
                     g/center
                     mg/seed-box))))
        {})))

(def seed-select
  {:space 1.5
   :camz -4
   :camy 1
   :cam-up V3Y
   :cam-offset 0.2
   :rot-speed 2
   :scroll-speed 0.15
   :falloff [1.0 0.4 0.2]})

(def editor
  {:inset 11
   :gap 5
   :min-size 24})

(def operators
  {:sd             {:col "#56ffee" :label "split"}
   :skew           {:col "#ffd641" :label "tilt"}
   :sd-inset       {:col "#ed732a" :label "inset"}
   :scale-side     {:col "#bd10d5" :label "scale"}
   :extrude        {:col "#3fa6f2" :label "stretch"}
   :reflect        {:col "#89c33f" :label "mirror"}
   :leaf           {:col "#ffffff" :label "leaf"}
   :shift-displace {:col "#b9c500" :label "shift"}
   :delete         {:col "#aaaaaa" :label "delete"}
   })

(defn operator-color
  [id] (:col (operators id)))

(def routes
  [{:match ["home"]
    :controller :home
    :hash "home"}
   {:match ["objects" :id]
    :bindings {:id {:validate [(v/uuid4)]}}
    :controller :editor}
   {:match ["objects" "new" :seed-id]
    :bindings {:seed-id {:validate [(v/member-of (set (map name (keys seeds))))]}}
    :controller :editor}
   {:match ["select" :seed-id]
    :bindings {:seed-id {:validate [(v/member-of (set (map name (keys seeds))))]}}
    :controller :selector}
   {:match ["select"]
    :controller :selector}
   {:match ["objects" "submit"]
    :controller :submit-form}
   {:match ["gallery" :page]
    :bindings {:page {:coerce utils/parse-int :validate [(v/number) (v/pos)]}}
    :controller :gallery}
   {:match ["gallery"]
    :controller :gallery}
   {:match ["login"]
    :controller :login}])

(def routes-unsupported
  [{:match ["not-supported"]
    :hash "not-supported"
    :controller :upgrade-browser}])

(def default-route (first routes))
(def default-route-unsupported (first routes-unsupported))

(def dom-transitions
  {[:loader :home] -1
   [:loader :editor] -1
   [:loader :gallery] -1
   [:loader :login] -1
   [:loader :submit-form] -1
   [:home :editor] -1
   [:home :selector] -1
   [:editor :home] 1
   [:editor :selector] 1
   [:editor :submit-form] -1
   [:submit-form :editor] 1
   [:selector :home] 1
   [:login :home] -1
   })

(def api-prefix "/api/1.0/")

(def api-routes
  (->> {:get-object "objects/"
        :submit-object "objects"}
       (reduce-kv
        (fn [acc k v] (assoc acc k (str api-prefix v)))
        {})))

(def controller-release-delay 900)

(def timeouts
  {:selector 10000
   :editor (* 2 60 1000)})
