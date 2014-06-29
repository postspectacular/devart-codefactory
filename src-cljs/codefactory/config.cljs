(ns codefactory.config
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.aabb :as a]
   [thi.ng.geom.cuboid :as cub]
   [thi.ng.morphogen.core :as mg]
   [thi.ng.cljs.utils :as utils]
   [thi.ng.validate.core :as v]))

(def webgl
  {:min-aa-res 480
   :bg-col [0.2 0.2 0.211 1]
   :shader-preset-ids [:xray-soft :lambert-default]
   :initial-view [0.10196 0.90405 -0.30269 -0.2838]})

(def seeds
  (->> {:box  {:seed (a/aabb 1)}
        :hex2 {:seed (cub/cuboid (mg/circle-lattice-seg 6 1 0.2))}
        :hex3 {:seed (cub/cuboid (mg/sphere-lattice-seg 6 0.25 0.0955 0.2))}}
       (reduce-kv
        (fn [acc k v]
          (assoc acc k (update-in v [:seed] (comp mg/seed-box g/center))))
        {})))

(def seed-select
  {:space 1.5
   :camy -4
   :camz 1
   :rot-speed 2
   :scroll-speed 0.15})

(def operators
  {:sd         {:col "#56ffee" :label "split"}
   :skew       {:col "#ffd641" :label "tilt"}
   :sd-inset   {:col "#ed732a" :label "inset"}
   :scale-side {:col "#bd10d5" :label "scale"}
   :ext        {:col "#3fa6f2" :label "stretch"}
   :reflect    {:col "#b9c500" :label "mirror"}
   nil         {:col "#ffffff" :label "delete"}})

(defn operator-color
  [id] (:col (operators id)))

(def routes
  [{:match ["home"] :controller :home :hash "home"}
   {:match ["edit" :id]
    :bindings {:id {:validate [(v/uuid4)]}}
    :controller :editor}
   {:match ["edit" "new" :seed-id]
    :bindings {:seed-id {:validate [(v/member-of (set (map name (keys seeds))))]}}
    :controller :editor}
   {:match ["select-seed"]
    :controller :seed-selector}
   {:match ["gallery" :page]
    :bindings {:page {:coerce utils/parse-int :validate [(v/number) (v/pos)]}}
    :controller :gallery}])

(def default-route (routes 0))

(def dom-transitions
  {[:loader :home] -1
   [:loader :editor] -1
   [:loader :gallery] -1
   [:home :editor] -1
   [:home :seed-selector] -1
   [:editor :home] 1
   [:editor :seed-selector] 1
   [:seed-selector :home] 1
   })

(def controller-release-delay 900)
