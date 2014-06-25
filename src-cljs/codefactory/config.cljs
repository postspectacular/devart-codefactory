(ns codefactory.config
  (:require
   [thi.ng.cljs.utils :as utils]
   [thi.ng.validate.core :as v]))

(def routes
  [{:match ["home"] :controller :home :hash "home"}
   {:match ["edit" :id]
    :bindings {:id {:validate [(v/uuid4)]}}
    :controller :editor}
   {:match ["edit" "new"]
    :controller :editor}
   {:match ["gallery" :page]
    :bindings {:page {:coerce utils/parse-int :validate [(v/number) (v/pos)]}}
    :controller :gallery}])

(def default-route (routes 0))

(def dom-transitions
  {[:loader :home] -1
   [:loader :editor] -1
   [:loader :gallery] -1
   [:home :editor] -1
   [:editor :home] 1})

(def controller-release-delay 900)

(def webgl
  {:min-aa-res 480
   :bg-col [0.2 0.2 0.211 1]
   :shader-preset-ids [:xray-soft :lambert-default]
   :initial-view [0.10196 0.90405 -0.30269 -0.2838]})

(def operators
  {:sd         {:col "#56ffee" :label "split"}
   :skew       {:col "#ffd641" :label "tilt"}
   :sd-inset   {:col "#ed732a" :label "inset"}
   :scale-side {:col "#bd10d5" :label "scale"}
   :ext        {:col "#3fa6f2" :label "stretch"}
   :reflect    {:col "#b9c500" :label "mirror"}
   nil         {:col "#ffffff" :label "delete"}})
