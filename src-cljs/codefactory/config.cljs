(ns codefactory.config
  (:require
   [thi.ng.cljs.utils :as utils]
   [thi.ng.validate.core :as v]))

(def routes
  [{:match ["home"] :controller :home :uri "home"}
   {:match ["edit" :id]
    :bindings {:id {:validate [(v/uuid4)]}}
    :controller :editor}
   {:match ["edit" "new"]
    :controller :editor}
   {:match ["gallery" :page]
    :bindings {:page {:coerce utils/parse-int :validate [(v/number) (v/pos)]}}
    :controller :gallery}])

(def default-route (routes 0))

(def route-transitions
  {[:loader :home] -1
   [:loader :editor] -1
   [:loader :gallery] -1
   [:home :editor] -1
   [:editor :home] 1})

(def canvas-bg [0.2 0.2 0.211 1])

(def shader-preset-ids [:xray-soft :lambert-default])

(def operators
  {:sd         {:col "#56ffee" :label "split"}
   :skew       {:col "#ffd641" :label "tilt"}
   :sd-inset   {:col "#ed732a" :label "inset"}
   :scale-side {:col "#bd10d5" :label "scale"}
   :ext        {:col "#3fa6f2" :label "stretch"}
   :reflect    {:col "#b9c500" :label "mirror"}
   nil         {:col "#ffffff" :label "delete"}})

(defn opnode-color-hex
  [node]
  (if (nil? node) "#666666" (get-in operators [(:op node) :col])))
