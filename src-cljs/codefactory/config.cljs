(ns codefactory.config)

(def module-name "codefactory")

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
