(ns codefactory.config)

(def module-name "codefactory")

(def canvas-bg [0.2 0.2 0.211 1])

(def shader-preset-ids [:xray-soft :lambert-default])

(def operators
  {:subdiv     {:col [0x56 0xff 0xee] :label "split"}
   :skew       {:col [0xff 0xd6 0x41] :label "tilt"}
   :sd-inset   {:col [0xed 0x73 0x2a] :label "inset"}
   :scale-side {:col [0xbd 0x10 0xd5] :label "scale"}
   :extrude    {:col [0x3f 0xa6 0xf2] :label "stretch"}
   :reflect    {:col [0xb9 0xc5 0x00] :label "mirror"}
   :delete     {:col [0x99 0x99 0x99] :label "delete"}})

