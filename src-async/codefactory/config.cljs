(ns codefactory.config
  (:require
   [thi.ng.cljs.dom :as dom]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.types.utils :as tu]
   [thi.ng.geom.core.vector :refer [vec3 V3Y V3Z]]
   [thi.ng.geom.aabb :as a]
   [thi.ng.geom.cuboid :as cub]
   [thi.ng.morphogen.core :as mg]
   [thi.ng.common.math.core :refer [HALF_PI]]
   [thi.ng.cljs.utils :as utils]
   [thi.ng.validate.core :as v]))

(defn scale-op
  [side scale & [out]]
  {:op :scale-side
   :args {:side side :scale scale}
   :out (mg/operator-output 1 out false)})

(declare app scale-op)

(def api-prefix "/api/1.0/")

(def op-aliases
  {:sd      :sd
   :inset   :sd-inset
   :reflect :reflect
   :tilt    :skew
   :shift   :split-displace2
   :stretch :ext-prop
   :scale   :scale-side
   :delete  :delete
   :leaf    :leaf})

(def op-aliases-reverse
  (zipmap (vals op-aliases) (keys op-aliases)))

(def seeds
  (->>
   {:box   {:seed (a/aabb 1)
            :initial-view {:view [0.1011 0.904 -0.3027 -0.284] :dist 2.5}}
    :pent3 {:seed (g/rotate-z (cub/cuboid (mg/sphere-lat 5 5 0.25)) (- HALF_PI))
            :initial-view {:view [0.0893 0.9233 -0.2117 -0.3055] :dist 1.5}}
    :hex3  {:seed (g/rotate-z (cub/cuboid (mg/sphere-lat 6 12 0.25)) (- HALF_PI))
            :initial-view {:view [0.0893 0.9233 -0.2117 -0.3055] :dist 1.5}}
    :oct3  {:seed (g/rotate-z (cub/cuboid (mg/sphere-lat 8 8 0.25)) (- HALF_PI))
            :initial-view {:view [0.0893 0.9233 -0.2117 -0.3055] :dist 1.5}}
    :pent2 {:seed (cub/cuboid (mg/circle-lattice-seg 5 1 0.5))
            :initial-view {:view [0.1011 0.904 -0.3027 -0.284] :dist 2}}
    :hex2  {:seed (cub/cuboid (mg/circle-lattice-seg 6 1 0.5))
            :initial-view {:view [0.1011 0.904 -0.3027 -0.284] :dist 2}}
    :oct2  {:seed (cub/cuboid (mg/circle-lattice-seg 8 1 0.5))
            :initial-view {:view [0.1011 0.904 -0.3027 -0.284] :dist 1.75}}
    :tri2  {:seed (cub/cuboid (mg/circle-lattice-seg 3 1 0.4))
            :initial-view {:view [0.1011 0.904 -0.3027 -0.284] :dist 1.5}}}
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

(def ^:export default-config
  {:modules
   {:home true
    :selector true
    :editor true
    :submit true
    :thanks true}

   :webgl
   {:min-aa-res 480
    :bg-col [0.2 0.2 0.211 1]
    :shader-preset-ids [:xray-soft :lambert-default]}

   :seeds seeds

   :seed-select
   {:space 1.5
    :camz -4
    :camy 1
    :cam-up V3Y
    :cam-offset 0.2
    :rot-speed 2
    :scroll-speed 0.15
    :falloff [1.0 0.4 0.2]}

   :editor
   {:inset 11
    :gap 2
    :margin 10
    :margin-bottom 70
    :map-width 200
    :map-height 245
    :min-label-width 50
    :height 245
    :min-size 24
    :map-bg "#222223"
    :map-selection "#a8a800"
    :map-color-offset -0.33
    :map-label-font "14px \"Abel\",sans-serif"
    :map-label-size 18
    :toolbar-icon-size [32 32]
    :toolbar-op-width 65
    :toolbar-sep-size [15 50]
    :toolbar-margin-left 65
    :toolbar-margin-right 65
    :root-label "<h1>TAP HERE TO BEGIN</h1>"
    :leaf-label "+"
    :map-labels ["CODE OVERVIEW" "This area always shows" "a map of your entire code."]}

   :operators
   {:sd      {:col "#56ffee" :label "split"
              :paths [{:d "M0,0 L1,0 L1,1 L0,1 Z"}
                      {:d "M0.5,0 L0.5,1"
                       :style {:stroke-dasharray "3.75,7.5"
                               :stroke-dashoffset "0"}}]}
    :tilt    {:col "#ffd641" :label "tilt"
              :paths [{:d "M0,1 L0.15,0 L1,0 L0.85,1 Z"}]}
    :inset   {:col "#ed732a" :label "inset"
              :paths [{:d "M0,0 L1,0 L1,1 L0,1 Z M0,0 L0.25,0.25 M1,0 L0.75,0.25 M1,1 L0.75,0.75 M0,1 L0.25,0.75"
                       :style {:opacity "0.3"}}
                      {:d "M0.25,0.25 L0.75,0.25 L0.75,0.75 L0.25,0.75 Z"}]}
    :scale   {:col "#bd10d5" :label "scale"
              :paths [{:d "M0,0 L1,0 L1,1 L0,1 z"
                       :style {:opacity "0.3"}}
                      {:d "M0,1 L0,0.5 L0.5,0.5 L0.5,1 Z"}]}
    :stretch {:col "#3fa6f2" :label "stretch"
              :paths [{:d "M0,0.5 L1,0.5 L1,1 L0,1 Z"}
                      {:d "M0,0.5 L0,0 L1,0 L1,0.5"
                       :style {:opacity "0.3"}}]}
    :reflect {:col "#89c33f" :label "mirror"
              :paths [{:d "M0,0 L1,0 L1,1 L0,1 Z M0.5,0 L0.5,1"}]}
    :shift   {:col "#b9c500" :label "shift"
              :paths [{:d "M0.2,0.5 L0.8,0.5 L1,1 L0,1 Z"}
                      {:d "M0.2,0.5 L0,0 L1,0 L0.8,0.5"
                       :style {:opacity "0.3"}}]}
    :delete  {:col "#aaaaaa" :label "delete"
              :paths [{:d "M0,0 L1,0 L1,1 L0,1 Z M0,0 L1,1 M0,1 L1,0"}]}
    :leaf    {:col "#ffffff" :label "leaf"}
    :undo    {:col "#aaaaaa" :label "undo"
              :paths [{:d "M0,0 L1,0 L1,1 L0,1 Z M0.2,0.8 L0.2,0.2 L0.8,0.2 M0.2,0.2 L0.8,0.8"}]}}

   :op-presets
   [[:splitx2 {:label "split x2" :node (mg/subdiv :cols 2)}]
    [:splity2 {:label "split y2" :node (mg/subdiv :rows 2)}]
    [:splitz2 {:label "split z2" :node (mg/subdiv :slices 2)}]
    [:splitx3 {:label "split x3" :node (mg/subdiv :cols 3)}]
    [:splity3 {:label "split y3" :node (mg/subdiv :rows 3)}]
    [:splitz3 {:label "split z3" :node (mg/subdiv :slices 3)}]
    [:sep]
    [:insetx {:label "inset x" :node (mg/subdiv-inset :dir :x :inset 0.5)}]
    [:insety {:label "inset y" :node (mg/subdiv-inset :dir :y :inset 0.5)}]
    [:insetz {:label "inset z" :node (mg/subdiv-inset :dir :z :inset 0.5)}]
    [:sep]
    [:mirrore {:label "mirror e" :node (mg/reflect :dir :e)}]
    [:mirrorw {:label "mirror w" :node (mg/reflect :dir :w)}]
    [:mirrorn {:label "mirror n" :node (mg/reflect :dir :n)}]
    [:mirrors {:label "mirror s" :node (mg/reflect :dir :s)}]
    [:mirrorf {:label "mirror f" :node (mg/reflect :dir :f)}]
    [:mirrorb {:label "mirror b" :node (mg/reflect :dir :b)}]
    [:sep]
    [:scalee {:label "scale e" :node (scale-op :e 0.5)}]
    [:scalew {:label "scale w" :node (scale-op :w 0.5)}]
    [:scalen {:label "scale n" :node (scale-op :n 0.5)}]
    [:scales {:label "scale s" :node (scale-op :s 0.5)}]
    [:scalef {:label "scale f" :node (scale-op :f 0.5)}]
    [:scaleb {:label "scale b" :node (scale-op :b 0.5)}]
    [:sep]
    [:tiltef {:label "tilt ef" :node (mg/skew :e :f :offset 0.5)}]
    [:tiltwf {:label "tilt wf" :node (mg/skew :w :f :offset 0.5)}]
    [:tiltnf {:label "tilt nf" :node (mg/skew :n :f :offset 0.5)}]
    [:tiltsf {:label "tilt sf" :node (mg/skew :s :f :offset 0.5)}]
    [:sep]
    [:stretche {:label "stretch e" :node (mg/extrude-prop :dir :e :len 0.5)}]
    [:stretchw {:label "stretch w" :node (mg/extrude-prop :dir :w :len 0.5)}]
    [:stretchn {:label "stretch n" :node (mg/extrude-prop :dir :n :len 0.5)}]
    [:stretchs {:label "stretch s" :node (mg/extrude-prop :dir :s :len 0.5)}]
    [:stretchf {:label "stretch f" :node (mg/extrude-prop :dir :f :len 0.5)}]
    [:stretchb {:label "stretch b" :node (mg/extrude-prop :dir :b :len 0.5)}]
    
    ]

   :routes
   [{:match ["home"]
     :controller :home
     :hash "home"}
    #_{:match ["objects" :id]
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
    {:match ["thanks" :id]
     :bindings {:id {:validate [(v/uuid4)]}}
     :controller :submit-confirm}
    #_{:match ["gallery" :page]
       :bindings {:page {:coerce utils/parse-int :validate [(v/number) (v/pos)]}}
       :controller :gallery}
    #_{:match ["gallery"]
       :controller :gallery}
    #_{:match ["login"]
       :controller :login}]

   :routes-unsupported
   [{:match ["not-supported"]
     :hash "not-supported"
     :controller :upgrade-browser}]

   :default-route 0
   :default-route-unsupported 0

   :dom-transitions
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
    [:submit-form :submit-confirm] -1
    [:submit-confirm :home] 1
    [:selector :home] 1
    [:login :home] -1
    }

   :dom-components
   {:home-continue    "home-continue"
    :fullscreen       "fs-toggle"
    :seed-canvas      "seed-canvas"
    :edit-canvas      "edit-canvas"
    :edit-continue    "edit-continue"
    :toolbar          "toolbar"
    :tools            "tools"
    :slider           "slider"
    :slider-wrapper   "slider-wrapper"
    :slider-range     "slider-val"
    :slider-label     "slider-label"
    :slider-val-label "slider-val-label"
    :viz-container    "viz-container"
    :viz-map          "viz-map"
    :thanks-cancel    "thanks-cancel"
    :preview-label    "preview-label"
    :toolbar-label    "toolbar-label"
    :submit-button    "bt-submit"
    :submit-cancel    "submit-cancel"}

   :timeouts
   {:selector 20000
    :editor (* 2 60 1000)
    :thanks 5000
    :controller-release-delay 900}

   :api
   {:prefix api-prefix
    :routes
    (->> {:get-object "objects/"
          :submit-object "objects"}
         (reduce-kv
          (fn [acc k v] (assoc acc k (str api-prefix v)))
          {}))}
   })

(def ^:export app default-config)

(def ^:export maintenance
  (-> default-config
      (assoc :modules nil
             :routes [(get-in app [:routes 0])]
             :routes-unsupported [(get-in app [:routes 0])])))

(def ^:export barbican
  (-> default-config
      (assoc-in [:api :inject] {:location "barbican"})))

(def ^:export workshop
  (-> app
      (assoc-in [:api :inject] {:location "workshop"})
      (assoc-in [:modules :workshop] true)
      (assoc-in [:timeouts :editor] (* 30 60 1000))
      (assoc-in [:timeouts :thanks] (* 5 60 1000))))

(defn set-config!
  [sym] (set! app (js/eval (aget js/window sym))))

(defn operator
  [op] (-> app :operators op))

(defn operator-color
  [op] (-> app :operators op :col))

(defn preset-node
  [op]
  (some
   #(if (= op (first %)) (-> % second :node))
   (:op-presets app)))

(defn translate-mg-op
  [op] (op-aliases-reverse op))

(def dom-component
  (memoize (fn [id] (-> app :dom-components id dom/by-id))))

(defn dom-component*
  [id] (-> app :dom-components id dom/by-id))

(defn api-route
  [id] (-> app :api :routes id))

(defn inject-api-request-data
  [data]
  (merge (-> app :api :inject) data))

(defn timeout
  [id] (-> app :timeouts id))

(defn transition
  [a b] ((:dom-transitions app) [a b]))
