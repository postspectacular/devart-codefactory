(ns codefactory.config
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.types.utils :as tu]
   [thi.ng.geom.core.vector :refer [vec3 V3Y V3Z]]
   [thi.ng.geom.aabb :as a]
   [thi.ng.geom.cuboid :as cub]
   [thi.ng.morphogen.core :as mg]
   [thi.ng.common.math.core :refer [HALF_PI]]
   [thi.ng.cljs.utils :as utils]
   [thi.ng.validate.core :as v]))

(def api-prefix "/api/1.0/")

(def op-aliases
  {:sd             :sd
   :inset          :sd-inset
   :reflect        :reflect
   :tilt           :skew
   :shift          :split-displace2
   :stretch        :ext-prop
   :scale          :scale-side
   :delete         :delete
   :leaf           :leaf})

(def op-aliases-reverse (zipmap (vals op-aliases) (keys op-aliases)))

(defn operator-color
  [config op] (get-in config [:operators op :col]))

(defn translate-mg-op
  [config op] (op-aliases-reverse op))


(def seeds
  (->> {:box   {:seed (a/aabb 1)
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

(def ^:export app
  {:modules {:home true
             :selector true
             :editor true
             :submit true
             :thanks true}
   :webgl
   {:min-aa-res 480
    :bg-col [0.2 0.2 0.211 1]
    :shader-preset-ids [:xray-strong :lambert-default]}

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
    :root-label "<h1>TAP HERE TO BEGIN</h1><p>This is your workspace area.</p><p>Here, each shape is visualized as box.</p><p>Assign operations to these elements to create your artwork.</p><p>Each operation creates more shapes, forming a hierarchy.</p><p>You can also delete elements to create more complex forms.</p>"
    :map-labels ["CODE OVERVIEW" "This area always shows" "the entire code structure." "Use this widget" "to navigate your code."]}

   :operators
   {:sd             {:col "#56ffee" :label "split"
                     :help "This operation splits the selected shape into smaller pieces using a regular grid. The shape can be split in all 3 directions."}
    :tilt           {:col "#ffd641" :label "tilt"
                     :help "This operation moves one of the shape's sides in a given direction to create a possibly more diamond-like shape."}
    :inset          {:col "#ed732a" :label "inset"
                     :help "This operation splits the selected shape into five smaller pieces by moving its corners towards the center. The resulting shapes are four walls and the core enclosed by them."}
    :scale          {:col "#bd10d5" :label "scale"
                     :help "This operation deforms the selected shape by scaling one of its sides. This is useful to create cones or rings (if combined with the MIRROR operation)."}
    :stretch        {:col "#3fa6f2" :label "stretch"
                     :help "This operation stretches the selected shape into the direction of one of its six sides."}
    :reflect        {:col "#89c33f" :label "mirror"
                     :help "This operation mirrors the selected shape on one of its six sides.<br/>Depending on the shape, repeated use with the same direction will result in rings."}
    :leaf           {:col "#ffffff" :label "leaf"}
    :shift          {:col "#b9c500" :label "shift"
                     :help "This operation splits the selected shape in the middle and tilts the resulting halves to form a chevron."}
    :delete         {:col "#aaaaaa" :label "delete"}}

   :op-presets
   [{:label "split x" :node (mg/subdiv :cols 2)}
    {:label "split y" :node (mg/subdiv :rows 2)}
    {:label "split z" :node (mg/subdiv :slices 2)}
    {:label "inset x" :node (mg/subdiv-inset :dir :x :inset 0.5)}
    {:label "inset y" :node (mg/subdiv-inset :dir :y :inset 0.5)}
    {:label "inset z" :node (mg/subdiv-inset :dir :z :inset 0.5)}
    {:label "mirror e" :node (mg/reflect :dir :e)}
    {:label "mirror w" :node (mg/reflect :dir :e)}
    {:label "mirror n" :node (mg/reflect :dir :e)}
    {:label "mirror s" :node (mg/reflect :dir :e)}
    {:label "mirror f" :node (mg/reflect :dir :e)}
    {:label "mirror b" :node (mg/reflect :dir :e)}
    ]
   
   :op->mg op-aliases

   :routes
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

   :timeouts
   {:selector 20000
    :editor (* 2 60 1000)
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

(def ^:export maintenance
  (-> app
      (assoc :modules nil
             :routes [(get-in app [:routes 0])]
             :routes-unsupported [(get-in app [:routes 0])])))

(def ^:export barbican
  (-> app
      (assoc-in [:api :inject] {:location "barbican"})))
