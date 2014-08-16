(ns codefactory.config
  (:require
   [thi.ng.cljs.dom :as dom]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.core.utils :as gu]
   [thi.ng.geom.types.utils :as tu]
   [thi.ng.geom.core.vector :refer [vec3 V3Y V3Z]]
   [thi.ng.geom.core.matrix :as mat]
   [thi.ng.geom.core.quaternion :as q]
   [thi.ng.geom.aabb :as a]
   [thi.ng.geom.cuboid :as cub]
   [thi.ng.morphogen.core :as mg]
   [thi.ng.common.math.core :refer [HALF_PI SQRT2]]
   [thi.ng.cljs.utils :as utils :refer [deep-merge]]
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

(defn seed-3d
  [points]
  (let [[a b c d e f g h] (mapv #(g/rotate-z % (- HALF_PI)) points)]
    (cub/cuboid [b f g c a e h d])))

(def seeds
  (->>
   {:box   {:seed (a/aabb 1)
            :scale 0.71
            :initial-view {:view [0.1011 0.904 -0.3027 -0.284] :dist 2.5}}
    :pent3 {:seed (seed-3d (mg/sphere-lat 5 5 0.25))
            :initial-view {:view [0.0893 0.9233 -0.2117 -0.3055] :dist 1.5}}
    :hex3  {:seed (seed-3d (mg/sphere-lat 6 12 0.25))
            :initial-view {:view [0.0893 0.9233 -0.2117 -0.3055] :dist 1.5}}
    :oct3  {:seed (seed-3d (mg/sphere-lat 8 8 0.25))
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

(def cam-views
  {:x  [0 (/ SQRT2) 0 (/ SQRT2)]
   :ix [0 (/ SQRT2) 0 (- (/ SQRT2))]
   :y  [0 (/ SQRT2) (- (/ SQRT2)) 0]
   :z  [0 1 0 0]})

(defn face-right
  [[a b c d]]
  (let [m2 (g/mix b c)
        m1 (g/mix a d)]
    (g/normalize (g/- m2 m1))))

(def view-face
  (memoize
   (fn [id]
     (fn [node]
       (let [verts (mg/face-vertices node id)
             n (mg/quad-normal verts)
             r (face-right verts)]
         (q/quat-from-matrix
          (mat/look-at n (vec3) (gu/ortho-normal n r))))))))

(def ^:export default-config
  {:modules
   {:home          true
    :gallery       true
    :selector      true
    :editor        true
    :submit        true
    :thanks        true
    :about         true
    :object-loader true}

   :modules-fallback
   {:about   true
    :gallery true}

   :min-window-size [480 600]

   :webgl
   {:min-aa-res 480
    :bg-col [0.2 0.2 0.211 1]
    :shader-preset-ids [:xray-soft :lambert-default]
    :axis {:radius 0.005 :length 2}}

   :home
   {:default-bg "/img/renders/20140701.jpg"
    :gallery-bt false
    :credits nil}

   :gallery
   {:query   {:filter "all"
              :limit  20}
    :buttons {:edit     true
              :download true}}

   :about
   {:icon-size [64 64]
    :links-clickable? true}

   :thanks
   {:link-clickable? true
    :body "To view visit <a href=\"http://devartcodefactory.com\">devartcodefactory.com</a> and see if your piece has been selected to be shown in the online gallery."}

   :seed-select
   {:space 1.45
    :camz -3
    :camy 1
    :cam-up V3Y
    :cam-offset 0.05
    :rot-speed 2
    :scroll-speed 0.15
    :falloff [1.0 0.25 0.1875]
    :seed-order [:box :hex2 :pent2 :tri2 :hex3 :oct3 :pent3 :oct2]}

   :editor
   {:min-submit-depth 3
    :views-enabled? false
    :inset 11
    :gap 2
    :margin 10
    :margin-bottom 70
    :map-width 200
    :map-height 245
    :min-label-width 50
    :height 245
    :min-size 28
    :zoom-delta 75
    :map-bg "#222223"
    :map-selection "#ffff00"
    :map-color-offset 0
    :map-label-font "14px \"Abel\",sans-serif"
    :map-label-col "#999"
    :map-label-size 18
    :toolbar-speed 0.25
    :toolbar-icon-size [32 32]
    :toolbar-op-width 60
    :toolbar-sep-size [15 60]
    :toolbar-margin-left 55
    :toolbar-margin-right 55
    :tooltips {:edit-canvas {:offset (fn [el]
                                       [(- (/ (.-clientWidth el) 2) 125)
                                        (/ (.-clientHeight el) 2)])
                             :content "This is your 3D preview. Drag the shape to rotate. Use the touchpad or mouse wheel to zoom."}
               :toolbar-label {:offset [12 -96]
                               :intro-offset [12 -114]
                               :user? true
                               :content "These are the code blocks to modify your shape. Drag to scroll left/right to reveal more tools. The slider below varies the amount of change for some tools. Use \"Empty\" to remove shapes. If you made a mistake, use \"Undo\" to go back."}
               :viz-label     {:offset [12 -72]
                               :intro-offset [12 -90]
                               :user? true
                               :content "A visualization of your code. Each code block creates one or more new shapes. Click any of the boxes to select them for modification. Click again to deselect. Drag to scroll."}
               :map-label     {:offset [-262 -50]
                               :intro-offset [-262 -68]
                               :user? true
                               :content "A zoomed out map of your code structure. The yellow rectangle marks the code region visible on the left. Drag to scroll."}
               :axis-label    {:offset [-262 -18]
                               :auto? true
                               :content "Click here to display the XYZ axes for better orientation."}}
    :root-label "<h1>Tap here to begin</h1>"
    :ftu-label-sel "<span>Apply a code operation<br/>to modify</span>"
    :ftu-label "<span>Tap to select</span>"
    :leaf-label "<span>+</span>"
    :map-labels ["CODE OVERVIEW"]
    :intro [:edit-canvas :toolbar-label :viz-label :map-label]}

   :icons
   {:fullscreen {:paths [{:d "M0,0 L1,0 L1,1 L0,1 Z M0.2,0.4 L0.2,0.2 L0.4,0.2 M0.2,0.2 L0.4,0.4 M0.8,0.4 L0.8,0.2 L0.6,0.2 M0.8,0.2 L0.6,0.4 M0.8,0.6 L0.8,0.8 L0.6,0.8 M0.8,0.8 L0.6,0.6 M0.4,0.8 L0.2,0.8 L0.2,0.6 M0.2,0.8 L0.4,0.6"}]}
    :axis       {:paths [{:d "M0,0 L1,0 L1,1 L0,1 Z M0.5,0.2 L0.5,0.58 L0.2,0.8 M0.8,0.8 L0.5,0.58"}]}
    :zoom-in    {:paths [{:d "M0,0 L1,0 L1,1 L0,1 Z M0.2,0.5 L0.8,0.5 M0.5,0.2 L0.5,0.8"}]}
    :zoom-out   {:paths [{:d "M0,0 L1,0 L1,1 L0,1 Z M0.2,0.5 L0.8,0.5"}]}
    }

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
              :paths [{:d "M0,0 L1,0 L1,1 L0,1 Z"
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
    :leaf    {:col "#dddddd" :label "leaf"}
    :undo    {:col "#aaaaaa" :label "undo"
              :paths [{:d "M0,0 L1,0 L1,1 L0,1 Z M0.2,0.8 L0.2,0.2 L0.8,0.2 M0.2,0.2 L0.8,0.8"}]}}

   :op-presets
   [[:splitx2 {:label "split x2" :node (mg/subdiv :cols 2) :view (view-face :b)}]
    [:splity2 {:label "split y2" :node (mg/subdiv :rows 2) :view (view-face :b)}]
    [:splitz2 {:label "split z2" :node (mg/subdiv :slices 2) :view (view-face :w)}]
    [:splitx3 {:label "split x3" :node (mg/subdiv :cols 3) :view (view-face :b)}]
    [:splity3 {:label "split y3" :node (mg/subdiv :rows 3) :view (view-face :b)}]
    [:splitz3 {:label "split z3" :node (mg/subdiv :slices 3) :view (view-face :w)}]
    [:sep]
    [:insetx {:label "inset x" :node (mg/subdiv-inset :dir :x :inset 0.5) :view (view-face :w)}]
    [:insety {:label "inset y" :node (mg/subdiv-inset :dir :y :inset 0.5) :view (view-face :s)}]
    [:insetz {:label "inset z" :node (mg/subdiv-inset :dir :z :inset 0.5) :view (view-face :b)}]
    [:sep]
    [:mirrorw {:label "mirror left" :node (mg/reflect :dir :w) :view (view-face :b)}]
    [:mirrore {:label "mirror right" :node (mg/reflect :dir :e) :view (view-face :b)}]
    [:mirrorn {:label "mirror up" :node (mg/reflect :dir :n) :view (view-face :b)}]
    [:mirrors {:label "mirror down" :node (mg/reflect :dir :s) :view (view-face :b)}]
    [:mirrorf {:label "mirror front" :node (mg/reflect :dir :f) :view (view-face :w)}]
    [:mirrorb {:label "mirror back" :node (mg/reflect :dir :b) :view (view-face :w)}]
    [:sep]
    [:scalew {:label "scale left" :node (scale-op :w 0.5) :view (view-face :b)}]
    [:scalee {:label "scale right" :node (scale-op :e 0.5) :view (view-face :b)}]
    [:scalen {:label "scale up" :node (scale-op :n 0.5) :view (view-face :b)}]
    [:scales {:label "scale down" :node (scale-op :s 0.5) :view (view-face :b)}]
    [:scalef {:label "scale front" :node (scale-op :f 0.5) :view (view-face :w)}]
    [:scaleb {:label "scale back" :node (scale-op :b 0.5) :view (view-face :w)}]
    [:sep]
    [:stretchw {:label "stretch left" :node (mg/extrude-prop :dir :w :len 0.5) :view (view-face :b)}]
    [:stretche {:label "stretch right" :node (mg/extrude-prop :dir :e :len 0.5) :view (view-face :b)}]
    [:stretchn {:label "stretch up" :node (mg/extrude-prop :dir :n :len 0.5) :view (view-face :b)}]
    [:stretchs {:label "stretch down" :node (mg/extrude-prop :dir :s :len 0.5) :view (view-face :b)}]
    [:stretchf {:label "stretch front" :node (mg/extrude-prop :dir :f :len 0.5) :view (view-face :w)}]
    [:stretchb {:label "stretch back" :node (mg/extrude-prop :dir :b :len 0.5) :view (view-face :w)}]
    [:sep]
    [:tiltef {:label "tilt 1" :node (mg/skew :e :f :offset 0.5) :view (view-face :s)}]
    [:tiltwf {:label "tilt 2" :node (mg/skew :w :f :offset 0.5) :view (view-face :s)}]
    [:tiltnf {:label "tilt 3" :node (mg/skew :n :f :offset 0.5) :view (view-face :w)}]
    [:tiltsf {:label "tilt 4" :node (mg/skew :s :f :offset 0.5) :view (view-face :w)}]
    [:sep]
    [:shiftxy {:label "shift x/y" :node (mg/split-displace :x :y :offset 0.5) :view (view-face :b)}]
    [:shiftxz {:label "shift x/z" :node (mg/split-displace :x :z :offset 0.5) :view (view-face :s)}]
    [:shiftyx {:label "shift y/x" :node (mg/split-displace :y :x :offset 0.5) :view (view-face :b)}]
    [:shiftyz {:label "shift y/z" :node (mg/split-displace :y :z :offset 0.5) :view (view-face :w)}]
    [:shiftzx {:label "shift z/x" :node (mg/split-displace :z :x :offset 0.5) :view (view-face :s)}]
    [:shiftzy {:label "shift z/y" :node (mg/split-displace :z :y :offset 0.5) :view (view-face :w)}]
    ]

   :routes
   [{:match ["home"] :controller :home :hash "home"}
    {:match ["objects" :id]
     :bindings {:id {:validate [(v/uuid4)]}}
     :controller :object-loader}
    {:match ["objects" "edit" :seed-id]
     :bindings {:seed-id {:validate [(v/member-of (set (map name (keys seeds))))]}}
     :controller :editor}
    {:match ["select" :seed-id]
     :bindings {:seed-id {:validate [(v/member-of (set (map name (keys seeds))))]}}
     :controller :selector}
    {:match ["select"] :controller :selector}
    {:match ["objects" "submit"] :controller :submit-form}
    {:match ["thanks"] :controller :submit-confirm}
    {:match ["about"] :controller :about}
    {:match ["gallery"] :controller :gallery}]

   :routes-fallback
   [{:match ["not-supported"] :hash "not-supported" :controller :upgrade-browser}
    {:match ["gallery"] :controller :gallery}
    {:match ["about"] :controller :about}]

   :default-route 0
   :default-route-fallback 0

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
    [:about :home] -1
    [:home :about] 1
    [:login :home] -1
    }

   :dom-components
   {:home-continue    "home-continue"
    :home-gallery     "home-gallery"
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
    :submit-cancel    "submit-cancel"
    :thanks-msg       "thanks-body"
    :about-continue   "about-continue"
    :object-url-wrapper "art-url-wrapper"
    :object-url       "art-url"
    :object-error     "object-error"
    :object-loader    "object-load-progress"
    :gallery-main     "gallery-items"
    :gallery-prev     "gallery-prev"
    :gallery-next     "gallery-next"
    :nav-toggle       "nav-toggle"
    :nav-body         "nav-body"
    }

   :timeouts
   {:selector (* 24 60 60 1000)
    :editor   (* 24 60 60 1000)
    :thanks   (* 24 60 60 1000)
    :about    (* 24 60 60 1000)
    :controller 900
    :tooltip  3000}

   :api
   {:prefix api-prefix
    :routes
    (->> {:get-object "objects/"
          :submit-object "objects"
          :credits "jobs/current"
          :gallery "objects"}
         (reduce-kv
          (fn [acc k v] (assoc acc k (str api-prefix v)))
          {}))}
   })

(defn disable-routes
  [id]
  (fn [routes]
    (mapv #(if (= (:controller %) id) (assoc % :disabled true) %) routes)))

(defn enable-routes
  [id]
  (fn [routes]
    (mapv #(if (= (:controller %) id) (dissoc % :disabled) %) routes)))

(defn disable-module
  [config id]
  (-> config
      (assoc-in [:modules id] false)
      (update-in [:routes] (disable-routes id))
      (update-in [:routes-fallback] (disable-routes id))))

(defn enable-module
  [config id]
  (-> config
      (assoc-in [:modules id] true)
      (update-in [:routes] (enable-routes id))
      (update-in [:routes-fallback] (enable-routes id))))

(def ^:export app default-config)

(def ^:export maintenance
  (-> app
      (assoc :modules nil
             :routes [(get-in app [:routes 0])]
             :routes-fallback [(get-in app [:routes 0])])))

(def ^:export barbican
  (-> app
      (deep-merge
       {:gallery {:buttons {:download false}}

        :timeouts {:selector (* 30 1000)
                   :editor   (* 3 60 1000)
                   :thanks   (* 60 1000)
                   :about    (* 5 60 1000)}

        :api {:inject {:location "barbican"}}

        :about {:links-clickable? false}

        :thanks
        {:link-clickable? false
         :body "To view visit devartcodefactory.com and see if your piece has been selected to be shown in the online gallery."}

        :editor
        {:tooltips {:edit-canvas   {:content "This your 3D preview. Touch the shape to rotate. Pinch to zoom."}
                    :viz-label     {:content "A visualization of your code. Each code block creates one or more new shapes. Tap any of the boxes to select them for modification. Tap again to deselect. Drag to scroll."}
                    :axis-label    {:content "Tap here to display the XYZ axes for better orientation."}}}})
      (disable-module :gallery)))

(def ^:export workshop
  (-> app
      (deep-merge
       {:api {:inject {:location "workshop"}}
        :modules {:workshop true}})))

(def ^:export staging
  (-> app
      (deep-merge
       {:gallery {:buttons {:download true}}
        :about   {:links-clickable? true}
        :thanks  {:link-clickable? true}})
      (enable-module :gallery)))

(defn set-config!
  [sym] (set! app (js/eval (aget js/window sym))))

(defn apply-fallback
  [config]
  (->> {:gallery {:buttons {:edit false}}}
       (deep-merge config)
       (set! app)))

(defn operator
  [op] (-> app :operators op))

(defn operator-color
  [op] (-> app :operators op :col))

(defn preset-for-id
  [op]
  (some
   #(if (= op (first %)) (second %))
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

(defn module-enabled?
  [id] (-> app :modules id))
