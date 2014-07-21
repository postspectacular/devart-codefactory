(ns codefactory.operators
  (:require
   [codefactory.config :as config]
   [codefactory.tree :as tree]
   [thi.ng.cljs.async :as async]
   [thi.ng.cljs.dom :as dom]
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.utils :as utils]
   [thi.ng.common.math.core :as m :refer [HALF_PI]]
   [thi.ng.morphogen.core :as mg]
   [thi.ng.geom.core.vector :refer [V3X V3Y V3Z]]
   [thi.ng.geom.core.quaternion :as q]))

(defn init-op-separator
  [el [w h]]
  (let [svg (dom/create-ns!
             dom/svg-ns "svg" el
             {:width w :height h :viewBox "0 0 1 1"
              :preserveAspectRatio "none"})]
    (dom/create-ns! dom/svg-ns "path" svg {:d "M0.5,0 L0.5,1"})
    (dom/add-class! el "sep")
    [w]))

(defn init-op-button
  [el id node label [iconw iconh] width bus]
  (let [op     (config/translate-mg-op (:op node))
        svg    (dom/create-ns!
                dom/svg-ns "svg" el
                {:width iconw
                 :height iconh
                 :viewBox "-0.05 -0.05 1.1 1.1"
                 :preserveAspectRatio "none"})
        [spec] (dom/add-listeners
                [[el "click" (fn [] (async/publish bus :op-triggered id))]])]
    (dom/set-attribs!
     el {:id (name id) :class (str "op-" (name op) " tool")})
    (-> (dom/create! "div" el) (dom/set-text! label))
    (loop [paths (-> config/app :operators op :paths)]
      (when-let [p (first paths)]
        (-> (dom/create-ns! dom/svg-ns "path" svg {:d (:d p)})
            (dom/set-style! (clj->js (:style p))))
        (recur (next paths))))
    [width spec]))

(defn center-preset
  [bus id specs]
  (let [off (nth (specs id) 3)
        w (.-innerWidth js/window)
        x (- w off (/ w 2))]
    (debug :center id x)
    (async/publish bus :update-toolbar x)))

(defn highlight-selected-preset
  [id specs]
  (loop [specs specs]
    (if specs
      (let [[k [el]] (first specs)]
        ((if (= id k) dom/add-class! dom/remove-class!) el "selected")
        (recur (next specs))))))

(defn disable-presets
  [specs]
  (loop [specs specs]
    (if specs
      (let [[k [el]] (first specs)]
        (dom/remove-class! el "selected")
        (dom/add-class! el "disabled")
        (recur (next specs))))))

(defn enable-presets
  [specs]
  (loop [specs specs]
    (if specs
      (let [[k [el]] (first specs)]
        (dom/remove-class! el "disabled")
        (recur (next specs))))))

(defn init-op-triggers
  [bus tools]
  (let [{icon-size :toolbar-icon-size
         op-width  :toolbar-op-width
         sep-size  :toolbar-sep-size
         offset    :toolbar-margin-left} (:editor config/app)
        [width specs] (reduce
                       (fn [[total specs] [id {:keys [label node]}]]
                         (let [el (dom/create! "div" tools)
                               [w spec] (if (= :sep id)
                                          (init-op-separator el sep-size)
                                          (init-op-button
                                           el id node label
                                           icon-size op-width bus))
                               total' (+ total w)]
                           (if spec
                             [total' (assoc specs id (conj spec total))]
                             [total' specs])))
                       [0 {}] (:op-presets config/app))]
    (dom/set-style! tools #js {:width width})
    (disable-presets specs)
    {:width width :offset offset :specs specs}))

(defn remove-op-triggers
  [bus coll]
  (loop [coll coll]
    (if coll
      (let [[el f] (first coll)]
        (.removeEventListener el "click" f)
        (recur (next coll))))))

(defn init-op-slider
  [editor bus path op
   {:keys [label min max value step listener format] :as spec}]
  (when spec
    (let [cls      (str "op-" (name op))
          parent   (config/dom-component :slider)
          wrapper  (-> (config/dom-component :slider-wrapper) (dom/set-html! ""))
          slider   (dom/create!
                    "input" wrapper
                    {:id "slider-val" :type "range" :class cls
                     :min min :max max :value value :step step})
          val      (config/dom-component :slider-val-label)
          set-val! (fn [x] (dom/set-text! val (format x)))]
      (dom/remove-class! parent "disabled")
      (dom/add-class! parent cls)
      (dom/set-text! (config/dom-component :slider-label) label)
      (set-val! value)
      (dom/add-listeners
       [[slider "change"
         (fn []
           (let [n (utils/parse-float (.-value slider))]
             (swap! editor assoc-in (cons :tree path)
                    (listener n (get-in (:tree @editor) (conj path :args))))
             (set-val! n)
             (debug :tree (:tree @editor))
             (swap! editor tree/update-meshes false)
             (async/publish bus :render-scene nil)))]]))))

(defn show-op-controls
  [{:keys [editor local bus node slider op orig]}]
  (let [{:keys [tree selection]} @editor
        {:keys [viz canvas]} @local
        path (mg/child-path selection)
        mg-op (config/op-aliases op)
        listeners (init-op-slider editor bus path op slider)]
    (swap!
     editor assoc
     :sel-type op
     :tree (if (seq path) (assoc-in tree path node) node))
    (swap!
     local merge
     {:orig-edit-node orig
      :ctrl-active? true
      :ctrl-listeners listeners})
    (debug :tree (:tree @editor))
    (swap! editor tree/update-meshes true)
    (async/publish bus :render-scene nil)))

(defn release-op-controls
  [local]
  (let [{:keys [viz canvas ctrl-active? ctrl-listeners]} @local]
    (debug :release-op ctrl-active?)
    (when ctrl-active?
      (swap! local assoc :ctrl-active? false :ctrl-listeners nil)
      (dom/remove-listeners ctrl-listeners)
      (dom/set-attribs! (config/dom-component :slider) {:class "disabled"})
      (dom/set-attribs!
       (config/dom-component* :slider-range)
       {:class "" :disabled true})
      (dom/set-text! (config/dom-component :slider-label) "")
      (dom/set-text! (config/dom-component :slider-val-label) ""))))

(defrecord SliderSpec [label min max value step listener format])

(def float-label (utils/float-formatter 3))

(def int-label str)

(def face-label tree/face-labels)

(def direction-label tree/direction-labels)

(defn same-op?
  [op orig] (= (config/op-aliases op) (:op orig)))

(defmulti handle-operator
  (fn [op preset orig editor local bus] op))

(defmethod handle-operator :default
  [op & _] (warn :not-implemented op))

(defmethod handle-operator :delete
  [_ preset orig editor local bus]
  (let [{:keys [tree selection]} @editor
        tree (tree/delete-node-at tree selection)]
    (reset! editor
            (-> @editor
                (assoc :tree tree)
                (tree/update-meshes true)
                (assoc :selection selection
                       :sel-type (tree/node-operator (tree/node-at tree selection)))))
    (async/publish bus :regenerate-scene nil)))

(defmethod handle-operator :sd
  [op preset orig editor local bus]
  (let [node (if (and (same-op? op orig)
                      (== (apply * (vals (:args orig)))
                          (apply * (vals (:args preset)))))
               (assoc preset :out (:out orig))
               preset)]
    (debug :node node)
    (show-op-controls
     {:editor editor
      :local local
      :bus bus
      :op op
      :node node
      :orig orig})))

;; TODO add cam alignment
(defmethod handle-operator :inset
  [op preset orig editor local bus]
  (let [{:keys [node-cache selection]} @editor
        min-len   (tree/node-shortest-edge (node-cache selection))
        min-inset (* 0.025 min-len)
        max-inset (* 0.45 min-len)
        step      (* 0.025 min-len)
        preset    (assoc-in preset [:args :inset] (m/mix min-inset max-inset 0.5))
        inset     (m/clamp (:inset (tree/op-args-or-default op orig preset))
                           min-inset max-inset)
        node      (if (same-op? op orig)
                    (assoc preset :out (:out orig))
                    preset)]
    (debug :path selection :node node)
    (show-op-controls
     {:editor editor
      :local local
      :bus bus
      :op op
      :node node
      :orig orig
      :slider (->SliderSpec
               "inset" min-inset max-inset inset step
               (fn [n _] (assoc-in node [:args :inset] n))
               float-label)})))

(defmethod handle-operator :reflect
  [op preset orig editor local bus]
  (let [node (if (same-op? op orig)
               (assoc preset :out (:out orig))
               preset)]
    (debug :node node)
    (show-op-controls
     {:editor editor
      :local local
      :bus bus
      :op op
      :node node
      :orig orig})))

;; TODO constrain length
(defmethod handle-operator :stretch
  [op preset orig editor local bus]
  (let [node (if (same-op? op orig)
               (assoc preset :out (:out orig))
               preset)
        len  (:len (tree/op-args-or-default op orig node))]
    (debug :node node)
    (show-op-controls
     {:editor editor
      :local local
      :bus bus
      :op op
      :node node
      :orig orig
      :slider (->SliderSpec
               "length" 0.02 2.0 len 0.001
               (fn [n _] (assoc-in node [:args :len] n))
               float-label)})))

(defmethod handle-operator :shift
  [op preset orig editor local bus]
  (let [{:keys [tree selection]} @editor
        node (mg/split-displace :x :z :offset 0.1)
        {:keys [dir ref offset] :as args} (tree/op-args-or-default op orig node)
        children (if (same-op? op orig) (:out orig))]
    (debug :path selection :args args :node node)
    (show-op-controls
     {:editor editor
      :local local
      :bus bus
      :op op
      :slider (->SliderSpec
               "shift length" 0.0 1.0 offset 0.001
               (fn [n {:keys [dir ref]}]
                 (mg/split-displace dir ref :offset n :out children))
               float-label)
      :node node
      :orig orig})))

(defmethod handle-operator :tilt
  [op preset orig editor local bus]
  (let [{:keys [tree selection]} @editor
        node preset
        {:keys [side ref offset] :as args} (tree/op-args-or-default op orig node)
        children (if (same-op? op orig) (:out orig))]
    (debug :path selection :args args :node node)
    (show-op-controls
     {:editor editor
      :local local
      :bus bus
      :op op
      :slider (->SliderSpec
               "offset" 0.0 2.0 offset 0.001
               (fn [n {:keys [side ref]}]
                 (mg/skew side ref :offset n :out children))
               float-label)
      :node node
      :orig orig})))

(defmethod handle-operator :scale
  [op preset orig editor local bus]
  (let [{:keys [tree selection]} @editor
        ctor (fn [side scale out]
               {:op :scale-side
                :args {:side side :scale scale}
                :out (mg/operator-output 1 out false)})
        node preset
        {:keys [side scale] :as args} (tree/op-args-or-default op orig node)
        children (if (same-op? op orig) (:out orig))]
    (debug :path selection :args args :node node)
    (show-op-controls
     {:editor editor
      :local local
      :bus bus
      :op op
      :node node
      :orig orig
      :sliders (->SliderSpec
                "scale" 0.1 2.0 scale 0.001
                (fn [n {:keys [side]}] (ctor side n children))
                float-label)})))
