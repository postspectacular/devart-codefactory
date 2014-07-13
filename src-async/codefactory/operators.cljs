(ns codefactory.operators
  (:require
   [codefactory.config :as config]
   [codefactory.tree :as tree]
   [thi.ng.cljs.async :as async]
   [thi.ng.cljs.dom :as dom]
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.utils :as utils]
   [thi.ng.common.math.core :as m]
   [thi.ng.morphogen.core :as mg]))

(defn init-op-triggers
  [bus config]
  (mapv
   (fn [op]
     (let [el (dom/by-id (str "op-" (name op)))
           f (fn [e] (.preventDefault e) (async/publish bus :op-triggered op))]
       (.addEventListener el "click" f)
       [el f]))
   (keys (dissoc (:operators config) :leaf))))

(defn remove-op-triggers
  [bus coll]
  (loop [coll coll]
    (if coll
      (let [[el f] (first coll)]
        (.removeEventListener el "click" f)
        (recur (next coll))))))

(defn init-op-slider
  [editor bus path i op {:keys [label min max value step listener]}]
  (let [el-id    (str "ctrl" i)
        cls      (str "op-" (name op))
        parent   (dom/by-id "op-sliders")
        el-label (dom/create! "p" parent {:id (str el-id "-label")})
        el       (dom/create!
                  "input" parent
                  {:id el-id :type "range" :class cls
                   :min min :max max :value value :step step})]
    (dom/set-text! el-label label)
    [el "change"
     (fn [e]
       (let [n (utils/parse-float (.-value el))]
         (swap! editor assoc-in (cons :tree path)
                (listener n (get-in (:tree @editor) (conj path :args))))
         (debug :tree (:tree @editor))
         (swap! editor tree/update-meshes false)
         (async/publish bus :render-scene nil)))]))

(defn init-op-controls
  [editor bus path op specs config]
  (let [op-col (config/operator-color config op)]
    (dom/set-style! (dom/by-id "ctrl-ok-path") #js {:fill op-col})
    (dom/set-style! (dom/by-id "ctrl-cancel-path") #js {:stroke op-col})
    (->> specs
         (map-indexed
          (fn [i spec] (init-op-slider editor bus path i op spec)))
         vec)))

(defn show-op-controls
  [{:keys [editor local bus default sliders op orig]}]
  (let [{:keys [tree selection]} @editor
        {:keys [viz canvas config]} @local
        path (mg/child-path selection)
        node (if (= op (:op orig)) orig default)
        ctrl (dom/by-id "op-ctrl")
        listeners (init-op-controls editor bus path op sliders config)
        listeners (conj listeners
                        ["#ctrl-ok" "click"
                         (fn [e]
                           (.preventDefault e)
                           (async/publish bus :commit-operator nil))]
                        ["#ctrl-cancel" "click"
                         (fn [e]
                           (.preventDefault e)
                           (async/publish bus :cancel-operator nil))])]
    (dom/add-listeners listeners)
    (dom/add-class! viz "hidden")
    (dom/add-class! canvas "hidden")
    (dom/set-attribs! ctrl {:class (str "op-" (name op))})
    (swap!
     editor merge
     {:sel-type op
      :tree (if (seq path) (assoc-in tree path node) node)})
    (swap!
     local merge
     {:orig-tree-value orig
      :ctrl-active? true
      :ctrl-listeners listeners})
    (debug :tree (:tree @editor))
    (swap! editor tree/update-meshes true)
    (async/publish bus :render-scene nil)))

(defrecord SliderSpec [label min max val step listener])

(defn slider-specs
  [& specs] (mapv #(apply ->SliderSpec %) specs))

(defn remove-op-controls
  [local]
  (let [{:keys [viz canvas ctrl-active? ctrl-listeners]} @local]
    (when ctrl-active?
      (swap! local assoc :ctrl-active? false :ctrl-listeners nil)
      (dom/remove-listeners ctrl-listeners)
      (dom/add-class! (dom/by-id "op-ctrl") "hidden")
      (dom/set-html! (dom/by-id "op-sliders") "")
      (dom/remove-class! viz "hidden")
      (dom/remove-class! canvas "hidden"))))

(defmulti handle-operator (fn [op editor local bus] op))

(defmethod handle-operator :default
  [op & _] (warn :not-implemented op))

(defmethod handle-operator :delete
  [_ editor local bus]
  (let [{:keys [tree selection]} @editor
        tree (tree/delete-node-at tree selection)]
    (reset! editor
            (-> @editor
                (assoc :tree tree :selection nil)
                (tree/update-meshes true)))
    (swap! local assoc :selected-id nil)
    (async/publish bus :regenerate-scene nil)))

(defmethod handle-operator :sd
  [op editor local bus]
  (let [{:keys [tree selection]} @editor
        orig (tree/node-at tree selection)
        default (mg/subdiv)
        {:keys [cols rows slices] :as args} (tree/op-args-or-default op orig default)]
    (debug :path selection :args args :default default)
    (show-op-controls
     {:editor editor
      :local local
      :bus bus
      :op op
      :sliders (slider-specs
                ["columns" 1 5 cols 1
                 (fn [n {:keys [rows slices]}]
                   (mg/subdiv :cols (int n) :rows rows :slices slices))]
                ["rows" 1 5 rows 1
                 (fn [n {:keys [cols slices]}]
                   (mg/subdiv :cols cols :rows (int n) :slices slices))]
                ["slices" 1 5 slices 1
                 (fn [n {:keys [rows cols]}]
                   (mg/subdiv :cols cols :rows rows :slices (int n)))])
      :default default
      :orig orig})))

(defmethod handle-operator :sd-inset
  [op editor local bus]
  (let [{:keys [tree selection]} @editor
        orig    (tree/node-at tree selection)
        default (mg/subdiv-inset :dir :x :inset 0.05)
        {:keys [dir inset] :as args} (tree/op-args-or-default op orig default)]
    (debug :path selection :args args :default default)
    (show-op-controls
     {:editor editor
      :local local
      :bus bus
      :op op
      :sliders (slider-specs
                ["direction" 0 2 (tree/direction-idx dir) 1
                 (fn [n {:keys [inset]}]
                   (mg/subdiv-inset
                    :dir (tree/direction-ids (int n)) :inset inset))]
                ["inset" 0.02 0.45 inset 0.001
                 (fn [n {:keys [dir]}]
                   (mg/subdiv-inset :dir dir :inset n))])
      :default default
      :orig orig})))

(defmethod handle-operator :reflect
  [op editor local bus]
  (let [{:keys [tree selection]} @editor
        orig (tree/node-at tree selection)
        default (mg/reflect :dir :e)
        {:keys [dir] :as args} (tree/op-args-or-default op orig default)]
    (debug :path selection :args args :default default)
    (show-op-controls
     {:editor editor
      :local local
      :bus bus
      :op op
      :sliders (slider-specs
                ["direction" 0 5 (tree/face-idx dir) 1
                 (fn [n _] (mg/reflect :dir (tree/face-ids (int n))))])
      :default default
      :orig orig})))

(defmethod handle-operator :extrude
  [op editor local bus]
  (let [{:keys [tree selection]} @editor
        orig (tree/node-at tree selection)
        default (mg/extrude :dir :e :len 1.0)
        {:keys [dir len] :as args} (tree/op-args-or-default op orig default)]
    (debug :path selection :args args :default default)
    (show-op-controls
     {:editor editor
      :local local
      :bus bus
      :op op
      :sliders (slider-specs
                ["direction" 0 5 (tree/face-idx dir) 1
                 (fn [n {:keys [len]}]
                   (mg/extrude :dir (tree/face-ids (int n)) :len len))]
                ["length" 0.02 2.0 len 0.001
                 (fn [n {:keys [dir]}] (mg/extrude :dir dir :len n))])
      :default default
      :orig orig})))

(defmethod handle-operator :split-displace
  [op editor local bus]
  (let [{:keys [tree selection]} @editor
        orig (tree/node-at tree selection)
        default (mg/split-displace :x :z :offset 0.1)
        {:keys [dir ref offset] :as args} (tree/op-args-or-default op orig default)]
    (debug :path selection :args args :default default)
    (show-op-controls
     {:editor editor
      :local local
      :bus bus
      :op op
      :sliders (slider-specs
                ["split direction" 0 3 (tree/direction-idx dir) 1
                 (fn [n {:keys [ref offset]}]
                   (mg/split-displace
                    (tree/direction-ids (int n)) ref :offset offset))]
                ["shift direction" 0 3 (tree/direction-idx dir) 1
                 (fn [n {:keys [dir offset]}]
                   (mg/split-displace
                    dir (tree/direction-ids (int n)) :offset offset))]
                ["shift length" 0.0 2.0 offset 0.001
                 (fn [n {:keys [dir ref]}]
                   (mg/split-displace dir ref :offset n))])
      :default default
      :orig orig})))

(defmethod handle-operator :skew
  [op editor local bus]
  (let [{:keys [tree selection]} @editor
        orig (tree/node-at tree selection)
        default (mg/reflect :dir :e)
        {:keys [dir] :as args} (tree/op-args-or-default op orig default)]
    (debug :path selection :args args :default default)
    (show-op-controls
     {:editor editor
      :local local
      :bus bus
      :op op
      :sliders (slider-specs
                ["direction" 0 5 (tree/face-idx dir) 1
                 (fn [n _] (mg/reflect :dir (tree/face-ids (int n))))])
      :default default
      :orig orig})))
