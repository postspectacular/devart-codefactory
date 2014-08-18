(ns codefactory.editor.operators
  (:require-macros
   [thi.ng.macromath.core :as mm])
  (:require
   [codefactory.editor.tree :as tree]
   [codefactory.config :as config]
   [codefactory.common :as common]
   [thi.ng.cljs.async :as async]
   [thi.ng.cljs.dom :as dom]
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.utils :as utils]
   [thi.ng.common.math.core :as m :refer [HALF_PI]]
   [thi.ng.morphogen.core :as mg]
   [thi.ng.geom.core.vector :refer [V3X V3Y V3Z]]))

(defn op-class
  [op] (str "op-" (name op)))

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
    ;;(debug :tree (:tree @editor))
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
  [op orig]
  ;;(debug :same-op (= (config/op-aliases op) (:op orig)) op (dissoc orig :out))
  (= (config/op-aliases op) (:op orig)))

(defn inject-orig-args
  [node preset & keys]
  (-> preset
      (update-in [:args] merge (select-keys (:args node) keys))
      (assoc :out (:out node))))

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
        node      (if (same-op? op orig)
                    (inject-orig-args orig preset :inset)
                    preset)
        inset     (m/clamp (-> node :args :inset) min-inset max-inset)]
    (debug :node node)
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
               (inject-orig-args orig preset :len)
               preset)]
    (debug :node node)
    (show-op-controls
     {:editor editor
      :local local
      :bus bus
      :op op
      :node node
      :orig orig
      :slider (->SliderSpec
               "length" 0.0 2.0 (-> node :args :len) 0.001
               (fn [n _] (assoc-in node [:args :len] n))
               float-label)})))

(defmethod handle-operator :shift
  [op preset orig editor local bus]
  (let [node (if (same-op? op orig)
               (inject-orig-args orig preset :offset)
               preset)]
    (debug :node node)
    (show-op-controls
     {:editor editor
      :local local
      :bus bus
      :op op
      :slider (->SliderSpec
               "shift length" -1.0 1.0 (-> node :args :offset) 0.001
               (fn [n _] (assoc-in node [:args :offset] n))
               float-label)
      :node node
      :orig orig})))

(defmethod handle-operator :tilt
  [op preset orig editor local bus]
  (let [{:keys [tree selection]} @editor
        node (if (same-op? op orig)
               (inject-orig-args orig preset :offset)
               preset)]
    (debug :node node)
    (show-op-controls
     {:editor editor
      :local local
      :bus bus
      :op op
      :slider (->SliderSpec
               "offset" -1.0 1.0 (-> node :args :offset) 0.001
               (fn [n _] (assoc-in node [:args :offset] n))
               float-label)
      :node node
      :orig orig})))

(defmethod handle-operator :scale
  [op preset orig editor local bus]
  (let [node (if (same-op? op orig)
               (inject-orig-args orig preset :scale)
               preset)]
    (debug :node node)
    (show-op-controls
     {:editor editor
      :local local
      :bus bus
      :op op
      :node node
      :orig orig
      :slider (->SliderSpec
               "scale" 0.1 2.0 (-> node :args :scale) 0.001
               (fn [n _] (assoc-in node [:args :scale] n))
               float-label)})))
