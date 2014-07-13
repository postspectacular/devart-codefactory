(ns codefactory.treedit
  (:require-macros
   [cljs.core.async.macros :as asm :refer [go]]
   [thi.ng.macromath.core :as mm])
  (:require
   [cljs.core.async :as cas :refer [>! <! alts! chan put! close! timeout]]
   [codefactory.config :as config]
   [codefactory.color :as col]
   [codefactory.tree :as tree]
   [codefactory.shared :as shared]
   [thi.ng.cljs.async :as async]
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.utils :as utils]
   [thi.ng.cljs.dom :as dom]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.core.matrix :as mat :refer [M44]]
   [thi.ng.geom.core.vector :as v :refer [vec2 vec3]]
   [thi.ng.geom.rect :as r]
   [thi.ng.common.math.core :as m]
   [thi.ng.morphogen.core :as mg]))

(def svg-ns "http://www.w3.org/2000/svg")

(defn ->px [x] (str x "px"))

(def node-id
  (memoize (fn [path] (apply str (cons "node-" (interpose "-" path))))))

(defn remove-node-event-handlers
  [bus nodes]
  (->> nodes
       vals
       (map
        (fn [{:keys [el handler channel]}]
          (.removeEventListener el "click" handler)
          (async/unsubscribe bus :node-toggle channel)
          (close! channel)))
       dorun))

(defn node-event-handler
  [ch bus id]
  (go
    (loop []
      (when (<! ch)
        (async/publish bus :node-toggle id)
        (recur)))))

(defn make-node
  [parent path op x y w h bus sel config]
  (let [el (dom/create! "div" parent)
        id (node-id path)
        cls (str "op-" (name op))
        [ch handler] (dom/event-channel el "click")]

    (cond
     (= :leaf op)
     (dom/set-html! el (if (empty? path)
                         (get-in config [:editor :root-label])
                         "+"))
     (= :delete op)
     (let [svg (dom/create-ns! svg-ns "svg" el
                               {:width "100%" :height "100%"
                                :viewBox "0 0 1 1"
                                :preserveAspectRatio "none"
                                :class "op-delete"})]
       (dom/create-ns! svg-ns "path" svg {:d "M0,0 L1,1 M0,1 L1,0"})
       (dom/create-ns! svg-ns "rect" svg {:x 0 :y 0 :width 1 :height 1}))
     :else nil)

    (doto el
      (dom/set-attribs! {:id id})
      (dom/set-style!
       #js {:left   (->px x)
            :top    (->px y)
            :width  (->px w)
            :height (->px h)}))

    (if (not= :delete op) (dom/add-class! el cls))
    (if (= path sel) (dom/add-class! el "selected"))

    (node-event-handler ch bus id)

    [id {:el el :path path :channel ch :handler handler}]))

(defn cell-size
  [total gap num]
  (if (pos? num)
    (/ (- total (* (dec num) gap)) num)
    total))

(defn cell-size-at
  [tree path width gap]
  (reduce
   (fn [width n]
     (->> path
          (take n)
          (tree/num-children-at tree)
          (cell-size width gap)))
   width (range (inc (count path)))))

(defn generate-branch
  [bus viz tree scroll sel config]
  (let [{:keys [gap height margin-bottom]} (:editor config)
        off (g/+ scroll
                 (.-offsetLeft viz)
                 (- (.-innerHeight js/window) (+ height margin-bottom)))]
    (fn gen-branch*
      [acc path x y w h]
      (let [node (tree/node-at tree path)
            nc (count (:out node))
            wc (cell-size w gap nc)
            cy (- y h gap)
            op (tree/node-operator node)
            acc (conj acc
                      (make-node viz path op
                                 (+ x (:x off)) (+ (- y h) (:y off)) w h
                                 bus sel config))]
        (if (pos? nc)
          (loop [acc acc, i 0]
            (if (< i nc)
              (recur (gen-branch* acc (conj path i) (mm/madd i wc i gap x) cy wc h)
                     (inc i))
              acc))
          acc)))))

(defn compute-viewport
  [local]
  (let [{:keys [scroll config]} @local
        {:keys [margin map-width map-height height]} (:editor config)]
    [(- (:x scroll))
     (:y scroll)
     (mm/madd margin -3 (- (.-innerWidth js/window) map-width))
     height]))

(defn map-focus-rect
  [[x y w h] viz-width viz-height map-width map-height]
  (let [x (m/map-interval x 0 viz-width 1 (- map-width 2))
        y (m/map-interval y 0 viz-height 1 (- map-height 2))
        w (m/map-interval w 0 viz-width 1 (- map-width 2))
        h (m/map-interval h 0 viz-height 1 (- map-height 2))]
    [x y w h]))

(defn compute-required-width
  [editor]
  (let [{:keys [max-nodes-path tree config]} @editor
        {:keys [gap margin min-size map-width]} (:editor config)
        width (mm/madd margin -3 (- (.-innerWidth js/window) map-width))
        cw (cell-size-at tree max-nodes-path width gap)]
    (if (< cw min-size)
      (loop [path max-nodes-path
             num-sibs (tree/num-children-at tree max-nodes-path)
             width min-size]
        (let [width (mm/madd width num-sibs gap (dec num-sibs))]
          (if (seq path)
            (let [path' (pop path)]
              (recur path' (tree/num-children-at tree path') width))
            width)))
      width)))

(defn regenerate-viz
  [editor local bus]
  (tree/update-stats editor)
  (let [{:keys [viz nodes width scroll config]} @local
        {:keys [tree tree-depth selection]} @editor
        {:keys [gap margin height]} (:editor config)
        width' (compute-required-width editor)
        node-height (cell-size height gap tree-depth)
        scroll (if (== width width') scroll (vec2))
        layout (generate-branch bus viz tree scroll selection config)]
    (dom/set-html! viz "")
    (remove-node-event-handlers bus nodes)
    (swap!
     local assoc
     :width       width'
     :node-height node-height
     :scroll      scroll
     :nodes       (layout {} [] margin height width' node-height))))

(defn resize-branch
  [nodes tree gap offset]
  (fn resize-branch*
    [path x w]
    (let [el (:el (nodes (node-id path)))
          nc (tree/num-children-at tree path)
          wc (cell-size w gap nc)]
      (dom/set-style! el #js {:left (->px (+ x (:x offset))) :width (->px w)})
      (if (pos? nc)
        (loop [i 0]
          (when (< i nc)
            (resize-branch* (conj path i) (mm/madd i wc i gap x) wc)
            (recur (inc i))))))))

(defn resize-viz
  [editor local]
  (let [{:keys [viz nodes scroll config]} @local
        {:keys [node-cache tree tree-depth]} @editor
        {:keys [gap margin height]} (:editor config)
        width  (compute-required-width editor)
        offset (g/+ scroll (.-offsetLeft viz) (- (.-innerHeight js/window) 270))]
    ((resize-branch nodes tree gap offset) [] margin width)
    (swap! local assoc :width width)))

(def map-op-color
  (memoize
   (fn [config op]
     (col/gray-offset-hex
      (config/operator-color config op)
      (get-in config [:editor :map-color-offset])))))

(defn map-branch
  [ctx tree path x y w h sel config]
  (let [{:keys [op out] :as node} (tree/node-at tree path)
        nc (count out)
        wc (cell-size w 1 nc)
        col (if (= path sel)
              (get-in config [:editor :map-selection])
              (map-op-color config (tree/node-operator node)))]
    (set! (.-fillStyle ctx) col)
    (.fillRect ctx x (inc (- y h)) w h)
    (if (pos? nc)
      (loop [i 0]
        (when (< i nc)
          (map-branch ctx tree (conj path i) (mm/madd i wc i 1 x) (- y h) wc h sel config)
          (recur (inc i)))))))

(defn draw-map-labels
  [ctx labels cx cy col font leading]
  (let [num (count labels)]
    (set! (.-fillStyle ctx) col)
    (set! (.-font ctx) font)
    (set! (.-textAlign ctx) "center")
    (set! (.-textBaseline ctx) "middle")
    (loop [i 0, y (int (/ num -2))]
      (when (< i num)
        (.fillText ctx (labels i) cx (mm/madd leading y cy))
        (recur (inc i) (inc y))))))

(defn regenerate-map
  [editor local]
  (let [{:keys [ctx width height config]} @local
        {:keys [tree tree-depth selection]} @editor
        {:keys [map-bg map-width map-height] :as econf} (:editor config)
        [vx vy vw vh :as vp] (map-focus-rect
                              (compute-viewport local)
                              width height map-width map-height)]
    (swap! local assoc :viewport vp)
    (set! (.-fillStyle ctx) map-bg)
    (set! (.-strokeStyle ctx) nil)
    (.fillRect ctx 0 0 map-width map-height)
    (map-branch ctx tree [] 0 map-height
                map-width (/ map-height tree-depth)
                selection config)
    (when (== 1 tree-depth)
      (let [{:keys [map-labels map-label-font map-label-size]} econf]
        (draw-map-labels ctx map-labels
                         (/ map-width 2) (/ map-height 2)
                         map-bg map-label-font map-label-size)))
    (set! (.-strokeStyle ctx) "yellow")
    (set! (.-lineWidth ctx) 2)
    (.strokeRect ctx vx vy vw vh)
    ))

(defn scroll-viewport
  [editor local x]
  (let [{:keys [width viewport config]} @local
        {:keys [map-width map-height]} (:editor config)
        [_ _ viz-width] (compute-viewport local)
        [_ _ vw] viewport
        sx (m/map-interval-clamped (mm/madd vw -0.5 x) 0 (- map-width vw) 0 (- (- width viz-width)))]
    (swap! local assoc-in [:scroll :x] sx)
    (resize-viz editor local)
    (regenerate-map editor local)))

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
  [editor bus path i op
   {:keys [label min max value step listener]
    :or {step 1}}]
  (let [el-id (str "ctrl" i)
        cls (str "op-" (name op))
        parent (dom/by-id "op-sliders")
        el-label (dom/create! "p" parent {:id (str el-id "-label")})
        el (dom/create! "input" parent
                        {:id el-id :type "range" :class cls
                         :min min :max max :value value :step step})]
    (dom/set-text! el-label label)
    [el "change"
     (fn [e]
       (let [n (utils/parse-float (.-value el))]
         (swap!
          editor
          assoc-in (cons :tree path)
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
          (fn [i spec]
            (init-op-slider editor bus path i op spec)))
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
  [op & _]
  (warn :not-implemented op))

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
      :sliders [{:label "columns" :min 1 :max 5 :value cols :step 1
                 :listener (fn [n {:keys [rows slices]}] (mg/subdiv :cols (int n) :rows rows :slices slices))}
                {:label "rows" :min 1 :max 5 :value rows :step 1
                 :listener (fn [n {:keys [cols slices]}] (mg/subdiv :cols cols :rows (int n) :slices slices))}
                {:label "slices" :min 1 :max 5 :value slices :step 1
                 :listener (fn [n {:keys [rows cols]}] (mg/subdiv :cols cols :rows rows :slices (int n)))}]
      :default default
      :orig orig})))

(defmethod handle-operator :sd-inset
  [op editor local bus]
  (let [{:keys [tree selection]} @editor
        orig (tree/node-at tree selection)
        default (mg/subdiv-inset :dir :x :inset 0.05)
        {:keys [dir inset] :as args} (tree/op-args-or-default op orig default)]
    (debug :path selection :args args :default default)
    (show-op-controls
     {:editor editor
      :local local
      :bus bus
      :op op
      :sliders [{:label "direction" :min 0 :max 2 :value (tree/direction-idx dir) :step 1
                 :listener (fn [n {:keys [inset]}] (mg/subdiv-inset :dir (tree/direction-ids (int n)) :inset inset))}
                {:label "inset" :min 0.02 :max 0.45 :value inset :step 0.001
                 :listener (fn [n {:keys [dir]}] (mg/subdiv-inset :dir dir :inset n))}]
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
      :sliders [{:label "direction" :min 0 :max 5 :value (tree/face-idx dir) :step 1
                 :listener (fn [n _] (mg/reflect :dir (tree/face-ids (int n))))}]
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
      :sliders [{:label "direction" :min 0 :max 5 :value (tree/face-idx dir) :step 1
                 :listener (fn [n {:keys [len]}] (mg/extrude :dir (tree/face-ids (int n)) :len len))}
                {:label "length" :min 0.02 :max 2.0 :value len :step 0.001
                 :listener (fn [n {:keys [dir]}] (mg/extrude :dir dir :len n))}]
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
      :sliders [{:label "split direction" :min 0 :max 3 :value (tree/direction-idx dir) :step 1
                 :listener (fn [n {:keys [ref offset]}] (mg/split-displace (tree/direction-ids (int n)) ref :offset offset))}
                {:label "shift direction" :min 0 :max 3 :value (tree/direction-idx dir) :step 1
                 :listener (fn [n {:keys [dir offset]}] (mg/split-displace dir (tree/direction-ids (int n)) :offset offset))}
                {:label "shift length" :min 0.0 :max 2.0 :value offset :step 0.001
                 :listener (fn [n {:keys [dir ref]}] (mg/split-displace dir ref :offset n))}]
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
      :sliders [{:label "direction" :min 0 :max 5 :value (tree/face-idx dir) :step 1
                 :listener (fn [n _] (mg/reflect :dir (tree/face-ids (int n))))}]
      :default default
      :orig orig})))

(defn handle-resize
  [ch bus editor local]
  (go
    (loop []
      (when (<! ch)
        (resize-viz editor local)
        (regenerate-map editor local)
        (async/publish bus :user-action nil)
        (recur)))))

(defn handle-regen
  [ch bus editor local]
  (go
    (loop []
      (when (<! ch)
        (regenerate-viz editor local bus)
        (regenerate-map editor local)
        (async/publish bus :render-scene nil)
        (recur)))))

(defn handle-node-toggle
  [ch bus local]
  (go
    (loop []
      (let [[_ id] (<! ch)
            sel (:selected-id @local)]
        (when id
          (if (= id sel)
            (async/publish bus :node-deselected [id true])
            (do
              (when sel (async/publish bus :node-deselected [sel false]))
              (async/publish bus :node-selected id)))
          (async/publish bus :user-action nil)
          (recur))))))

(defn handle-node-selected
  [ch bus editor local]
  (let [toolbar (dom/by-id "toolbar")]
    (go
      (loop []
        (let [[_ id] (<! ch)
              {:keys [el path]} (get-in @local [:nodes id])
              {:keys [tree meshes]} @editor]
          (when id
            (swap! local assoc :selected-id id)
            (swap!
             editor assoc
             :selection path
             :sel-type (tree/node-operator (tree/node-at tree path))
             :display-meshes (tree/filter-leaves-and-selection meshes tree path))
            (dom/add-class! el "selected")
            (dom/add-class! toolbar "rollon")
            (async/publish bus :render-scene nil)
            (regenerate-map editor local)
            (recur)))))))

(defn handle-node-deselected
  [ch bus editor local]
  (let [toolbar (dom/by-id "toolbar")]
    (go
      (loop []
        (let [[_ [id render?]] (<! ch)
              node (get-in @local [:nodes id])
              {:keys [tree meshes]} @editor]
          (when id
            (swap! local assoc :selected-id nil)
            (swap! editor assoc :selection nil :sel-type nil)
            (dom/remove-class! (:el node) "selected")
            (dom/remove-class! toolbar "rollon")
            (when render?
              (swap!
               editor assoc
               :display-meshes (tree/filter-leaves-and-selection meshes tree nil))
              (async/publish bus :render-scene nil)
              (regenerate-map editor local))
            (recur)))))))

(defn handle-op-triggered
  [ch bus editor local]
  (go
    (loop []
      (let [[_ op] (<! ch)
            {:keys [tree selection]} @editor]
        (when op
          (debug :new-op op)
          (remove-op-controls local)
          (handle-operator op editor local bus)
          (async/publish bus :user-action nil)
          (recur))))))

(defn handle-commit-op
  [ch bus local]
  (go
    (loop []
      (when (<! ch)
        (remove-op-controls local)
        (async/publish bus :regenerate-scene nil)
        (async/publish bus :user-action nil)
        (recur)))))

(defn handle-cancel-op
  [ch bus editor local]
  (go
    (loop []
      (when (<! ch)
        (remove-op-controls local)
        ;; TODO
        (async/publish bus :regenerate-scene nil)
        (async/publish bus :user-action nil)
        (recur)))))

(defn handle-viz-interactions
  [events bus editor local]
  (let [canvas (:canvas @local)]
    (go
      (loop [down? false]
        (let [[e ch] (alts! events)]
          (when e
            (if (or (= ch (events 0))
                    (= ch (events 3))
                    (and down? (= ch (events 1))))
              (do
                (scroll-viewport editor local (- (.-clientX e) (.-offsetLeft canvas) 10))
                (async/publish bus :user-action nil)
                (recur true))
              (recur false))))))))

(defn handle-release
  [ch bus local]
  (go
    (let [_ (<! ch)
          {:keys [viz canvas op-triggers nodes subs ctrls]} @local]
      (debug :tedit-release)
      (dom/remove! viz)
      (dom/remove! canvas)
      (dom/remove-class! (dom/by-id "toolbar") "rollon")
      (dom/add-class! (dom/by-id "op-ctrl") "hidden")
      (remove-node-event-handlers bus nodes)
      (remove-op-triggers bus op-triggers)
      (remove-op-controls local)
      (dorun (map dom/destroy-event-channel (:events @local)))
      (async/unsubscribe-and-close-many bus subs)
      (reset! local nil))))

(defn init
  [editor bus config]
  (let [{:keys [gap margin height map-width map-height]} (:editor config)
        parent  (dom/by-id "edit-treemap")
        viz     (dom/insert!
                 (dom/create! "div" nil {:id "viz-container"})
                 parent)
        canvas  (dom/insert!
                 (dom/create! "canvas" nil
                              {:id "viz-map"
                               :width map-width
                               :height map-height})
                 parent)
        subs    (async/subscription-channels
                 bus [:node-toggle :node-selected :node-deselected
                      :commit-operator :cancel-operator :op-triggered
                      :window-resize :regenerate-scene
                      :release-editor])
        e-specs [(dom/event-channel canvas "mousedown")
                 (dom/event-channel canvas "mousemove")
                 (dom/event-channel canvas "mouseup")
                 (dom/event-channel canvas "touchmove" dom/touch-handler)]
        events  (mapv first e-specs)
        local   (atom
                 {:config      config
                  :subs        subs
                  :events      e-specs
                  :canvas      canvas
                  :ctx         (.getContext canvas "2d")
                  :viz         viz
                  :scroll      (vec2)
                  :nodes       {}
                  :selected-id nil
                  :height      height
                  :op-triggers (init-op-triggers bus config)})]
    (debug :init-tedit)
    (regenerate-viz editor local bus)
    (regenerate-map editor local)

    (handle-resize          (:window-resize subs)    bus editor local)
    (handle-regen           (:regenerate-scene subs) bus editor local)
    (handle-node-toggle     (:node-toggle subs)      bus local)
    (handle-node-selected   (:node-selected subs)    bus editor local)
    (handle-node-deselected (:node-deselected subs)  bus editor local)
    (handle-op-triggered    (:op-triggered subs)     bus editor local)
    (handle-commit-op       (:commit-operator subs)  bus local)
    (handle-cancel-op       (:cancel-operator subs)  bus editor local)
    (handle-release         (:release-editor subs)   bus local)
    (handle-viz-interactions events bus editor local)))
