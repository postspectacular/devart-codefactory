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

(def svg-ns "http://www.w3.org/2000/svg")

(defn make-node
  [parent path op x y w h bus sel]
  (let [el (dom/create! "div" parent)
        id (node-id path)
        cls (str "op-" (name op))
        [ch handler] (dom/event-channel el "click")
        label (fn [l]
                (dom/set-html! el l)
                ;;(dom/set-style! el #js {:line-height (->px h)})
                )]

    (cond
     (= :leaf op)
     (label (if (empty? path) (:root-label config/editor) "+"))

     (= :delete op)
     (let [svg (dom/create-ns!
                svg-ns "svg" el
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

    (go
      (loop []
        (when (<! ch)
          (async/publish bus :node-toggle id)
          (recur))))

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
  [bus viz tree scroll sel]
  (let [{:keys [gap height margin-bottom]} config/editor
        off (g/+ scroll
                 (.-offsetLeft viz)
                 (- (.-innerHeight js/window) (+ height margin-bottom)))]
    (fn gen-branch*
      [acc nodes path x y w h]
      (let [branch (tree/select-sub-paths nodes path)
            children (tree/select-direct-children branch path)
            nc (count children)
            wc (cell-size w gap nc)
            cy (- y h gap)
            op (tree/node-operator (tree/node-at tree path))
            acc (conj acc (make-node viz path op (+ x (:x off)) (+ (- y h) (:y off)) w h bus sel))]
        (if (pos? nc)
          (reduce
           (fn [acc [c i]]
             (gen-branch* acc branch c (mm/madd i wc i gap x) cy wc h))
           acc (zipmap (sort (keys children)) (range)))
          acc)))))

(defn compute-viewport
  [local]
  (let [{:keys [scroll]} @local
        {:keys [margin map-width map-height height]} config/editor]
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
  (let [{:keys [max-nodes-path tree]} @editor
        {:keys [gap margin min-size map-width]} config/editor
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
  (let [{:keys [viz nodes width scroll]} @local
        {:keys [node-cache tree tree-depth selection]} @editor
        {:keys [gap margin height]} config/editor
        width' (compute-required-width editor)
        node-height (cell-size height gap tree-depth)
        scroll (if (== width width') scroll (vec2))
        layout (generate-branch bus viz tree scroll selection)]
    (dom/set-html! viz "")
    (remove-node-event-handlers bus nodes)
    (swap!
     local assoc
     :width       width'
     :node-height node-height
     :scroll      scroll
     :nodes       (layout {} node-cache [] margin height width' node-height))))

(defn resize-branch
  [nodes tree gap offset]
  (fn resize-branch*
    [path x w]
    (let [id (node-id path)
          el (:el (nodes id))
          children (:out (tree/node-at tree path))
          nc (count children)
          wc (cell-size w gap nc)]
      (dom/set-style! el #js {:left (->px (+ x (:x offset))) :width (->px w)})
      (if (pos? nc)
        (loop [i 0]
          (when (< i nc)
            (resize-branch* (conj path i) (mm/madd i wc i gap x) wc)
            (recur (inc i))))))))

(defn resize-viz
  [editor local]
  (let [{:keys [viz nodes scroll]} @local
        {:keys [node-cache tree tree-depth]} @editor
        {:keys [gap margin height]} config/editor
        width  (compute-required-width editor)
        offset (g/+ scroll (.-offsetLeft viz) (- (.-innerHeight js/window) 270))]
    ((resize-branch nodes tree gap offset) [] margin width)
    (swap! local assoc :width width)))

(def map-op-color
  (memoize
   (fn [op]
     (-> op config/operator-color (col/gray-offset-hex (:map-color-offset config/editor))))))

(defn map-branch
  [ctx tree path x y w h sel]
  (let [{:keys [op out] :as node} (tree/node-at tree path)
        nc (count out)
        wc (cell-size w 1 nc)
        col (if (= path sel)
              (:map-selection config/editor)
              (map-op-color (tree/node-operator node)))]
    (set! (.-fillStyle ctx) col)
    (.fillRect ctx x (inc (- y h)) w h)
    (if (pos? nc)
      (loop [i 0]
        (when (< i nc)
          (map-branch ctx tree (conj path i) (mm/madd i wc i 1 x) (- y h) wc h sel)
          (recur (inc i)))))))

(defn regenerate-map
  [editor local]
  (let [{:keys [ctx]} (:map @local)
        {:keys [width height]} @local
        {:keys [tree tree-depth selection]} @editor
        {:keys [map-bg map-width map-height map-labels]} config/editor
        [vx vy vw vh] (map-focus-rect
                       (compute-viewport local)
                       width height map-width map-height)
        cy (/ map-height 2)
        nl (count map-labels)]
    (swap! local assoc-in [:map :viewport] [vx vy vw vh])
    (set! (.-fillStyle ctx) map-bg)
    (set! (.-strokeStyle ctx) nil)
    (.fillRect ctx 0 0 map-width map-height)
    (map-branch ctx tree [] 0 map-height map-width (/ map-height tree-depth) selection)
    (when (== 1 tree-depth)
      (set! (.-fillStyle ctx) map-bg)
      (set! (.-font ctx) "14px \"Abel\", sans-serif")
      (set! (.-textAlign ctx) "center")
      (set! (.-textBaseline ctx) "middle")
      (loop [i 0, y (int (/ nl -2))]
        (when (< i nl)
          (.fillText ctx (map-labels i) (/ map-width 2) (mm/madd 18 y cy))
          (recur (inc i) (inc y)))))
    (set! (.-strokeStyle ctx) "yellow")
    (set! (.-lineWidth ctx) 2)
    (.strokeRect ctx vx vy vw vh)
    ))

(defn scroll-viewport
  [editor local x]
  (let [{:keys [map-width map-height]} config/editor
        {:keys [width map]} @local
        [_ _ viz-width] (compute-viewport local)
        [_ _ vw] (:viewport map)
        sx (m/map-interval-clamped (mm/madd vw -0.5 x) 0 (- map-width vw) 0 (- (- width viz-width)))]
    (swap! local assoc-in [:scroll :x] sx)
    (resize-viz editor local)
    (regenerate-map editor local)))

(defn init-op-triggers
  [bus]
  (mapv
   (fn [op]
     (let [el (dom/by-id (str "op-" (name op)))
           f (fn [e] (.preventDefault e) (async/publish bus :op-triggered op))]
       (.addEventListener el "click" f)
       [el f]))
   (keys (dissoc config/operators :leaf))))

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
        parent (dom/by-id "sliders")
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
  [editor bus path op specs]
  (let [op-col (config/operator-color op)]
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
        {:keys [viz] {:keys [canvas]} :map} @local
        path (mg/child-path selection)
        node (if (:op orig) orig default)
        ctrl (dom/by-id "op-ctrl")
        listeners (init-op-controls editor bus path op sliders)
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
    (dom/remove-class! ctrl "hidden")
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
  (let [{:keys [viz ctrl-active? ctrl-listeners] {:keys [canvas]} :map} @local]
    (when ctrl-active?
      (swap! local assoc :ctrl-active? false :ctrl-listeners nil)
      (dom/remove-listeners ctrl-listeners)
      (dom/add-class! (dom/by-id "op-ctrl") "hidden")
      (dom/set-html! (dom/by-id "sliders") "")
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

(defn init
  [editor bus]
  (let [{:keys [gap margin height map-width map-height]} config/editor
        {:keys [tree-depth]} @editor
        toolbar         (dom/by-id "toolbar")
        parent          (dom/by-id "edit-treemap")
        viz             (dom/insert! (dom/create! "div" nil {:id "viz-container"}) parent)
        canvas          (dom/insert!
                         (dom/create!
                          "canvas" nil
                          {:id "viz-map" :width map-width :height map-height})
                         parent)
        node-toggle     (async/subscribe bus :node-toggle)
        node-selected   (async/subscribe bus :node-selected)
        node-deselected (async/subscribe bus :node-deselected)
        commit-op       (async/subscribe bus :commit-operator)
        cancel-op       (async/subscribe bus :cancel-operator)
        release         (async/subscribe bus :release-editor)
        resize          (async/subscribe bus :window-resize)
        op-triggered    (async/subscribe bus :op-triggered)
        regenerate      (async/subscribe bus :regenerate-scene)
        mdown           (dom/event-channel canvas "mousedown")
        mmove           (dom/event-channel canvas "mousemove")
        mup             (dom/event-channel canvas "mouseup")
        tmove           (dom/event-channel canvas "touchmove" dom/touch-handler)
        mt-inputs       (mapv first [mdown mmove mup tmove])
        op-triggers     (init-op-triggers bus)
        local           (atom
                         {:subs       {:node-selected node-selected
                                       :node-deselected node-deselected
                                       :node-toggle node-toggle
                                       :release-editor release
                                       :window-resize resize
                                       :op-triggered op-triggered
                                       :commit-operator commit-op
                                       :cancel-operator cancel-op
                                       :regenerate-scene regenerate}
                          :events     [mdown mmove mup tmove]
                          :map        {:canvas canvas :ctx (.getContext canvas "2d")}
                          :viz        viz
                          :scroll     (vec2)
                          :nodes      {}
                          :selected-id nil
                          :height      height})]
    (regenerate-viz editor local bus)
    (regenerate-map editor local)

    (go
      (loop []
        (when (<! resize)
          (resize-viz editor local)
          (regenerate-map editor local)
          (async/publish bus :user-action nil)
          (recur))))

    (go
      (loop []
        (when (<! regenerate)
          (regenerate-viz editor local bus)
          (regenerate-map editor local)
          (async/publish bus :render-scene nil)
          (recur))))

    (go
      (loop []
        (let [[_ id] (<! node-toggle)
              sel (:selected-id @local)]
          (when id
            (if (= id sel)
              (async/publish bus :node-deselected [id true])
              (do
                (when sel (async/publish bus :node-deselected [sel false]))
                (async/publish bus :node-selected id)))
            (async/publish bus :user-action nil)
            (recur)))))

    (go
      (loop []
        (let [[_ id] (<! node-selected)
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
            (recur)))))

    (go
      (loop []
        (let [[_ [id render?]] (<! node-deselected)
              node (get-in @local [:nodes id])
              {:keys [tree meshes]} @editor]
          (when id
            (swap! local assoc :selected-id nil)
            (swap!
             editor assoc
             :selection nil :sel-type nil)
            (dom/remove-class! (:el node) "selected")
            (dom/remove-class! toolbar "rollon")
            (when render?
              (swap!
               editor assoc
               :display-meshes (tree/filter-leaves-and-selection meshes tree nil))
              (async/publish bus :render-scene nil)
              (regenerate-map editor local))
            (recur)))))

    (go
      (loop []
        (let [[_ op] (<! op-triggered)
              {:keys [tree selection]} @editor]
          (when op
            (debug :new-op op)
            (remove-op-controls local)
            (handle-operator op editor local bus)
            (async/publish bus :user-action nil)
            (recur)))))

    (go
      (loop []
        (when (<! commit-op)
          (remove-op-controls local)
          (async/publish bus :regenerate-scene nil)
          (async/publish bus :user-action nil)
          (recur))))

    (go
      (loop [down? false]
        (let [[e ch] (alts! mt-inputs)]
          (when e
            (if (or (= ch (mt-inputs 0))
                    (= ch (mt-inputs 3))
                    (and down? (= ch (mt-inputs 1))))
              (do
                (scroll-viewport editor local (- (.-clientX e) (.-offsetLeft canvas) 10))
                (async/publish bus :user-action nil)
                (recur true))
              (recur false))))))

    (go
      (let [_ (<! release)
            {:keys [nodes subs ctrls]} @local]
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
        (reset! local nil)))

    ))
