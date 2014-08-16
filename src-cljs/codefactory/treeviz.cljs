(ns codefactory.treeviz
  (:require-macros
   [cljs.core.async.macros :refer [go]]
   [thi.ng.macromath.core :as mm])
  (:require
   [cljs.core.async :as cas :refer [>! <! alts! chan put! close! timeout]]
   [codefactory.config :as config]
   [codefactory.color :as col]
   [codefactory.tree :as tree]
   [codefactory.operators :as ops]
   [codefactory.common :as common]
   [thi.ng.cljs.async :as async]
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.utils :as utils :refer [->px]]
   [thi.ng.cljs.dom :as dom]
   [thi.ng.cljs.gestures :as gest]
   [thi.ng.geom.ui.arcball :as arcball]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.core.matrix :as mat :refer [M44]]
   [thi.ng.geom.core.vector :as v :refer [vec2 vec3]]
   [thi.ng.geom.core.quaternion :as q]
   [thi.ng.geom.rect :as r]
   [thi.ng.common.math.core :as m]))

(def node-id
  (memoize (fn [path] (apply str (cons "node-" (interpose "-" path))))))

(defn highlight-selected-node
  [el nodes]
  (loop [nodes nodes]
    (when-let [n (first nodes)]
      (if (= n el)
        (-> n (dom/remove-class! "deselected") (dom/add-class! "selected"))
        (-> n (dom/remove-class! "selected") (dom/add-class! "deselected")))
      (recur (next nodes)))))

(defn unhighlight-selected-node
  [el nodes]
  (loop [nodes nodes]
    (when nodes
      (-> (first nodes)
          (dom/remove-class! "selected")
          (dom/remove-class! "deselected"))
      (recur (next nodes)))))

(defn set-node-label
  [el path op depth width min-width intro?]
  (cond
   (= :leaf op)
   (if (empty? path)
     (if intro?
       (dom/add-class! el "op-root")
       (-> el
           (dom/set-html! (-> config/app :editor :root-label))
           (dom/add-class! "op-root flash")))
     (dom/set-html!
      el
      (if (== 2 depth)
        (-> config/app :editor :ftu-label)
        (-> config/app :editor :leaf-label))))

   (= :delete op)
   (let [svg (dom/create-ns!
              dom/svg-ns "svg" el
              {:width "100%" :height "100%"
               :viewBox "0 0 1 1"
               :preserveAspectRatio "none"
               :class "op-delete"})]
     (dom/create-ns!
      dom/svg-ns "path" svg {:d "M0.01,0.01 L0.99,0.99 M0.01,0.99 L0.99,0.01"})
     (dom/create-ns!
      dom/svg-ns "rect" svg {:x 0.01 :y 0.01 :width 0.98 :height 0.98}))

   :else
   (if (>= width min-width)
     (dom/set-html! el (str "<span>" (-> config/app :operators op :label) "</span>")))))

(defn make-node
  [parent path op x y w h ox oy bus sel event?]
  (let [el (dom/create! "div" parent)
        id (node-id path)
        x' (+ x ox)
        y' (+ y oy)]

    (doto el
      (dom/set-attribs! {:id id})
      (dom/set-style!
       #js {:left   (->px x')
            :top    (->px y')
            :width  (->px w)
            :height (->px h)}))

    (dom/add-class! el (ops/op-class op))
    (if sel
      (if (= path sel)
        (dom/add-class! el "selected")
        (dom/add-class! el "deselected")))
    ;;(if (and sel (not= path sel)) (dom/add-class! el "deselected"))

    (if event?
      (dom/add-listeners [[el "click" #(async/publish bus :node-toggle id)]]))

    [id {:el el :x x :y y :w w :h h :path path}]))

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

(defn viewport-width
  []
  (let [{:keys [margin map-width]} (:editor config/app)]
    (mm/madd margin -3 (- (.-innerWidth js/window) map-width))))

(defn compute-viewport
  [local]
  (let [{:keys [scroll]} @local
        {:keys [height]} (:editor config/app)]
    [(- (:x scroll)) (:y scroll) (viewport-width) height]))

(defn scroll-offset
  [scroll el]
  (let [{:keys [height margin-bottom]} (:editor config/app)]
    (g/+ scroll
         (.-offsetLeft el)
         (mm/sub (.-innerHeight js/window) height margin-bottom))))

(defn map-focus-rect
  [[x y w h] viz-width viz-height map-width map-height]
  (let [x (m/map-interval x 0 viz-width  1 (- map-width 2))
        y (m/map-interval y 0 viz-height 1 (- map-height 2))
        w (m/map-interval w 0 viz-width  1 (- map-width 2))
        h (m/map-interval h 0 viz-height 1 (- map-height 2))]
    [x y w h]))

(defn compute-required-width
  [editor]
  (let [{:keys [max-nodes-path tree]} @editor
        {:keys [gap margin min-size]} (:editor config/app)
        width  (viewport-width)
        cw     (cell-size-at tree max-nodes-path width gap)]
    (if (< cw min-size)
      (loop [path     max-nodes-path
             num-sibs (tree/num-children-at tree max-nodes-path)
             width    min-size]
        (let [width'  (mm/madd width num-sibs gap (dec num-sibs))]
          (if (seq path)
            (let [path' (pop path)]
              (recur path' (tree/num-children-at tree path') width'))
            width')))
      width)))

(defn reposition-branch
  [nodes tree gap [offx offy]]
  (fn repos-branch*
    [path x w]
    (let [el (:el (nodes (node-id path)))
          nc (tree/num-children-at tree path)
          wc (cell-size w gap nc)]
      (dom/set-style! el #js {:left (->px (+ x offx))})
      (if (pos? nc)
        (loop [i 0]
          (when (< i nc)
            (repos-branch* (conj path i) (mm/madd i wc i gap x) wc)
            (recur (inc i))))))))

(defn reposition-viz
  [editor local scroll]
  (let [{:keys [viz nodes width]} @local
        {:keys [node-cache tree]} @editor
        {:keys [gap margin map-width]} (:editor config/app)
        min (- (viewport-width) width)
        scroll (assoc scroll :x (m/clamp (:x scroll) min 0))
        offset (scroll-offset scroll viz)]
    ;;(debug :repos-scroll scroll)
    (swap! local assoc :scroll scroll)
    ((reposition-branch nodes tree gap offset) [] margin width)))

(defn center-node
  [editor local]
  (let [{:keys [width nodes selected-id]} @local
        {:keys [w x]} (nodes selected-id)
        vp-width (viewport-width)
        min (- vp-width width)
        x' (+ (- (- x) (/ w 2)) (/ vp-width 2))
        x' (m/clamp x' min 0)]
    (reposition-viz editor local (vec2 x' 0))))

(defn generate-branch
  [bus viz tree depth scroll sel intro?]
  (let [{:keys [gap min-label-width]} (:editor config/app)
        [offx offy] (scroll-offset scroll viz)
        event? (not intro?)]
    (fn gen-branch*
      [acc path x y w h]
      (let [node (tree/node-at tree path)
            nc   (count (:out node))
            wc   (cell-size w gap nc)
            cy   (mm/sub y h gap)
            op   (config/translate-mg-op (tree/node-operator node))
            [x y w h] (if (and (== 1 depth) (empty? path))
                        [x (- y 4) (- w 4) (- h 4)] [x y w h])
            node (make-node viz path op x (- y h) w h offx offy bus sel event?)
            acc  (conj acc node)]
        (set-node-label (-> node second :el) path op depth w min-label-width intro?)
        (if (pos? nc)
          (loop [acc acc, i 0]
            (if (< i nc)
              (recur (gen-branch* acc (conj path i) (mm/madd i wc i gap x) cy wc h)
                     (inc i))
              acc))
          acc)))))

(defn regenerate-viz
  [editor local bus]
  (tree/update-stats editor)
  (let [{:keys [viz nodes width scroll]} @local
        {:keys [tree tree-depth selection intro-active?]} @editor
        {:keys [gap margin height]} (:editor config/app)
        width' (compute-required-width editor)
        scroll (if (< width' (m/abs (:x scroll))) (vec2) scroll)
        node-height (cell-size height gap tree-depth)
        layout (generate-branch bus viz tree tree-depth scroll selection intro-active?)]
    (-> viz
        (dom/set-html! "")
        (dom/set-attribs! {:class (str "depth" tree-depth)}))
    (swap!
     local assoc
     :width       width'
     :node-height node-height
     :scroll      scroll
     :nodes       (layout {} [] margin height width' node-height))
    (if selection
      (center-node editor local))))

(defn resize-branch
  [nodes tree depth gap [offx offy]]
  (fn resize-branch*
    [path x y w h]
    (let [el (:el (nodes (node-id path)))
          nc (tree/num-children-at tree path)
          [x y w h] (if (and (== 1 depth) (empty? path))
                      [x (- y 4) (- w 4) (- h 4)] [x y w h])
          wc (cell-size w gap nc)
          cy (mm/sub y h gap)]
      (dom/set-style!
       el #js {:left  (->px (+ x offx))
               :top   (->px (+ (- y h) offy))
               :width (->px w)})
      (if (pos? nc)
        (loop [i 0]
          (when (< i nc)
            (resize-branch* (conj path i) (mm/madd i wc i gap x) cy wc h)
            (recur (inc i))))))))

(defn resize-viz
  [editor local]
  (let [{:keys [viz nodes scroll]}           @local
        {:keys [node-cache tree tree-depth]} @editor
        {:keys [gap margin height]}          (:editor config/app)
        width       (compute-required-width editor)
        node-height (cell-size height gap tree-depth)
        offset      (scroll-offset scroll viz)]
    ((resize-branch nodes tree tree-depth gap offset) [] margin height width node-height)
    (swap! local assoc :width width :node-height node-height)))

(def map-op-color
  (memoize
   (fn [op]
     ;;(debug :col-op op)
     (col/gray-offset-hex
      (config/operator-color op)
      (-> config/app :editor :map-color-offset)))))

(defn draw-map-branch
  [ctx tree path x y w h sel]
  (let [{:keys [op out] :as node} (tree/node-at tree path)
        nc (count out)
        wc (cell-size w 1 nc)
        col (map-op-color (config/translate-mg-op (tree/node-operator node)))]
    (if (or (not sel) (= path sel))
      (do
        (set! (.-fillStyle ctx) col)
        (set! (.-strokeStyle ctx) "")
        (.fillRect ctx x (inc (- y h)) w (dec h)))
      (do
        (set! (.-fillStyle ctx) "")
        (set! (.-strokeStyle ctx) col)
        (.strokeRect ctx x (inc (- y h)) (max (dec w) 1) (dec h))))
    (if (pos? nc)
      (loop [i 0]
        (when (< i nc)
          (draw-map-branch ctx tree (conj path i) (mm/madd i wc i 1 x) (- y h) wc h sel)
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
  (let [{:keys [ctx width height]} @local
        {:keys [tree tree-depth selection]} @editor
        {:keys [map-bg map-label-col map-width map-height] :as econf} (:editor config/app)
        [vx vy vw vh :as vp] (map-focus-rect
                              (compute-viewport local)
                              width height map-width map-height)]
    (swap! local assoc :viewport vp)
    (set! (.-fillStyle ctx) map-bg)
    (set! (.-strokeStyle ctx) nil)
    (set! (.-lineWidth ctx) 1)
    (.fillRect ctx 0 0 map-width map-height)
    (if (< 1 tree-depth)
      (do
        (draw-map-branch ctx tree [] 0 map-height
                         map-width (/ map-height tree-depth)
                         selection)
        (set! (.-strokeStyle ctx) "yellow")
        (set! (.-lineWidth ctx) 2)
        (.strokeRect ctx vx vy vw vh))
      (let [{:keys [map-labels map-label-font map-label-size]} econf]
        (draw-map-labels ctx map-labels
                         (/ map-width 2) (/ map-height 2)
                         map-label-col map-label-font map-label-size)))))

(defn scroll-viewport
  [editor local x]
  (let [{:keys [width viewport scroll]} @local
        {:keys [map-width map-height]} (:editor config/app)
        [_ _ viz-width] (compute-viewport local)
        [_ _ vw] viewport
        sx (m/map-interval-clamped
            (mm/madd vw -0.5 x)
            0 (- map-width vw)
            0 (- (- width viz-width)))]
    (reposition-viz editor local (assoc scroll :x sx))
    (regenerate-map editor local)))

(defn update-submit-button
  [depth]
  ((if (and (number? depth) (>= depth (-> config/app :editor :min-submit-depth)))
     dom/remove-class!
     dom/add-class!)
   (config/dom-component :edit-continue) "hidden"))

(defn handle-resize
  [ch bus editor local]
  (go
    (loop []
      (when (<! ch)
        (resize-viz editor local)
        (regenerate-map editor local)
        (async/publish bus :user-action nil)
        (recur)))))

(defn handle-regenerate
  [ch bus editor local]
  (go
    (loop []
      (when (<! ch)
        (let [{:keys [tree-depth bounds arcball]} @editor]
          (update-submit-button tree-depth)
          (arcball/update-zoom-range arcball bounds)
          (regenerate-viz editor local bus)
          (regenerate-map editor local)
          (async/publish bus :render-scene nil))
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
  (let [toolbar (config/dom-component :toolbar)]
    (go
      (loop []
        (let [[_ id] (<! ch)
              {:keys [el path]} (get-in @local [:nodes id])
              {:keys [tree tree-depth meshes tools start-time]} @editor]
          (when id
            (if el
              (let [node (tree/node-at tree path)
                    op (config/translate-mg-op (tree/node-operator node))]
                (swap! local assoc :selected-id id)
                (swap!
                 editor assoc
                 :selection path
                 :sel-type op
                 :sel-time (mm/subm (utils/now) start-time 0.001)
                 :display-meshes (tree/filter-leaves-and-selection meshes tree path))
                (when (and (< tree-depth 3) (= op :leaf))
                  (-> el
                      (dom/set-html! (-> config/app :editor :ftu-label-sel))
                      (dom/remove-class! "op-root")))
                (highlight-selected-node el (->> @local :nodes vals (map :el)))
                (ops/enable-presets (:specs tools))
                (debug :sel-node node)
                (if-not (#{:leaf :delete} op)
                  (async/publish bus :op-triggered (:id node))
                  (async/publish bus :render-scene nil))
                (async/publish bus :user-action nil)
                (regenerate-map editor local))
              (warn :unknown-sel-id id))
            (recur)))))))

(defn handle-node-deselected
  [ch bus editor local]
  (let [toolbar (config/dom-component :toolbar)]
    (go
      (loop []
        (let [[_ [id render?]] (<! ch)
              {:keys [el]} (get-in @local [:nodes id])
              {:keys [tree tree-depth meshes selection sel-type tools]} @editor
              node (tree/node-at tree selection)
              op (config/translate-mg-op (tree/node-operator node))]
          (when id
            (swap! local assoc :selected-id nil)
            (swap! editor assoc :selection nil :sel-type nil)
            (unhighlight-selected-node el (->> @local :nodes vals (map :el)))
            (if (= op :leaf)
              (if (seq selection)
                (if (< tree-depth 3)
                  (dom/set-html! el (-> config/app :editor :ftu-label)))
                (-> el
                    (dom/set-html! (get-in config/app [:editor :root-label]))
                    (dom/add-class! "op-root"))))
            (ops/disable-presets (:specs tools))
            (ops/release-op-controls local)
            (when render?
              (swap!
               editor assoc
               :display-meshes (tree/filter-leaves-and-selection meshes tree nil))
              (async/publish bus :render-scene nil)
              (regenerate-map editor local))
            (recur)))))))

(defn orig-tree-node
  [tree selection local]
  (if (:ctrl-active? local)
    (:orig-edit-node local)
    (tree/node-at tree selection)))

(defn handle-op-triggered
  [ch bus editor local]
  (go
    (loop []
      (let [[_ id] (<! ch)
            {:keys [tree node-cache selection tools]} @editor]
        (when id
          (when (and selection (not (:active? tools)))
            (let [{preset :node view :view} (config/preset-for-id id)
                  op (config/translate-mg-op (:op preset))]
              (debug :new-op id preset)
              (ops/release-op-controls local)
              (ops/highlight-selected-preset id (:specs tools))
              (ops/center-preset bus (id (:specs tools)))
              (async/publish bus :backup-tree tree)
              (when (and view (-> config/app :editor :views-enabled?))
                (async/publish
                 bus :camera-update
                 (if (fn? view)
                   (view (node-cache selection))
                   (q/quat view))))
              (ops/handle-operator
               (or op id)
               (assoc preset :id id)
               (orig-tree-node tree selection @local)
               editor local bus)
              (async/publish bus :regenerate-scene nil)
              (when (and (not (:axis-hint-shown? @editor))
                         (== 1 (:tree-depth @editor)))
                (swap! editor assoc :axis-hint-shown? true)
                (async/publish bus :show-tooltip [(dom/by-id "axis-toggle") :axis-label]))))
          (async/publish bus :user-action nil)
          (recur))))))

(defn handle-undo
  [ch bus editor local]
  (go
    (loop []
      (let [_ (<! ch)
            {:keys [tree history tools]} @editor]
        (when _
          (when (seq history)
            (swap!
             editor assoc
             :tree (peek history)
             :history (pop history)
             :selection nil
             :sel-type nil)
            (swap! local assoc :selected-id nil)
            (swap! editor tree/update-meshes true)
            (ops/release-op-controls local)
            (ops/disable-presets (:specs tools))
            (when-not (seq (:history @editor))
              (dom/add-class! (dom/by-id "undo") "disabled"))
            (async/publish bus :regenerate-scene nil))
          (async/publish bus :user-action nil)
          (recur))))))

(defn handle-commit-op
  [ch bus local]
  (go
    (loop []
      (when (<! ch)
        (ops/release-op-controls local)
        (async/publish bus :regenerate-scene nil)
        (async/publish bus :user-action nil)
        (recur)))))

(defn handle-cancel-op
  [ch bus editor local]
  (go
    (loop []
      (when (<! ch)
        (let [{:keys [orig-edit-node]} @local
              {:keys [tree selection]} @editor]
          (swap!
           editor assoc
           :tree (tree/set-node-at tree selection orig-edit-node)
           :sel-type (config/translate-mg-op (tree/node-operator orig-edit-node)))
          (swap! editor tree/update-meshes true)
          (ops/release-op-controls local)
          (async/publish bus :regenerate-scene nil)
          (async/publish bus :user-action nil)
          (recur))))))

(defn handle-map-interactions
  [events bus editor local]
  (let [canvas (:canvas @local)]
    (go
      (loop [down? false]
        (let [[[e data] ch] (alts! events)]
          (when e
            (if (or (= :drag-start e)
                    (and (or down? (:touch? data))
                         (= :drag-move e)))
              (do
                (scroll-viewport
                 editor local (- (:x (:p data)) (.-offsetLeft canvas) 10))
                (async/publish bus :user-action nil)
                (recur true))
              (recur false))))))))

(defn handle-viz-scroll
  [viz events bus editor local]
  (go
    (loop [state nil]
      (let [[[e data] ch] (alts! events)]
        (when e
          (recur
           (case e
             :drag-start (let [scroll (:scroll @local)
                               target (common/next-parent-id (:target data))
                               state [scroll (:p data) (vec2) target]]
                           (swap! local assoc :scroll-active? true)
                           (async/publish bus :user-action nil)
                           state)
             :drag-move  (when state
                           (let [[scroll p _ target] state
                                 delta  (g/- (:p data) p)
                                 scroll' (g/madd (assoc delta :y 0) 2 scroll)]
                             (reposition-viz editor local scroll')
                             (regenerate-map editor local)
                             (async/publish bus :user-action nil)
                             [scroll p delta target]))
             :gesture-end (when state
                            (let [[_ p delta target] state
                                  dist (g/mag delta)]
                              (when (and (:touch? data) (< dist 20)
                                         (= "node" (subs target 0 4)))
                                (async/publish bus :node-toggle target))
                              (swap! local assoc :scroll-active? false)
                              (async/publish bus :user-action nil)
                              nil))
             nil)))))))

(defn handle-release
  [ch bus local]
  (go
    (let [_ (<! ch)
          {:keys [viz canvas op-triggers nodes subs ctrls]} @local]
      (debug :tedit-release)
      (dom/set-html! viz "")
      (ops/release-op-controls local)
      (dorun (map async/destroy-event-channel (:events @local)))
      (async/unsubscribe-and-close-many bus subs)
      (reset! local nil))))

(defn init
  [editor bus]
  (let [{:keys [gap margin height map-width map-height]} (:editor config/app)
        viz       (config/dom-component :viz-container)
        canvas    (-> (config/dom-component :viz-map)
                      (dom/set-attribs! {:width map-width :height map-height}))
        subs      (async/subscription-channels
                   bus [:node-toggle :node-selected :node-deselected
                        :commit-operator :cancel-operator
                        :op-triggered :undo-triggered
                        :window-resize :regenerate-scene
                        :release-editor])
        m-specs   (mapv
                   #(apply async/event-channel %)
                   [[canvas "mousedown" gest/mouse-gesture-start]
                    [canvas "mousemove" gest/mouse-gesture-move]
                    [canvas "mouseup" gest/gesture-end]
                    [canvas "touchmove" gest/touch-gesture-move]])
        v-specs   (mapv
                   #(apply async/event-channel %)
                   [[viz "mousedown" gest/mouse-gesture-start]
                    [viz "mousemove" gest/mouse-gesture-move]
                    [js/window "mouseup" gest/gesture-end]
                    [viz "touchstart" gest/touch-gesture-start]
                    [viz "touchmove" gest/touch-gesture-move]
                    [js/window "touchend" gest/gesture-end]])
        m-events  (mapv first m-specs)
        v-events  (mapv first v-specs)
        local   (atom
                 {:subs        subs
                  :events      (concat m-specs v-specs)
                  :canvas      canvas
                  :ctx         (.getContext canvas "2d")
                  :viz         viz
                  :scroll      (vec2)
                  :nodes       {}
                  :selected-id nil
                  :height      height})]
    (debug :init-tedit)
    (update-submit-button (:tree-depth @editor))
    (regenerate-viz editor local bus)
    (regenerate-map editor local)

    (handle-resize          (:window-resize subs)    bus editor local)
    (handle-regenerate      (:regenerate-scene subs) bus editor local)
    (handle-node-toggle     (:node-toggle subs)      bus local)
    (handle-node-selected   (:node-selected subs)    bus editor local)
    (handle-node-deselected (:node-deselected subs)  bus editor local)
    (handle-op-triggered    (:op-triggered subs)     bus editor local)
    (handle-undo            (:undo-triggered subs)   bus editor local)
    (handle-commit-op       (:commit-operator subs)  bus local)
    (handle-cancel-op       (:cancel-operator subs)  bus editor local)
    (handle-release         (:release-editor subs)   bus local)
    (handle-map-interactions m-events bus editor local)
    (handle-viz-scroll viz v-events bus editor local)))
