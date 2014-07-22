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
   [codefactory.shared :as shared]
   [thi.ng.cljs.async :as async]
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.utils :as utils :refer [->px]]
   [thi.ng.cljs.dom :as dom]
   [thi.ng.cljs.gestures :as gest]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.core.matrix :as mat :refer [M44]]
   [thi.ng.geom.core.vector :as v :refer [vec2 vec3]]
   [thi.ng.geom.rect :as r]
   [thi.ng.common.math.core :as m]))

(def node-id
  (memoize (fn [path] (apply str (cons "node-" (interpose "-" path))))))

(defn highlight-selected-node
  [el op]
  (dom/add-class! el (str "sel-flash-" (name op))))

(defn unhighlight-selected-node
  [el op]
  (dom/remove-class! el (str "sel-flash-" (name op))))

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
  [parent path op x y w h bus sel min-label-width]
  (let [el (dom/create! "div" parent)
        id (node-id path)
        cls (str "op-" (name op))
        [ch handler] (async/event-channel el "click")]

    (cond
     (= :leaf op)
     (dom/set-html!
      el (if (empty? path)
           (-> config/app :editor :root-label)
           (-> config/app :editor :leaf-label)))

     (= :delete op)
     (let [svg (dom/create-ns!
                dom/svg-ns "svg" el
                {:width "100%" :height "100%"
                 :viewBox "0 0 1 1"
                 :preserveAspectRatio "none"
                 :class "op-delete"})]
       (dom/create-ns!
        dom/svg-ns "path" svg {:d "M0,0 L1,1 M0,1 L1,0"})
       (dom/create-ns!
        dom/svg-ns "rect" svg {:x 0 :y 0 :width 1 :height 1}))

     :else
     (when (>= w min-label-width)
       (dom/set-text! el (-> config/app :operators op :label))))

    (doto el
      (dom/set-attribs! {:id id})
      (dom/set-style!
       #js {:left   (->px x)
            :top    (->px y)
            :width  (->px w)
            :height (->px h)}))

    (if (not= :delete op) (dom/add-class! el cls))
    (if (= path sel) (highlight-selected-node el op))

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
  (let [x (m/map-interval x 0 viz-width 1 (- map-width 2))
        y (m/map-interval y 0 viz-height 1 (- map-height 2))
        w (m/map-interval w 0 viz-width 1 (- map-width 2))
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
        (let [width (mm/madd width num-sibs gap (dec num-sibs))]
          (if (seq path)
            (let [path' (pop path)]
              (recur path' (tree/num-children-at tree path') width))
            width)))
      width)))

(defn generate-branch
  [bus viz tree scroll sel]
  (let [{:keys [gap min-label-width]} (:editor config/app)
        [offx offy] (scroll-offset scroll viz)]
    (fn gen-branch*
      [acc path x y w h]
      (let [node (tree/node-at tree path)
            nc (count (:out node))
            wc (cell-size w gap nc)
            cy (mm/sub y h gap)
            op (config/translate-mg-op (tree/node-operator node))
            acc (conj acc
                      (make-node viz path op
                                 (+ x offx) (+ (- y h) offy) w h
                                 bus sel min-label-width))]
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
        {:keys [tree tree-depth selection]} @editor
        {:keys [gap margin height]} (:editor config/app)
        width' (compute-required-width editor)
        node-height (cell-size height gap tree-depth)
        ;;scroll (if (== width width') scroll (vec2))
        layout (generate-branch bus viz tree scroll selection)]
    (dom/set-html! viz "")
    (remove-node-event-handlers bus nodes)
    (swap!
     local assoc
     :width       width'
     :node-height node-height
     ;;:scroll      scroll
     :nodes       (layout {} [] margin height width' node-height))))

(defn resize-branch
  [nodes tree gap [offx offy]]
  (fn resize-branch*
    [path x y w h]
    (let [el (:el (nodes (node-id path)))
          nc (tree/num-children-at tree path)
          cy (mm/sub y h gap)
          wc (cell-size w gap nc)]
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
    ((resize-branch nodes tree gap offset) [] margin height width node-height)
    (swap! local assoc :width width :node-height node-height)))

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
    (swap! local assoc :scroll scroll)
    ((reposition-branch nodes tree gap offset) [] margin width)))

(def map-op-color
  (memoize
   (fn [op]
     (debug :col-op op)
     (col/gray-offset-hex
      (config/operator-color op)
      (-> config/app :editor :map-color-offset)))))

(defn map-branch
  [ctx tree path x y w h sel]
  (let [{:keys [op out] :as node} (tree/node-at tree path)
        nc (count out)
        wc (cell-size w 1 nc)
        col (if (= path sel)
              (-> config/app :editor :map-selection)
              (map-op-color (config/translate-mg-op (tree/node-operator node))))]
    (set! (.-fillStyle ctx) col)
    (.fillRect ctx x (inc (- y h)) w h)
    (if (pos? nc)
      (loop [i 0]
        (when (< i nc)
          (map-branch ctx tree (conj path i) (mm/madd i wc i 1 x) (- y h) wc h sel)
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
        {:keys [map-bg map-width map-height] :as econf} (:editor config/app)
        [vx vy vw vh :as vp] (map-focus-rect
                              (compute-viewport local)
                              width height map-width map-height)]
    (swap! local assoc :viewport vp)
    (set! (.-fillStyle ctx) map-bg)
    (set! (.-strokeStyle ctx) nil)
    (.fillRect ctx 0 0 map-width map-height)
    (map-branch ctx tree [] 0 map-height
                map-width (/ map-height tree-depth)
                selection)
    (when (== 1 tree-depth)
      (let [{:keys [map-labels map-label-font map-label-size]} econf]
        (draw-map-labels ctx map-labels
                         (/ map-width 2) (/ map-height 2)
                         map-bg map-label-font map-label-size)))
    (set! (.-strokeStyle ctx) "yellow")
    (set! (.-lineWidth ctx) 2)
    (.strokeRect ctx vx vy vw vh)))

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
  [tree]
  ((if (seq tree) dom/remove-class! dom/add-class!)
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

(defn handle-regen
  [ch bus editor local]
  (go
    (loop []
      (when (<! ch)
        (update-submit-button (:tree @editor))
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
  (let [toolbar (config/dom-component :toolbar)]
    (go
      (loop []
        (let [[_ id] (<! ch)
              {:keys [el path]} (get-in @local [:nodes id])
              {:keys [tree meshes tools start-time]} @editor]
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
                (when (and (not (seq path)) (= op :leaf))
                  (dom/set-html! el ""))
                (highlight-selected-node el (:sel-type @editor))
                (ops/enable-presets (:specs tools))
                (debug :sel-node node)
                (when-let [pid (:id node)]
                  (ops/highlight-selected-preset pid (:specs tools))
                  (ops/center-preset bus (pid (:specs tools))))
                (async/publish bus :render-scene nil)
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
              node (get-in @local [:nodes id])
              {:keys [tree meshes selection sel-type tools]} @editor]
          (when id
            (swap! local assoc :selected-id nil)
            (swap! editor assoc :selection nil :sel-type nil)
            (unhighlight-selected-node (:el node) sel-type)
            (when (and (not (seq selection)) (nil? (:op (tree/node-at tree selection))))
              (dom/set-html! (:el node) (get-in config/app [:editor :root-label])))
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
            {:keys [tree selection tools]} @editor]
        (when id
          (when (and selection (not (:active? tools)))
            (let [preset (config/preset-node id)
                  op (config/translate-mg-op (:op preset))]
              (debug :new-op id preset)
              (ops/release-op-controls local)
              (ops/highlight-selected-preset id (:specs tools))
              (ops/center-preset bus (id (:specs tools)))
              (async/publish bus :backup-tree tree)
              (ops/handle-operator
               (or op id)
               (assoc preset :id id)
               (orig-tree-node tree selection @local)
               editor local bus)
              (async/publish bus :regenerate-scene nil)))
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
                               target (loop [el (:target data)]
                                        (if-let [id (first (dom/get-attribs el ["id"]))]
                                          id (recur (dom/parent el))))
                               state [scroll (:p data) (vec2) target]]
                           (swap! local assoc :scroll-active? true)
                           (debug :touch-start target)
                           state)
             :drag-move  (when state
                           (let [[scroll p _ target] state
                                 delta  (g/- (:p data) p)
                                 scroll' (g/madd (assoc delta :y 0) 2 scroll)]
                             (reposition-viz editor local scroll')
                             (regenerate-map editor local)
                             [scroll p delta target]))
             :gesture-end (when state
                            (let [[_ p delta target] state
                                  dist (g/mag delta)]
                              (when (and (:touch? data) (< dist 20))
                                (async/publish bus :node-toggle target))
                              (swap! local assoc :scroll-active? false)
                              nil))
             nil)))))))

(defn handle-release
  [ch bus local]
  (go
    (let [_ (<! ch)
          {:keys [viz canvas op-triggers nodes subs ctrls]} @local]
      (debug :tedit-release)
      (remove-node-event-handlers bus nodes)
      ;;(ops/remove-op-triggers bus op-triggers)
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
        m-specs   [(async/event-channel canvas "mousedown" gest/mouse-gesture-start)
                   (async/event-channel canvas "mousemove" gest/mouse-gesture-move)
                   (async/event-channel canvas "mouseup" gest/gesture-end)
                   (async/event-channel canvas "touchmove" gest/touch-gesture-move)]
        v-specs   [(async/event-channel viz "mousedown" gest/mouse-gesture-start)
                   (async/event-channel viz "mousemove" gest/mouse-gesture-move)
                   (async/event-channel js/window "mouseup" gest/gesture-end)
                   (async/event-channel viz "touchstart" gest/touch-gesture-start)
                   (async/event-channel viz "touchmove" gest/touch-gesture-move)
                   (async/event-channel js/window "touchend" gest/gesture-end)]
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
    (update-submit-button (:tree @editor))
    (regenerate-viz editor local bus)
    (regenerate-map editor local)

    (handle-resize          (:window-resize subs)    bus editor local)
    (handle-regen           (:regenerate-scene subs) bus editor local)
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
