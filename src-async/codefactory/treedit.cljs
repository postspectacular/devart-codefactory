(ns codefactory.treedit
  (:require-macros
   [cljs.core.async.macros :as asm :refer [go]]
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
   [thi.ng.cljs.utils :as utils]
   [thi.ng.cljs.dom :as dom]
   [thi.ng.cljs.gestures :as gest]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.core.matrix :as mat :refer [M44]]
   [thi.ng.geom.core.vector :as v :refer [vec2 vec3]]
   [thi.ng.geom.rect :as r]
   [thi.ng.common.math.core :as m]))

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
  [parent path op x y w h bus sel min-label-width config]
  (let [el (dom/create! "div" parent)
        id (node-id path)
        cls (str "op-" (name op))
        econf (:editor config)
        [ch handler] (dom/event-channel el "click")]

    (cond
     (= :leaf op)
     (dom/set-html! el (if (empty? path) (:root-label econf) "+"))

     (= :delete op)
     (let [svg (dom/create-ns! svg-ns "svg" el
                               {:width "100%" :height "100%"
                                :viewBox "0 0 1 1"
                                :preserveAspectRatio "none"
                                :class "op-delete"})]
       (dom/create-ns! svg-ns "path" svg {:d "M0,0 L1,1 M0,1 L1,0"})
       (dom/create-ns! svg-ns "rect" svg {:x 0 :y 0 :width 1 :height 1}))
     :else
     (when (>= w min-label-width)
       (dom/set-text! el (get-in config [:operators op :label]))))

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

(defn generate-branch
  [bus viz tree scroll sel config]
  (let [{:keys [gap height margin-bottom min-label-width]} (:editor config)
        [offx offy] (g/+ scroll
                         (.-offsetLeft viz)
                         (mm/sub (.-innerHeight js/window) height margin-bottom))]
    (fn gen-branch*
      [acc path x y w h]
      (let [node (tree/node-at tree path)
            nc (count (:out node))
            wc (cell-size w gap nc)
            cy (mm/sub y h gap)
            op (config/translate-mg-op config (tree/node-operator node))
            acc (conj acc
                      (make-node viz path op
                                 (+ x offx) (+ (- y h) offy) w h
                                 bus sel min-label-width config))]
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
  [nodes tree gap [offx offy]]
  (fn resize-branch*
    [path x y w h]
    (let [el (:el (nodes (node-id path)))
          nc (tree/num-children-at tree path)
          cy (mm/sub y h gap)
          wc (cell-size w gap nc)]
      (dom/set-style! el #js {:left (->px (+ x offx))
                              :top (->px (+ (- y h) offy))
                              :width (->px w)})
      (if (pos? nc)
        (loop [i 0]
          (when (< i nc)
            (resize-branch* (conj path i) (mm/madd i wc i gap x) cy wc h)
            (recur (inc i))))))))

(defn resize-viz
  [editor local]
  (let [{:keys [viz nodes scroll config]} @local
        {:keys [node-cache tree tree-depth]} @editor
        {:keys [gap margin margin-bottom height]} (:editor config)
        width  (compute-required-width editor)
        node-height (cell-size height gap tree-depth)
        offset (g/+ scroll
                    (.-offsetLeft viz)
                    (mm/sub (.-innerHeight js/window) height margin-bottom))]
    ((resize-branch nodes tree gap offset) [] margin height width node-height)
    (swap! local assoc :width width :node-height node-height)))

(def map-op-color
  (memoize
   (fn [config op]
     (debug :col-op op)
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
              (map-op-color config (config/translate-mg-op config (tree/node-operator node))))]
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
    (.strokeRect ctx vx vy vw vh)))

(defn scroll-viewport
  [editor local x]
  (let [{:keys [width viewport config]} @local
        {:keys [map-width map-height]} (:editor config)
        [_ _ viz-width] (compute-viewport local)
        [_ _ vw] viewport
        sx (m/map-interval-clamped (mm/madd vw -0.5 x)
                                   0 (- map-width vw)
                                   0 (- (- width viz-width)))]
    (swap! local assoc-in [:scroll :x] sx)
    (resize-viz editor local)
    (regenerate-map editor local)))

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
  [ch bus editor local config]
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
             :sel-type (->> path
                            (tree/node-at tree)
                            (tree/node-operator)
                            (config/translate-mg-op config))
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
          (ops/remove-op-controls local)
          (ops/handle-operator op editor local bus)
          (async/publish bus :user-action nil)
          (recur))))))

(defn handle-commit-op
  [ch bus local]
  (go
    (loop []
      (when (<! ch)
        (ops/remove-op-controls local)
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
           :sel-type (tree/node-operator orig-edit-node))
          (swap! editor tree/update-meshes true)
          (ops/remove-op-controls local)
          (async/publish bus :regenerate-scene nil)
          (async/publish bus :user-action nil)
          (recur))))))

(defn handle-viz-interactions
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

(defn handle-release
  [ch bus local]
  (go
    (let [_ (<! ch)
          {:keys [viz canvas op-triggers nodes subs ctrls]} @local]
      (debug :tedit-release)
      (dom/remove! viz)
      (dom/remove! canvas)
      (dom/remove-class! (dom/by-id "toolbar") "rollon")
      (remove-node-event-handlers bus nodes)
      (ops/remove-op-triggers bus op-triggers)
      (ops/remove-op-controls local)
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
        e-specs [(dom/event-channel canvas "mousedown" gest/mouse-gesture-start)
                 (dom/event-channel canvas "mousemove" gest/mouse-gesture-move)
                 (dom/event-channel canvas "mouseup" gest/gesture-end)
                 (dom/event-channel canvas "touchmove" gest/touch-gesture-move)]
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
                  :op-triggers (ops/init-op-triggers bus config)})]
    (debug :init-tedit)
    (regenerate-viz editor local bus)
    (regenerate-map editor local)

    (handle-resize          (:window-resize subs)    bus editor local)
    (handle-regen           (:regenerate-scene subs) bus editor local)
    (handle-node-toggle     (:node-toggle subs)      bus local)
    (handle-node-selected   (:node-selected subs)    bus editor local config)
    (handle-node-deselected (:node-deselected subs)  bus editor local)
    (handle-op-triggered    (:op-triggered subs)     bus editor local)
    (handle-commit-op       (:commit-operator subs)  bus local)
    (handle-cancel-op       (:cancel-operator subs)  bus editor local)
    (handle-release         (:release-editor subs)   bus local)
    (handle-viz-interactions events bus editor local)))
