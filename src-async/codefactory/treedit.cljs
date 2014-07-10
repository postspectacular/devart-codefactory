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
   [thi.ng.common.math.core :as m]))

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

(defn make-node
  [parent path op x y w h bus]
  (let [el (dom/create! "div" parent)
        id (node-id path)
        cls (str "op-" (name op))
        [ch handler] (dom/event-channel el "click")
        label (fn [l]
                (set! (.-innerText el) l)
                (dom/set-style! el #js {:line-height (->px h)}))]

    (cond
     (= :leaf op)
     (label (if (empty? path) "TAP TO BEGIN" "+"))
     (= :delete op)
     (label "X")
     :else nil)

    (doto el
      (dom/set-attribs! {:id id})
      (dom/add-class! cls)
      (dom/set-style!
       #js {:left (->px x)
            :top  (->px y)
            :width (->px w)
            :height (->px h)}))

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
  [bus viz tree local]
  (let [{:keys [scroll]} @local
        gap (:gap config/editor)
        off (g/+ scroll (.-offsetLeft viz) (- (.-innerHeight js/window) 270))]
    (fn gen-branch*
      [acc nodes path x y w h]
      (let [branch (tree/select-sub-paths nodes path)
            children (tree/select-direct-children branch path)
            nc (count children)
            wc (cell-size w gap nc)
            cy (- y h gap)
            op (tree/node-operator (tree/node-at tree path))
            acc (conj acc (make-node viz path op (+ x (:x off)) (+ (- y h) (:y off)) w h bus))]
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
            (recur (pop path) (tree/num-children-at tree path) width)
            width)))
      width)))

(defn regenerate-viz
  [editor local bus]
  (let [{:keys [viz nodes width node-height]} @local
        {:keys [node-cache tree tree-depth]} @editor
        {:keys [gap margin height]} config/editor
        layout (generate-branch bus viz tree local)]
    (debug :height height node-height)
    (set! (.-innerHtml viz) "")
    (remove-node-event-handlers bus nodes)
    (swap!
     local assoc
     :nodes (layout {} node-cache [] margin height width node-height))))

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

(defn map-branch
  [ctx tree path x y w h sel]
  (let [{:keys [op out] :as node} (tree/node-at tree path)
        nc (count out)
        wc (cell-size w 1 nc)
        col (if (= path sel)
              (:map-selection config/editor)
              (config/operator-color (tree/node-operator node)))] ;; TODO highlight sel
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
        {:keys [map-bg map-width map-height]} config/editor
        [vx vy vw vh] (map-focus-rect
                       (compute-viewport local)
                       width height map-width map-height)]
    (swap! local assoc-in [:map :viewport] [vx vy vw vh])
    (set! (.-fillStyle ctx) map-bg)
    (set! (.-strokeStyle ctx) nil)
    (.fillRect ctx 0 0 map-width map-height)
    (map-branch ctx tree [] 0 map-height map-width (/ map-height tree-depth) selection)
    (when (== 1 tree-depth)
      (set! (.-fillStyle ctx) map-bg)
      (set! (.-font ctx) "14px \"Abel\" sans-serif")
      (set! (.-textAlign ctx) "center")
      (set! (.-textBaseline ctx) "middle")
      (.fillText ctx "CODE OVERVIEW" (/ map-width 2) (/ map-height 2)))
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
        sx (m/map-interval-clamped x 0 (- map-width vw) 0 (- (- width viz-width)))]
    (swap! local assoc-in [:scroll :x] sx)
    (resize-viz editor local)
    (regenerate-map editor local)))

(defn init
  [editor bus]
  (let [{:keys [gap margin height map-width map-height]} config/editor
        {:keys [tree-depth]} @editor
        toolbar         (dom/by-id "toolbar")
        parent          (dom/by-id "edit-treemap")
        viz             (dom/insert! (dom/create! "div" nil {:id "viz-container"}) parent)
        canvas          (dom/insert!
                         (dom/create! "canvas" nil {:id "viz-map" :width map-width :height map-height})
                         parent)
        node-toggle     (async/subscribe bus :node-toggle)
        node-selected   (async/subscribe bus :node-selected)
        node-deselected (async/subscribe bus :node-deselected)
        release         (async/subscribe bus :release-editor)
        resize          (async/subscribe bus :window-resize)
        mdown           (dom/event-channel canvas "mousedown")
        mmove           (dom/event-channel canvas "mousemove")
        mup             (dom/event-channel canvas "mouseup")
        map-inputs      (mapv first [mdown mmove mup])
        width           (compute-required-width editor)
        max-width       (mm/madd margin -2 (.-innerWidth js/window))
        local           (atom
                         {:subs  {:node-selected node-selected
                                  :node-deselected node-deselected
                                  :node-toggle node-toggle
                                  :release-editor release
                                  :window-resize resize}
                          :events [mdown mmove mup]
                          :map {:ctx (.getContext canvas "2d")}
                          :viz   viz
                          :scroll (vec2)
                          :scrolling? (> width max-width)
                          :nodes {}
                          :selected-id nil
                          :width width
                          :height map-height
                          :node-height (cell-size height gap tree-depth)})]
    (regenerate-viz editor local bus)
    (regenerate-map editor local)

    (go
      (loop []
        (when (<! resize)
          (debug :tedit-resize)
          (resize-viz editor local)
          (regenerate-map editor local)
          (recur))))

    (go
      (loop []
        (let [[_ id] (<! node-toggle)
              sel (:selected-id @local)]
          (when id
            (debug :node-toggle id)
            (if (= id sel)
              (async/publish bus :node-deselected [id true])
              (do
                (when sel (async/publish bus :node-deselected [sel false]))
                (async/publish bus :node-selected id)))
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
      (loop [down? false]
        (let [[e ch] (alts! map-inputs)]
          (when e
            (if (or (= ch (map-inputs 0))
                    (and down? (= ch (map-inputs 1))))
              (do (scroll-viewport editor local (- (.-clientX e) (.-offsetLeft canvas) 10))
                  (recur true))
              (recur false))))))

    (go
      (let [_ (<! release)
            {:keys [viz nodes subs]} @local]
        (debug :tedit-release)
        (dom/remove! viz)
        (dom/remove! canvas)
        (dom/remove-class! (dom/by-id "toolbar") "rollon")
        (remove-node-event-handlers bus nodes)
        (dorun (map dom/destroy-event-channel (:events @local)))
        (async/unsubscribe-and-close-many bus subs)
        (reset! local nil)))

    ))
