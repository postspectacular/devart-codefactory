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
    [(:x scroll)
     (:y scroll)
     (mm/madd margin -2 (- (.-innerWidth js/window) map-width))
     height]))

(defn compute-required-width
  [editor]
  (let [{:keys [max-nodes-path tree]} @editor
        {:keys [gap margin min-size map-width]} config/editor
        width (mm/madd margin -2 (- (.-innerWidth js/window) map-width))
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
  [ctx tree path x y w h]
  (let [{:keys [op out] :as node} (tree/node-at tree path)
        nc (count out)
        wc (cell-size w 1 nc)
        col (config/operator-color (tree/node-operator node))] ;; TODO highlight sel
    (set! (.-fillStyle ctx) col)
    (.fillRect ctx x (inc (- y h)) w h)
    (if (pos? nc)
      (loop [i 0]
        (when (< i nc)
          (map-branch ctx tree (conj path i) (mm/madd i wc i 1 x) (- y h) wc h)
          (recur (inc i)))))))

(defn regenerate-map
  [editor local]
  (let [{:keys [ctx]} (:map @local)
        {:keys [width height]} @local
        {:keys [tree tree-depth]} @editor
        {:keys [map-bg map-width map-height]} config/editor
        [vx vy vw vh] (compute-viewport local)
        vx' (m/map-interval vx 0 vw 0 map-width)
        vy' (m/map-interval vy 0 vh 0 map-height)
        vw' (m/map-interval vw 0 width 0 map-width)
        vh' (m/map-interval vh 0 height 0 map-height)]
    (set! (.-fillStyle ctx) map-bg)
    (set! (.-strokeStyle ctx) nil)
    (.fillRect ctx 0 0 map-width map-height)
    (map-branch ctx tree [] 0 map-height map-width (/ map-height tree-depth))
    (set! (.-strokeStyle ctx) "yellow")
    (.strokeRect ctx vx' vy' vw' vh')))

(defn init
  [editor bus]
  (let [{:keys [gap margin height map-width map-height]} config/editor
        {:keys [tree-depth]} @editor
        parent          (dom/by-id "edit-treemap")
        viz             (dom/create! "div" nil)
        canvas          (dom/create! "canvas" nil)
        node-toggle     (async/subscribe bus :node-toggle)
        node-selected   (async/subscribe bus :node-selected)
        node-deselected (async/subscribe bus :node-deselected)
        release         (async/subscribe bus :release-editor)
        resize          (async/subscribe bus :window-resize)
        width           (compute-required-width editor)
        max-width       (mm/madd margin -2 (.-innerWidth js/window))
        local           (atom
                         {:subs  {:node-selected node-selected
                                  :node-deselected node-deselected
                                  :node-toggle node-toggle
                                  :release-editor release
                                  :window-resize resize}
                          :map {:ctx (.getContext canvas "2d")}
                          :viz   viz
                          :scroll (vec2)
                          :scrolling? (> width max-width)
                          :nodes {}
                          :selected-id nil
                          :width width
                          :height map-height
                          :node-height (cell-size height gap tree-depth)})]
    (.insertBefore parent canvas (.-firstChild parent))
    (.insertBefore parent viz (.-firstChild parent))
    (dom/set-attribs! canvas {:id "viz-map" :width map-width :height map-height})
    (dom/set-attribs! viz {:id "viz-container"})
    (regenerate-viz editor local bus)
    (regenerate-map editor local)

    (go
      (loop []
        (when (<! resize)
          (debug :tedit-resize)
          (resize-viz editor local)
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
              node (get-in @local [:nodes id])]
          (when id
            (debug :node-selected id node)
            (swap! local assoc :selected-id id)
            (dom/add-class! (:el node) "selected")
            (dom/add-class! (dom/by-id "toolbar") "rollon")
            (recur)))))

    (go
      (loop []
        (let [[_ [id render?]] (<! node-deselected)
              node (get-in @local [:nodes id])]
          (when id
            (debug :node-deselected id node)
            (swap! local assoc :selected-id nil)
            (dom/remove-class! (:el node) "selected")
            (dom/remove-class! (dom/by-id "toolbar") "rollon")
            (when render?)
            (recur)))))

    (go
      (let [_ (<! release)
            {:keys [viz nodes subs]} @local]
        (debug :tedit-release)
        (dom/remove! viz)
        (dom/remove! canvas)
        (dom/remove-class! (dom/by-id "toolbar") "rollon")
        (remove-node-event-handlers bus nodes)
        (async/unsubscribe-and-close-many bus subs)
        (reset! local nil)))

    ))

;;@migurski as verbose & eloquent (t)his description is, it too seems all encompassing of (not just) contemporary society...
;;@migurski it's a valid political/cultural critique, but hardly one which is or requires "new" in its title/mission
;;@migurski most societies have been dominated by luddites & technology has always been a tool of power, hence our continous use/dev of it
;;@migurski i'd rather see a "new edu" movement which focuses on
