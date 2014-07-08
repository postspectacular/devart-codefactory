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

(defn node-id
  [path]
  (apply str (cons "node-" (interpose "-" path))))

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
     (label "x")

     :else nil)
    (doto el
      (dom/set-attribs! {:id id})
      (dom/add-class! (str cls " selectable"))
      (dom/set-style! #js {:left (->px x)
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
  (if (pos? num) (/ (- total (* (dec num) gap)) num) total))

(defn generate-branch
  [bus viz tree]
  (let [gap (:gap config/editor)
        offx (.-offsetLeft viz)
        offy (+ (.-offsetTop viz) gap)]
    (fn gen-branch*
      [acc nodes path x y w h]
      (let [branch (tree/select-sub-paths nodes path)
            children (tree/select-direct-children branch path)
            nc (count children)
            wc (cell-size w gap nc)
            cy (- y h)
            op (tree/node-operator (tree/node-at tree path))
            acc (conj acc (make-node viz path op (+ x offx) (+ (- y h) offy) w (- h gap) bus))]
        (if (pos? nc)
          (reduce
           (fn [acc [c i]]
             (gen-branch* acc branch c (mm/madd i wc i gap x) cy wc h))
           acc (zipmap (sort (keys children)) (range)))
          acc)))))

(defn compute-required-width
  [editor]
  (let [{:keys [max-nodes max-nodes-depth]} @editor
        {:keys [gap min-size]} config/editor
        width (.-innerWidth js/window)
        cw (cell-size width gap max-nodes)]
    (if (< cw min-size)
      (do (debug :too-small cw) width)
      width)))

(defn regenerate-viz
  [editor local bus]
  (let [{:keys [viz nodes]} @local
        {:keys [node-cache tree tree-depth]} @editor
        width (compute-required-width editor)
        height (.-clientHeight (dom/parent viz))
        node-height (cell-size height (:gap config/editor) tree-depth)
        layout (generate-branch bus viz tree)]
    (set! (.-innerHtml viz) "")
    (remove-node-event-handlers bus nodes)
    (swap! local assoc :nodes (layout {} node-cache [] 10 height (- width 20) node-height))))

(defn init
  [editor bus]
  (let [{:keys [inset gap]} config/editor-viz
        parent          (dom/by-id "edit-treemap")
        viz             (dom/create! "div" nil)
        node-toggle     (async/subscribe bus :node-toggle)
        node-selected   (async/subscribe bus :node-selected)
        node-deselected (async/subscribe bus :node-deselected)
        release         (async/subscribe bus :release-editor)
        resize          (async/subscribe bus :window-resize)
        local           (atom
                         {:subs  {:node-selected node-selected
                                  :node-deselected node-deselected
                                  :node-toggle node-toggle
                                  :node-release release}
                          :viz   viz
                          :nodes {}
                          :selected-id nil})]
    (.insertBefore parent viz (.-firstChild parent))
    (dom/set-attribs! viz {:id "viz-container"})
    (regenerate-viz editor local bus)

    (go
      (loop []
        (when (<! resize)
          (debug :tedit-resize)
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
            (recur)))))

    (go
      (loop []
        (let [[_ [id render?]] (<! node-deselected)
              node (get-in @local [:nodes id])]
          (when id
            (debug :node-deselected id node)
            (swap! local assoc :selected-id nil)
            (dom/remove-class! (:el node) "selected")
            (when render?)
            (recur)))))

    (go
      (let [_ (<! release)
            {:keys [viz nodes subs]} @local]
        (debug :tedit-release)
        (dom/remove! viz)
        (remove-node-event-handlers bus nodes)
        (async/unsubscribe-and-close-many bus subs)
        (reset! local nil)))

    ))
