(ns codefactory.editor.node-edit
  (:require-macros
   [cljs.core.async.macros :refer [go]]
   [thi.ng.macromath.core :as mm])
  (:require
   [cljs.core.async :refer [<! alts! timeout]]
   [codefactory.editor.tree :as tree]
   [codefactory.editor.operators :as ops]
   [codefactory.editor.toolbar :as tools]
   [codefactory.editor.layout :as layout]
   [codefactory.editor.overview :as overview]
   [codefactory.config :as config]
   [codefactory.color :as col]
   [codefactory.common :as common]
   [thi.ng.cljs.async :as async]
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.utils :as utils :refer [->px]]
   [thi.ng.cljs.dom :as dom]
   [thi.ng.cljs.gestures :as gest]
   [thi.ng.geom.ui.arcball :as arcball]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.core.vector :as v :refer [vec2]]
   [thi.ng.geom.core.quaternion :as q]
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
   (dom/create-dom!
    [:svg {:width "100%" :height "100%"
           :viewBox "0 0 1 1"
           :preserveAspectRatio "none"
           :class "op-delete"}
     [:path {:d "M0.01,0.01 L0.99,0.99 M0.01,0.99 L0.99,0.01"}]
     [:rect {:x 0.01 :y 0.01 :width 0.98 :height 0.98}]]
    el)

   :else
   (dom/create-dom!
    [:span
     (if (>= width min-width) (-> config/app :operators op :label) "\u00A0")]
    el)))

(defn make-node
  [parent path op x y w h ox oy bus sel event?]
  (let [id (node-id path)
        el (dom/create-dom!
            [:div {:id id
                   :class (str (ops/op-class op) (if (= path sel) " selected" " deselected"))
                   :style {:left   (->px (+ x ox))
                           :top    (->px (+ y oy))
                           :width  (->px w)
                           :height (->px h)}}]
            parent)]
    (if event?
      (dom/add-listeners [[el "click" #(async/publish bus :node-toggle id)]]))
    [id {:el el :x x :y y :w w :h h :path path}]))

(defn reposition-branch
  [nodes tree sizes gap [offx offy]]
  (fn repos-branch*
    [path x]
    (let [el (:el (nodes (node-id path)))
          nc (tree/num-children-at tree path)]
      (dom/set-style! el {:left (->px (+ x offx))})
      (if (pos? nc)
        (loop [i 0, x x]
          (if (< i nc)
            (let [path' (conj path i)]
              (repos-branch* path' x)
              (recur (inc i) (+ x (sizes path'))))))))))

(defn reposition-viz
  [editor local scroll]
  (let [{:keys [viz nodes layout-nodes width]} @local
        {:keys [node-cache tree]} @editor
        {:keys [gap margin map-width]} (:editor config/app)
        min (- (layout/viewport-width) width)
        scroll (assoc scroll :x (m/clamp (:x scroll) min 0))
        offset (layout/scroll-offset scroll viz)]
    (swap! local assoc :scroll scroll)
    ((reposition-branch nodes tree layout-nodes gap offset) [] margin)))

(defn center-node
  [editor local]
  (let [{:keys [width nodes selected-id]} @local
        {:keys [w x]} (nodes selected-id)
        vp-width (layout/viewport-width)
        min (- vp-width width)
        x' (+ (- (- x) (/ w 2)) (/ vp-width 2))
        x' (m/clamp x' min 0)]
    (reposition-viz editor local (vec2 x' 0))))

(defn generate-branch
  [bus parent tree sizes depth scroll sel intro?]
  (let [{:keys [gap min-label-width]} (:editor config/app)
        [offx offy] (layout/scroll-offset scroll parent)
        event? (not intro?)]
    (fn gen-branch*
      [acc path x y h]
      (let [node (tree/node-at tree path)
            nc   (count (:out node))
            w    (sizes path)
            cy   (mm/sub y h gap)
            op   (config/translate-mg-op (tree/node-operator node))
            [x y w h] (if (and (== 1 depth) (empty? path))
                        [x (- y 4) (- w 4) (- h 4)] [x y (- w gap) h])
            node (make-node parent path op x (- y h) w h offx offy bus sel event?)
            acc  (conj acc node)]
        (set-node-label (-> node second :el) path op depth w min-label-width intro?)
        (if (pos? nc)
          (loop [acc acc, i 0, x x]
            (if (< i nc)
              (let [path' (conj path i)]
                (recur (gen-branch* acc path' x cy h) (inc i) (+ x (sizes path'))))
              acc))
          acc)))))

(defn regenerate-viz
  [editor local bus]
  (tree/update-stats editor)
  (let [{:keys [viz nodes width scroll]} @local
        {:keys [tree tree-depth node-cache selection intro-active?]} @editor
        {:keys [gap margin height min-size]} (:editor config/app)
        [width' sizes] (layout/compute-layout tree node-cache min-size)
        scroll (if (< width' (m/abs (:x scroll))) (vec2) scroll)
        node-height (layout/cell-size height gap tree-depth)
        layout (generate-branch bus viz tree sizes tree-depth scroll selection intro-active?)]
    (-> viz
        (dom/clear!)
        (dom/set-attribs! {:class (str "depth" tree-depth)}))
    (swap!
     local assoc
     :width        width'
     :node-height  node-height
     :scroll       scroll
     :nodes        (layout {} [] margin height node-height)
     :layout-nodes sizes)
    (if selection
      (center-node editor local))))

(defn resize-branch
  [nodes tree sizes depth gap [offx offy]]
  (fn resize-branch*
    [path x y h]
    (let [el (:el (nodes (node-id path)))
          nc (tree/num-children-at tree path)
          w  (sizes path)
          [x y w h] (if (and (== 1 depth) (empty? path))
                      [x (- y 4) (- w 4) (- h 4)] [x y (- w gap) h])
          cy (mm/sub y h gap)]
      (dom/set-style!
       el {:left  (->px (+ x offx))
           :top   (->px (+ (- y h) offy))
           :width (->px w)})
      (if (pos? nc)
        (loop [i 0, x x]
          (if (< i nc)
            (let [path' (conj path i)]
              (resize-branch* path' x cy h)
              (recur (inc i) (+ x (sizes path'))))))))))

(defn resize-viz
  [editor local]
  (let [{:keys [viz nodes scroll]}           @local
        {:keys [node-cache tree tree-depth]} @editor
        {:keys [gap margin height min-size]} (:editor config/app)
        [width sizes] (layout/compute-layout tree node-cache min-size)
        node-height   (layout/cell-size height gap tree-depth)
        offset        (layout/scroll-offset scroll viz)]
    ((resize-branch nodes tree sizes tree-depth gap offset)
     [] margin height node-height)
    (swap!
     local assoc
     :layout-nodes sizes
     :width width
     :node-height node-height)))

(defn scroll-viewport
  [editor local x]
  (let [{:keys [width viewport scroll]} @local
        {:keys [map-width map-height]} (:editor config/app)
        [_ _ viz-width] (layout/compute-viewport local)
        [_ _ vw] viewport
        sx (m/map-interval-clamped
            (mm/madd vw -0.5 x)
            0 (- map-width vw)
            0 (- (- width viz-width)))]
    (reposition-viz editor local (assoc scroll :x sx))
    (overview/regenerate editor local)))

(defn update-submit-button
  [depth]
  ((if (and (number? depth) (>= depth (-> config/app :editor :min-submit-depth)))
     dom/remove-class!
     dom/add-class!)
   (config/dom-component :edit-continue) "hidden"))

(defn handle-resize
  [ch bus editor local]
  (let [ch (async/throttle ch 500)]
    (go
      (loop []
        (when (<! ch)
          (resize-viz editor local)
          (overview/regenerate editor local)
          (async/publish bus :user-action nil)
          (recur))))))

(defn handle-regenerate
  [ch bus editor local]
  (go
    (loop []
      (when (<! ch)
        (let [{:keys [tree-depth bounds arcball]} @editor]
          (update-submit-button tree-depth)
          (arcball/update-zoom-range arcball bounds)
          (regenerate-viz editor local bus)
          (overview/regenerate editor local)
          (async/publish bus :render-scene nil)
          (recur))))))

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
                (highlight-selected-node el (->> @local :nodes vals (mapv :el)))
                (tools/enable-presets (:specs tools))
                (debug :sel-node node)
                (if-not (#{:leaf :delete} op)
                  (async/publish bus :op-triggered (:id node))
                  (async/publish bus :render-scene nil))
                (async/publish bus :user-action nil)
                (overview/regenerate editor local))
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
            (unhighlight-selected-node el (->> @local :nodes vals (mapv :el)))
            (if (= op :leaf)
              (if (seq selection)
                (if (< tree-depth 3)
                  (dom/set-html! el (-> config/app :editor :ftu-label)))
                (-> el
                    (dom/set-html! (get-in config/app [:editor :root-label]))
                    (dom/add-class! "op-root"))))
            (tools/disable-presets (:specs tools))
            (ops/release-op-controls local)
            (when render?
              (swap!
               editor assoc
               :display-meshes (tree/filter-leaves-and-selection meshes tree nil))
              (async/publish bus :render-scene nil)
              (overview/regenerate editor local))
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
              (tools/highlight-selected-preset id (:specs tools))
              (tools/center-preset bus (id (:specs tools)))
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
            (tools/disable-presets (:specs tools))
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
                (scroll-viewport editor local (- (:x (:p data)) (.-offsetLeft canvas) 10))
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
                             (overview/regenerate editor local)
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
    (<! ch)
    (let [{:keys [viz canvas op-triggers nodes subs ctrls]} @local]
      (debug :tedit-release)
      (dom/clear! viz)
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
        subs      (-> (async/subscription-channels
                       bus [:node-toggle :node-selected :node-deselected
                            :commit-operator :cancel-operator
                            :op-triggered :undo-triggered
                            :regenerate-scene
                            :release-editor])
                      (assoc :window-resize
                        (async/subscribe bus :window-resize (async/sliding-channel 1))))
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
    (overview/regenerate editor local)

    (handle-resize           (:window-resize subs)    bus editor local)
    (handle-regenerate       (:regenerate-scene subs) bus editor local)
    (handle-node-toggle      (:node-toggle subs)      bus local)
    (handle-node-selected    (:node-selected subs)    bus editor local)
    (handle-node-deselected  (:node-deselected subs)  bus editor local)
    (handle-op-triggered     (:op-triggered subs)     bus editor local)
    (handle-undo             (:undo-triggered subs)   bus editor local)
    (handle-commit-op        (:commit-operator subs)  bus local)
    (handle-cancel-op        (:cancel-operator subs)  bus editor local)
    (handle-release          (:release-editor subs)   bus local)
    (handle-map-interactions m-events bus editor local)
    (handle-viz-scroll       viz v-events bus editor local)))
