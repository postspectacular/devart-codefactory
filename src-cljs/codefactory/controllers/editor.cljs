(ns codefactory.controllers.editor
  (:require-macros
   [thi.ng.macromath.core :as mm])
  (:require
   [codefactory.config :as config]
   [codefactory.color :as col]
   [codefactory.webgl :as webgl]
   [codefactory.protocols :as proto]
   [codefactory.controllers.shared :as shared]
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.app :as app :refer [handle-event emit]]
   [thi.ng.cljs.dom :as dom]
   [thi.ng.cljs.io :as io]
   [thi.ng.cljs.route :as route]
   [thi.ng.cljs.utils :as utils]
   [thi.ng.geom.webgl.core :as gl]
   [thi.ng.geom.webgl.animator :as anim]
   [thi.ng.geom.webgl.buffers :as buf]
   [thi.ng.geom.webgl.shaders :as sh]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.core.matrix :as mat :refer [M44]]
   [thi.ng.geom.core.quaternion :as quat]
   [thi.ng.geom.core.vector :as v :refer [vec2 vec3]]
   [thi.ng.geom.aabb :as a]
   [thi.ng.geom.cuboid :as cub]
   [thi.ng.geom.rect :as r]
   [thi.ng.geom.basicmesh :as bm]
   [thi.ng.geom.ui.arcball :as arcball]
   [thi.ng.morphogen.core :as mg]
   [thi.ng.common.math.core :as m]
   ))

(def direction-ids [:x :y :z])
(def direction-idx {:x 0 :y 1 :z 2})
(def face-ids [:e :w :n :s :f :b])
(def face-idx {:e 0 :w 1 :n 2 :s 3 :f 4 :b 5})

(def mg-trees
  {:box {:seed :box
         :tree {}
         :sel nil}
   :alu {:seed :box
         :tree (let [branch (fn [[dir lpos]]
                              (mg/subdiv-inset
                               :dir :y :inset 0.05
                               :out {lpos (mg/subdiv dir 3 :out {1 nil}) 4 nil}))
                     module (mg/subdiv-inset
                             :dir :y :inset 0.4
                             :out (mapv branch [[:cols 0] [:cols 1] [:slices 2] [:slices 3]]))]
                 (mg/subdiv
                  :rows 3
                  :out [module
                        (mg/subdiv
                         :rows 3 :out {1 (mg/subdiv :cols 3 :out [nil {} nil])})
                        module]))
         :sel [2 0 0 0]}
   :hex2 {:seed :hex2
          :tree (let [hex (mg/apply-recursively (mg/reflect :dir :e) 5 [1] 1)
                      reflected-hex (mg/reflect :dir :n :out [{} hex])
                      inject #(-> hex
                                  (assoc-in (mg/child-path [1 1 0]) %)
                                  (assoc-in (mg/child-path [1 1 1 1 0]) %))
                      seed-clone (mg/reflect :dir :s :out [{} (inject reflected-hex)])]
                  (mg/reflect :dir :s :out [(inject seed-clone) (inject reflected-hex)]))
          :sel [1 1 1 1 0]}})

(defn node-operator
  [n] (cond (:op n) (:op n), n :leaf, :else :delete))

(declare instance)

(defn submit-model
  []
  (route/set-route! "submit-form"))

(defn load-model
  [queue id]
  (app/emit queue :editor-show-loader nil)
  (io/request
   :uri (str "/api/models/" id)
   :method :get
   :edn? true
   :success (fn [_ data]
              (app/emit
               queue :editor-get-model-success
               {:uuid id
                :seed-id (:seed data)
                :tree (:tree data)}))
   :error   (fn [status body]
              (app/emit queue :editor-get-model-fail [status body]))))

(defn init-model
  [queue {:keys [id seed-id]}]
  (if id
    (load-model queue id)
    (app/emit queue :editor-select-seed seed-id)))

(defn init-tree
  [state id]
  (let [state (merge
               state
               {:tree {}
                :computed-tree {[] (:seed (config/seeds (keyword id)))}
                :meshes {}
                :seed-id id
                :selection nil})]
    ;;(prn :ct (:computed-tree state))
    state))

(defn render-scene
  [state]
  (let [{:keys [gl arcball shaders proj meshes selection sel-type time sel-time]} @state
        view (arcball/get-view arcball)
        shared-unis {:view view
                     :model M44
                     :proj proj
                     :normalMat (-> (g/invert view) (g/transpose))}]
    (apply gl/clear-color-buffer gl (:bg-col config/webgl))
    (gl/clear-depth-buffer gl 1.0)
    (if selection
      (webgl/render-with-selection
       gl shaders shared-unis
       (dissoc meshes selection)
       (select-keys meshes [selection])
       (col/hex->rgb (config/operator-color sel-type))
       time sel-time)
      (webgl/render-meshes gl (shaders 1) meshes shared-unis nil))
    ;;(swap! state update-in [:time] + 0.01666)
    ))

(defn select-sub-paths
  [coll root]
  (let [croot (count root)]
    (->> (keys coll)
         (reduce
          (fn [acc k]
            (if (> (count k) croot)
              (if (every? #(= (nth % 0) (nth % 1)) (partition 2 (interleave root k)))
                (conj acc k)
                acc)
              acc))
          [])
         (select-keys coll))))

(defn delete-branch-meshes
  [gl meshes root]
  (let [d-meshes (select-sub-paths meshes root)
        meshes (apply dissoc meshes (keys d-meshes))]
    (dorun
     (map
      (fn [[id m]]
        (.deleteBuffer gl (:buffer m))) d-meshes))
    meshes))

(defn update-meshes
  [state]
  (let [{:keys [gl tree computed-tree selection meshes]} state
        path (or selection [])
        root (get computed-tree path)
        sub-tree (get-in tree (mg/child-path path))
        meshes (delete-branch-meshes gl meshes path)
        ;; TODO delete all paths in computed-tree below selected root
        branch (->> path
                    (mg/compute-tree-map* root sub-tree (transient {}))
                    (persistent!))
        meshes (->> branch
                    (reduce
                     (fn [acc [path node]]
                       (if (= :leaf (mg/classify-node-at tree path))
                         (assoc!
                          acc path
                          (-> (g/into (bm/basic-mesh) (g/faces node))
                              (gl/as-webgl-buffer-spec {:tessellate true :fnormals true})
                              (buf/make-attribute-buffers-in-spec gl gl/static-draw)))
                         acc))
                     (transient meshes))
                    (persistent!))]
    (prn :mkeys (sort (keys meshes)))
    (merge
     state
     {:computed-tree (merge computed-tree branch)
      :meshes meshes})))

(def svg-ns "http://www.w3.org/2000/svg")

(defn svg-node-id
  [path]
  (apply str (cons "node_" (interpose "_" path))))

(defn svg-node
  [parent path type x y w h total-width aspect queue]
  (let [group (dom/create-ns! svg-ns "g" parent)
        node (dom/create-ns! svg-ns "rect" group)
        id (svg-node-id path)
        cls (str "op-" (name type))]
    (condp = type
      :leaf (let [plus (dom/create-ns! svg-ns "path" group)
                  x (mm/madd w 0.5 x)
                  y (mm/madd h 0.5 y)
                  s (/ 5 total-width)]
              (dom/set-attribs!
               plus
               {:id (str id "_plus") :class cls
                :transform (str "translate(" x "," y ") scale(" s "," (* s aspect) ")")
                :d "M -1,0, L 1,0 M 0,-1 L 0,1"}))
      nil)
    (dom/set-attribs!
     node {:id id :x x :y y :width w :height h :class (str cls " selectable")})
    (.addEventListener
     node
     "click"
     (fn [e] (emit queue :editor-node-toggle [(.-target e) path])))
    [id {:svg node :path path}]))

(defn render-viz
  [state]
  state)

(defn init-viz
  [state queue]
  (let [{:keys [computed-tree]} state
        {:keys [inset]} config/editor-viz
        parent  (dom/by-id "edit-treemap")
        viz (dom/create-ns! svg-ns "svg" nil)
        _ (.insertBefore parent viz (.-firstChild parent))
        [w h] (dom/size parent)
        inset (/ inset w)
        max-size (mm/madd inset -2 1)
        aspect (/ w h)
        _ (dom/set-attribs!
           viz {:id "tree-viz" :width w :height h :viewBox "0 0 1 1"
                :preserveAspectRatio "none"})
        [id node] (svg-node viz [] :leaf inset (- max-size 0.25) max-size 0.25 w aspect queue)]
    (assoc state
      :viz-container viz
      :svg-width w
      :svg-height h
      :svg-max-size max-size
      :svg-inset inset
      :svg-aspect aspect
      :viz-nodes {id node})))

(defn resize-viz
  [state]
  (let [{:keys [viz-container svg-nodes ctrl-active?]} state
        {:keys [inset]} config/editor-viz
        [w h] (dom/size (dom/parent viz-container))
        inset (/ inset w)
        aspect (/ w h)
        state (assoc state
                :svg-width w
                :svg-height h
                :svg-inset inset
                :svg-aspect aspect)]
    (dom/set-attribs! viz-container {:width w :height h})
    (render-viz state)))

(defn init-op-slider
  [state path i op-id {:keys [label min max value step listener]}]
  (let [el-id (str "ctrl" i)
        el (dom/by-id el-id)
        el-label (dom/by-id (str el-id "-label"))
        cls (str "op-" (name op-id))]
    (dom/set-attribs! el {:class cls :min min :max max :value value :step step})
    (dom/set-text! el-label label)
    (dom/remove-class! el "hidden")
    (dom/remove-class! el-label "hidden")
    [el "change"
     (fn [e]
       (let [n (utils/parse-float (.-value el))]
         (swap!
          state
          assoc-in (cons :tree path)
          (listener n (get-in (:tree @state) (concat path [:args]))))
         (prn (:tree @state))
         (swap! state update-meshes)
         (render-scene state)))]))

(defn init-op-controls
  [state path op specs]
  (let [op-col (config/operator-color op)]
    (dom/set-style! (dom/by-id "ctrl-ok-path") #js {:fill op-col})
    (dom/set-style! (dom/by-id "ctrl-cancel-path") #js {:stroke op-col})
    (dotimes [i 3]
      (dom/add-class! (dom/by-id (str "ctrl" i)) "hidden")
      (dom/add-class! (dom/by-id (str "ctrl" i "-label")) "hidden"))
    (loop [acc [], specs specs, i 0]
      (if specs
        (recur
         (conj acc (init-op-slider state path i op (first specs)))
         (next specs)
         (inc i))
        acc))))

(defn show-op-controls*
  [{:keys [state queue default sliders op orig]}]
  (let [{:keys [viz-container tree sel-node selection]} @state
        path (mg/child-path selection)
        ctrl (dom/by-id "op-ctrl")
        listeners (init-op-controls state path op sliders)
        listeners (conj listeners
                        ["#ctrl-ok" "click" (fn [e]
                                              (emit queue :editor-commit-operator nil)
                                              (.preventDefault e))]
                        ["#ctrl-cancel" "click" (fn [e]
                                                  (emit queue :editor-cancel-operator nil)
                                                  (.preventDefault e))])]
    (app/add-listeners listeners)
    (dom/set-attribs! sel-node {:class (str "op-" (name op))})
    (dom/set-attribs! (dom/by-id (str (.-id sel-node) "_plus")) {:class "hidden"})
    (app/merge-state
     state
     {:orig-tree-value orig
      :sel-type op
      :ctrl-active? true
      :ctrl-listeners listeners
      :tree (if (seq path) (assoc-in tree path default) default)})
    (prn (:tree @state))
    (swap! state update-meshes)
    (render-scene state)
    (dom/remove-class! ctrl "hidden")))

(defn op-args-or-default
  [id node default]
  (:args (if (= id (:op node)) node default)))

(defn tree-node-at
  [tree path]
  (if (seq path) (get-in tree path) tree))

(defmulti show-op-controls
  (fn [state queue op-id] op-id))

(defmethod show-op-controls :sd
  [state queue op-id]
  (let [{:keys [viz-container tree sel-node selection]} @state
        path (mg/child-path selection)
        orig (tree-node-at tree path)
        default (mg/subdiv)
        {:keys [cols rows slices]} (op-args-or-default op-id orig default)]
    (show-op-controls*
     {:state state
      :queue queue
      :op op-id
      :sliders [{:label "columns" :min 1 :max 5 :value cols :step 1
                 :listener (fn [n {:keys [rows slices]}] (mg/subdiv :cols (int n) :rows rows :slices slices))}
                {:label "rows" :min 1 :max 5 :value rows :step 1
                 :listener (fn [n {:keys [cols slices]}] (mg/subdiv :cols cols :rows (int n) :slices slices))}
                {:label "slices" :min 1 :max 5 :value slices :step 1
                 :listener (fn [n {:keys [rows cols]}] (mg/subdiv :cols cols :rows rows :slices (int n)))}]
      :default default
      :orig orig})))

(defmethod show-op-controls :sd-inset
  [state queue op-id]
  (let [{:keys [viz-container tree sel-node selection]} @state
        path (mg/child-path selection)
        orig (tree-node-at tree path)
        default (mg/subdiv-inset :dir :x :inset 0.1)
        {:keys [dir inset]} (op-args-or-default op-id orig default)]
    (show-op-controls*
     {:state state
      :queue queue
      :op op-id
      :sliders [{:label "direction" :min 0 :max 2 :value (direction-idx dir) :step 1
                 :listener (fn [n {:keys [inset]}] (mg/subdiv-inset :dir (direction-ids (int n)) :inset inset))}
                {:label "inset" :min 0.02 :max 0.45 :value inset :step 0.001
                 :listener (fn [n {:keys [dir]}] (mg/subdiv-inset :dir dir :inset n))}]
      :default default
      :orig orig})))

(defmethod show-op-controls :reflect
  [state queue op-id]
  (let [{:keys [viz-container tree sel-node selection]} @state
        path (mg/child-path selection)
        orig (if (seq path) (get-in tree path) tree)
        default (mg/reflect :dir :e)
        {:keys [dir]} (op-args-or-default op-id orig default)]
    (show-op-controls*
     {:state state
      :queue queue
      :op op-id
      :sliders [{:label "direction" :min 0 :max 5 :value (face-idx dir) :step 1
                 :listener (fn [n _] (mg/reflect :dir (face-ids (int n))))}]
      :default default
      :orig orig})))

(defmethod show-op-controls :ext
  [state queue op-id]
  (let [{:keys [viz-container tree sel-node selection]} @state
        path (mg/child-path selection)
        orig (tree-node-at tree path)
        default (mg/extrude :dir :e :len 1.0)
        {:keys [dir len]} (op-args-or-default op-id orig default)]
    (show-op-controls*
     {:state state
      :queue queue
      :op op-id
      :sliders [{:label "direction" :min 0 :max 5 :value (face-idx dir) :step 1
                 :listener (fn [n {:keys [len]}] (mg/extrude :dir (face-ids (int n)) :len len))}
                {:label "length" :min 0.02 :max 2.0 :value len :step 0.001
                 :listener (fn [n {:keys [dir]}] (mg/extrude :dir dir :len n))}]
      :default default
      :orig orig})))

(defn rollback-op-edit
  [state render?]
  (let [{:keys [orig-tree-value tree selection]} @state
        path (mg/child-path selection)]
    (prn :rollback path orig-tree-value)
    (swap! state
           assoc
           :tree (if (seq path)
                   (assoc-in tree path orig-tree-value)
                   orig-tree-value)
           :orig-tree-value nil)
    (swap! state update-meshes)
    (when render?
      (render-scene state))))

(defmethod handle-event :editor-redraw-canvas
  [_ _ _]
  (let [state (.-state instance)
        sel (:selection @state)]
    (render-scene state)))

(defmethod handle-event :editor-node-toggle
  [[_ [node :as args]] _ q]
  (let [pn (dom/parent node)
        on? (not (neg? (.indexOf (or (dom/get-attrib pn "class") "") "selected")))]
    (emit q (if on? :editor-node-deselected :editor-node-selected) args)))

(defmethod handle-event :editor-node-selected
  [[_ [node path]] _ q]
  (let [pn (dom/parent node)
        {:keys [computed-tree tree]} @(.-state instance)
        tree-node (get-in tree (mg/child-path path))
        node-type (node-operator tree-node)]
    (prn :node-type node-type)
    (dom/add-class! (dom/by-id "toolbar") "rollon")
    (dom/set-attribs! pn {:class "selected"})
    (swap! (.-state instance) assoc :selection path :sel-type node-type :sel-node node)
    (emit q :editor-redraw-canvas nil)))

(defmethod handle-event :editor-node-deselected
  [[_ [node]] _ q]
  (let [pn (dom/parent node)]
    (dom/remove-class! (dom/by-id "toolbar") "rollon")
    (dom/add-class! (dom/by-id "op-ctrl") "hidden")
    (dom/set-attribs! pn {:class nil})
    (swap! (.-state instance) assoc :selection nil :sel-type nil :sel-node nil)
    (emit q :editor-redraw-canvas nil)))

(defmethod handle-event :editor-op-triggered
  [[_ op-id] _ q]
  (let [state (.-state instance)
        {:keys [sel-node sel-type svg-inset svg-max-size ctrl-active? ctrl-listeners]} @state]
    (when (not= op-id sel-type)
      (when ctrl-active?
        (rollback-op-edit state false)
        (app/remove-listeners ctrl-listeners))
      (dom/set-attribs! sel-node {:y svg-inset :height svg-max-size})
      (show-op-controls state q op-id))))

(defmethod handle-event :editor-commit-operator
  [_ _ q]
  )

(defmethod handle-event :editor-cancel-operator
  [_ _ q]
  (let [state (.-state instance)]
    (prn :cancel)
    (rollback-op-edit state false)
    (swap! state render-viz)
    (emit q :editor-node-deselected [(:sel-node @state)])))

(defn init-state
  [state queue initial opts]
  (let [resize-window (shared/resize-window*
                       state initial
                       (fn [state]
                         (swap! state resize-viz)
                         (render-scene state)))
        listener* (fn [id] (fn [e] (.preventDefault e) (emit queue :editor-op-triggered id)))
        dom-listeners  [["#edit-cancel" "click" (shared/cancel-module "select-seed")]
                        ["#edit-submit" "click" submit-model]
                        ["$window" "resize" resize-window]
                        ["#op-sd" "click" (listener* :sd)]
                        ["#op-sd-inset" "click" (listener* :sd-inset)]
                        ["#op-reflect" "click" (listener* :reflect)]
                        ["#op-ext" "click" (listener* :ext)]]]
    (reset!
     state
     (-> initial
         (merge
          {:dom-listeners dom-listeners
           :sel-time 0
           :time 0
           :viz-container (dom/by-id "tree-viz")})
         (webgl/init-arcball nil (fn [_] (emit queue :editor-redraw-canvas nil)))
         (init-tree (get-in opts [:params :seed-id]))
         (init-viz queue)
         (update-meshes)))
    (resize-window)
    (js/setTimeout
     (fn []
       (resize-window)
       (let [{:keys [arcball canvas-width canvas-height]} @state]
         (arcball/resize arcball canvas-width canvas-height)))
     850)
    (app/add-listeners dom-listeners)
    (init-model queue (get-in opts [:params]))))

(deftype EditorController
    [state ^:mutable shared ^:mutable queue]
  proto/PController
  (init [_ opts]
    (debug :init-editor)
    (set! shared (:state opts))
    (set! queue (:queue opts))
    (if-let [initial (webgl/init-webgl (dom/by-id "edit-canvas"))]
      (init-state state queue initial opts)
      (app/emit queue :webgl-missing nil))
    (shared/show-nav))
  (release [_]
    (let [{:keys [viz-container arcball dom-listeners]} @state]
      (debug :release-editor)
      (app/remove-listeners dom-listeners)
      (arcball/unlisten! arcball)
      (dom/remove! viz-container)
      (dom/remove-class! (dom/by-id "toolbar") "rollon")
      (dom/add-class! (dom/by-id "op-ctrl") "hidden")
      (reset! state nil)
      (set! shared nil))))

(def instance (EditorController. (atom nil) nil nil))
