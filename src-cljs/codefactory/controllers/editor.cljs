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
    (swap! state update-in [:time] + 0.01666)))

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
        _ (prn :path path)
        _ (prn :root root)
        _ (prn :sub-tree sub-tree)
        meshes (delete-branch-meshes gl meshes path)
        ;; TODO remove old attrib buffers
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
     node {:id id :x x :y y :width w :height h
           :class cls})
    (.addEventListener
     node
     "click"
     (fn [e] (emit queue :editor-node-toggle [(.-target e) path])))
    [id {:svg node :path path}]))

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
  (let [{:keys [viz-container svg-nodes]} state
        {:keys [inset]} config/editor-viz
        [w h] (dom/size (dom/parent viz-container))
        inset (/ inset w)
        aspect (/ w h)]
    (dom/set-attribs! viz-container {:width w :height h})
    (assoc state
      :svg-width w
      :svg-height h
      :svg-inset inset
      :svg-aspect aspect)))

(defmulti show-op-controls
  (fn [state op-id] op-id))

(defmethod show-op-controls :sd
  [state op-id]
  (let [{:keys [viz-container tree sel-node selection]} @state
        path (mg/child-path selection)
        orig (if (seq path) (get-in tree path) tree)
        {:keys [cols rows slices]} (:args orig)
        ctrl (dom/by-id "op-ctrl")
        c0 (dom/by-id "ctrl0")
        c1 (dom/by-id "ctrl1")
        c2 (dom/by-id "ctrl2")
        listener (fn [el subdiv]
                   (fn [e]
                     (let [n (utils/parse-int (.-value el))
                           tree (:tree @state)]
                       (swap! state assoc-in (concat [:tree] path) (subdiv n (get-in tree (concat path [:args]))))
                       (prn (:tree @state))
                       (swap! state update-meshes)
                       (render-scene state))))
        listeners [[c0 "change" (listener c0 (fn [n {:keys [rows slices]}] (mg/subdiv :cols n :rows rows :slices slices)))]
                   [c1 "change" (listener c1 (fn [n {:keys [cols slices]}] (mg/subdiv :cols cols :rows n :slices slices)))]
                   [c2 "change" (listener c2 (fn [n {:keys [rows cols]}] (mg/subdiv :cols cols :rows rows :slices n)))]]]
    (app/add-listeners listeners)
    (dom/set-attribs! sel-node {:class (str "op-" (name op-id))})
    (dom/set-attribs! (dom/by-id (str (.-id sel-node) "_plus")) {:class "hidden"})
    (dom/set-attribs! c0 {:class "op-sd" :min 1 :max 5 :value cols :step 1})
    (dom/set-attribs! c1 {:class "op-sd" :min 1 :max 5 :value rows :step 1})
    (dom/set-attribs! c2 {:class "op-sd" :min 1 :max 5 :value slices :step 1})
    (dom/set-text! (dom/by-id "ctrl0-label") "columns")
    (dom/set-text! (dom/by-id "ctrl1-label") "rows")
    (dom/set-text! (dom/by-id "ctrl1-label") "slices")
    (app/merge-state
     state
     {:orig-tree-value orig
      :sel-type op-id
      :ctrl-listeners listeners
      :tree (if (seq path) (assoc-in tree path (mg/subdiv)) (mg/subdiv))})
    (prn (:tree @state))
    (swap! state update-meshes)
    (render-scene state)
    (dom/remove-class! ctrl "hidden")))

(defmethod show-op-controls :sd-inset
  [state op-id]
  (let [{:keys [viz-container tree svg-nodes selection]} @state
        ctrl (dom/by-id "op-ctrl")]
    (dom/set-attribs! (dom/by-id "ctrl0") {:class "op-sd-inset" :min 1 :max 5 :value 1 :step 1})
    (dom/set-attribs! (dom/by-id "ctrl1") {:class "op-sd-inset" :min 1 :max 5 :value 1 :step 1})
    (dom/set-attribs! (dom/by-id "ctrl2") {:class "op-sd-inset" :min 1 :max 5 :value 1 :step 1})
    (dom/remove-class! ctrl "hidden")))

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
  [[_ [node path]] _ q]
  (let [pn (dom/parent node)]
    (dom/remove-class! (dom/by-id "toolbar") "rollon")
    (dom/add-class! (dom/by-id "op-ctrl") "hidden")
    (dom/set-attribs! pn {:class nil})
    (swap! (.-state instance) assoc :selection nil)
    (emit q :editor-redraw-canvas nil)))

(defmethod handle-event :editor-op-triggered
  [[_ op-id] _ q]
  (let [{:keys [sel-node svg-inset svg-max-size]} @(.-state instance)]
    (dom/set-attribs! sel-node {:y svg-inset :height svg-max-size})
    (show-op-controls (.-state instance) op-id)))

(defn init-state
  [state queue initial opts]
  (let [resize-window (shared/resize-window*
                       state initial
                       (fn [state]
                         (swap! state resize-viz)
                         (render-scene state)))
        dom-listeners  [["#edit-cancel" "click" (shared/cancel-module "select-seed")]
                        ["$window" "resize" resize-window]
                        ["#op-sd" "click" (fn [e] (.preventDefault e) (emit queue :editor-op-triggered :sd))]
                        ["#op-sd-inset" "click" (fn [e] (.preventDefault e) (emit queue :editor-op-triggered :sd-inset))]
                        ]]
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
      (app/emit queue :webgl-missing nil)))
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
