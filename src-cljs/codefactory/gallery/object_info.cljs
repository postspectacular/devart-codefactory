(ns codefactory.gallery.object-info
  (:require-macros
   [cljs.core.async.macros :as asm :refer [go]]
   [thi.ng.macromath.core :as mm])
  (:require
   [codefactory.config :as config]
   [codefactory.common :as common]
   [codefactory.gallery.item :as item]
   [thi.ng.cljs.async :as async]
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.route :as route]
   [thi.ng.cljs.utils :as utils :refer [->px]]
   [thi.ng.cljs.io :as io]
   [thi.ng.cljs.dom :as dom]
   [thi.ng.common.math.core :as m :refer [TWO_PI]]
   [thi.ng.geom.core.vector :refer [vec2]]
   [cljs.core.async :refer [<! timeout]]
   [clojure.string :as str]))

(defn load-graph
  [bus id]
  (common/loader-html
   "Loading artwork details..."
   (config/dom-component :gallery-info-main))
  (io/request
   :uri     (config/api-route :object-graph id)
   :method  :get
   :edn?    true
   :success (fn [_ data]
              (async/publish bus :gallery-info-loaded (:body data)))
   :error   (fn [status data]
              (warn :response status data))))

(defn graph-nodes
  [graph]
  (->> graph
       vals
       (apply concat)
       (reduce (fn [acc n] (assoc acc (:id n) n)) {})))

(defn extract-branch
  [graph nodes id]
  (loop [branch [id], parent (:parent-id (nodes id))]
    (if parent
      (recur (conj branch parent) (:parent-id (nodes parent)))
      branch)))

(defn graph-branches
  [graph nodes heads]
  (mapv (fn [id] (extract-branch graph nodes id)) heads))

(defn sort-nodes
  [nodes] (sort-by (comp - :created val) nodes))

(defn compute-node-positions
  [nodes height]
  (let [offset (/ height 2)]
    (->> nodes
         (map-indexed (fn [i [id]] [id (mm/madd i height offset)]))
         (into {}))))

(defn compute-item-height
  [heights]
  (let [w (.-innerWidth js/window)]
    (some
     (fn [[range h]] (if (m/in-range? range w) h))
     heights)))

(defn svg-branch
  [nodes visited branch x ypos height]
  (loop [visited visited, path "", branch branch, first? true]
    (if branch
      (let [id     (first branch)
            parent (visited id)
            y      (ypos id)]
        (if y
          (if first?
            (recur (assoc visited id (vec2 x y)) (str path "M" x "," y) (next branch) false)
            (if-not parent
              (recur (assoc visited id (vec2 x y)) (str path " L" x "," y) (next branch) false)
              (let [[px py] parent
                    by1 (- py (* height 0.3))
                    by2 (- py (* height 0.2))
                    path (str path " L" x "," by1 " C" x "," by2 "," px "," by2 "," px "," py)]
                [visited [:path {:d path}]])))
          [visited [:path {:d path}]]))
      [visited [:path {:d path}]])))

(defn svg-node-label
  [node-pos id {:keys [created]} radius lx]
  (let [[x y]   (node-pos id)
        created (js/Date. created)
        date    (utils/format-date created)
        time    (utils/format-time created)]
    [:g
     [:text {:x lx :y (- y 4)} date]
     [:text {:x lx :y (+ y 12)} time]
     [:circle {:cx x :cy y :r radius}]]))

(defn timeline-svg
  [parent width height & body]
  (dom/create-dom!
   [:svg {:width width :height height :id "branchviz"}
    body]
   parent))

(defn generate-timeline
  [parent graph]
  (let [{:keys [item-heights radius branch-width label-width] :as conf} (-> config/app :gallery-info)
        heads      (mapv key (filter #(empty? (val %)) graph))
        num-heads  (count heads)
        nodes      (graph-nodes graph)
        branches   (sort-by (comp - count) (graph-branches graph nodes heads))
        sorted     (sort-nodes nodes)
        ih         (compute-item-height item-heights)
        nodes-ypos (compute-node-positions sorted ih)
        width      (mm/madd num-heads branch-width label-width)
        height     (* (count nodes) ih)
        x          (- width radius)
        lx         (mm/madd (dec num-heads) (- branch-width) radius -2 x)
        [node-pos
         paths]    (->> branches
                        (reduce
                         (fn [[visited paths i] branch]
                           (let [x (mm/madd branch-width (- i) x)
                                 [visited path] (svg-branch nodes visited branch x nodes-ypos ih)]
                             [visited (conj paths path) (inc i)]))
                         [{} () 0]))
         labels    (map (fn [[id n]] (svg-node-label node-pos id n radius lx)) nodes)]
    (timeline-svg parent width height paths labels)
    sorted))

(defn item-credits
  [{:keys [title author tree-depth]}]
  [:div
   [:p [:span "title:"] title]
   [:p [:span "author:"] author]
   [:p [:span "complexity:"] tree-depth]])

(defn generate-item-details
  [parent bus items]
  (let [buttons (-> config/app :gallery-info :buttons)
        attribs {:class "item-version"}]
    (dorun
     (map
      #(item/gallery-item % buttons parent bus attribs (item-credits %))
      items))))

(defn redraw-all
  [graph bus]
  (let [parent   (->> :gallery-info-main
                      (config/dom-component)
                      (dom/clear!)
                      (dom/create! "div"))
        timeline (dom/create! "div" parent)
        versions (dom/create! "div" parent {:class "versions"})]
    (->> graph
         (generate-timeline timeline)
         (vals)
         (generate-item-details versions bus))))

(defn handle-loaded
  [ch bus local]
  (go
    (while true
      (let [[_ graph] (<! ch)]
        (swap! local assoc :graph graph)
        (redraw-all graph bus)))))

(defn handle-resize
  [ch bus local]
  (go
    (while true
      (<! ch)
      (when-let [graph (:graph @local)]
        (redraw-all graph bus)))))

(defn init-button-bar
  [bus local]
  (dom/add-listeners
   [[(config/dom-component :gallery-info-cancel) "click"
     #(route/set-route! "gallery")]]))

(defn init
  [bus]
  (let [init    (async/subscribe bus :init-gallery-info)
        loaded  (async/subscribe bus :gallery-info-loaded)
        resize  (async/throttle
                 (async/subscribe bus :window-resize (async/sliding-channel 1))
                 500)
        local   (atom {})]

    (init-button-bar bus local)
    (handle-loaded   loaded bus local)
    (handle-resize   resize bus local)

    (go
      (while true
        (let [[_ [_ {:keys [id]}]] (<! init)]
          (swap!
           local assoc
           :id    id
           :graph nil)
          (load-graph bus id))))))
