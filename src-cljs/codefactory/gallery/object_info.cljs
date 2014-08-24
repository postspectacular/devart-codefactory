(ns codefactory.gallery.object-info
  (:require-macros
   [cljs.core.async.macros :as asm :refer [go]]
   [thi.ng.macromath.core :as mm])
  (:require
   [codefactory.config :as config]
   [codefactory.common :as common]
   [thi.ng.cljs.async :as async]
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.route :as route]
   [thi.ng.cljs.utils :as utils :refer [->px]]
   [thi.ng.cljs.io :as io]
   [thi.ng.cljs.dom :as dom]
   [thi.ng.common.math.core :as m :refer [TWO_PI]]
   [thi.ng.geom.core.vector :refer [vec2]]
   [hiccups.runtime :as h]
   [cljs.core.async :refer [<! timeout]]
   [clojure.string :as str]))

(defn load-graph
  [bus id]
  (dom/set-html!
   (config/dom-component :gallery-info-main)
   (common/loader-html "Loading artwork details..."))
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
  (debug :new-branch id)
  (loop [branch [id], parent (:parent-id (nodes id))]
    (debug :id id :parent parent)
    (if parent
      (recur (conj branch parent) (:parent-id (nodes parent)))
      branch)))

(defn graph-branches
  [graph nodes heads]
  (mapv (fn [id] (extract-branch graph nodes id)) heads))

(defn sort-nodes
  [nodes] (sort-by (comp - :created val) nodes))

(defn compute-node-positions
  [nodes item-height]
  (let [offset (/ item-height 2)]
    (->> nodes
         (map-indexed (fn [i [id]] [id (mm/madd i item-height offset)]))
         (into {}))))

(defn draw-branch
  [ctx x ypos height nodes visited branch]
  (set! (.-strokeStyle ctx) "white")
  (.beginPath ctx)
  (loop [visited visited, branch branch, last nil]
    (if branch
      (let [id     (first branch)
            parent (visited id)
            pos    (ypos id)]
        (prn :last last)
        (if last
          (if parent
            (let [by1 (- (:y parent) (* height 0.3))
                  by2 (- (:y parent) (* height 0.2))]
              (.lineTo ctx x by1)
              (.bezierCurveTo ctx x by2, (:x parent) by2, (:x parent) (:y parent))
              (.stroke ctx)
              visited)
            (let [p' (vec2 x pos)]
              (.lineTo ctx x pos)
              (recur (assoc visited id p') (next branch) p')))
          (let [p' (vec2 x pos)]
            (.moveTo ctx x pos)
            (recur (assoc visited id p') (next branch) p'))))
      (do
        (.stroke ctx)
        visited))))

(defn draw-node-label
  [ctx node-pos id {:keys [created]} radius lx]
  (let [[x y]   (node-pos id)
        created (js/Date. created)
        date    (utils/format-date created)
        time    (utils/format-time created)]
    (doto ctx
      (.beginPath)
      (.arc x y radius 0 TWO_PI true)
      (.fill)
      (.fillText date lx (- y 8))
      (.fillText time lx (+ y 8)))))

(defn generate-timeline
  [parent graph]
  (let [{:keys [item-height color font radius branch-width]} (-> config/app :gallery-info)
        heads      (map key (filter #(empty? (val %)) graph))
        num-heads  (count heads)
        nodes      (graph-nodes graph)
        branches   (sort-by (comp - count) (graph-branches graph nodes heads))
        sorted     (sort-nodes nodes)
        nodes-ypos (compute-node-positions sorted item-height)
        width      (mm/madd num-heads branch-width 80)
        height     (* (count nodes) item-height)
        canvas     (dom/create! "canvas" parent {:width width :height height})
        ctx        (.getContext canvas "2d")
        x          (- width radius)
        lx         (mm/madd (dec num-heads) (- branch-width) radius -2 x)
        node-pos   (->> branches
                        (reduce
                         (fn [[visited i] branch]
                           (let [x (mm/madd branch-width (- i) x)]
                             [(draw-branch ctx x nodes-ypos item-height nodes visited branch)
                              (inc i)]))
                         [{} 0])
                        (first))]
    (set! (.-fillStyle ctx) color)
    (set! (.-strokeStyle ctx) color)
    (set! (.-font ctx) font)
    (set! (.-textAlign ctx) "right")
    (set! (.-textBaseline ctx) "middle")
    (doseq [[id n] nodes]
      (draw-node-label ctx node-pos id n radius lx))
    sorted))

(defn generate-item-details
  [parent items]
  (dom/set-html!
   parent
   (h/render-html
    (map
     (fn [{:keys [title author tree-depth] :as item}]
       [:div.item-version
        [:div [:img {:src (common/item-asset-url item :preview) :width 320}]]
        [:div
         [:p [:span "title:"] title]
         [:p [:span "author:"] author]
         [:p [:span "complexity:"] tree-depth]
         ]])
     items))))

(defn handle-refresh
  [ch bus local]
  (go
    (while true
      (let [[_ graph] (<! ch)
            parent (->> :gallery-info-main
                        (config/dom-component)
                        (dom/clear!)
                        (dom/create! "div"))
            sorted (generate-timeline (dom/create! "div" parent) graph)]
        ;;(debug :graph sorted)        
        (generate-item-details (dom/create! "div" parent {:class "versions"}) (vals sorted))))))

(defn init-button-bar
  [bus local]
  (dom/add-listeners
   [[(config/dom-component :gallery-info-cancel) "click"
     #(route/set-route! "gallery")]]))

(defn init
  [bus]
  (let [init    (async/subscribe bus :init-gallery-info)
        refresh (async/subscribe bus :gallery-info-loaded)
        local   (atom {})]

    (init-button-bar bus local)
    (handle-refresh refresh bus local)

    (go
      (while true
        (let [[_ [_ {:keys [id]}]] (<! init)]
          (swap!
           local assoc
           :id       id
           :loading? true)
          (load-graph bus id))))))
