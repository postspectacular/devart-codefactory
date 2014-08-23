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
   [hiccups.runtime :as h]
   [cljs.core.async :refer [<! timeout]]
   [clojure.string :as str]))

(defn load-info
  [bus id]
  (dom/set-html!
   (config/dom-component :gallery-info-main)
   (common/loader-html "Loading artwork details..."))
  (io/request
   :uri     (str (config/api-route :gallery-info) id)
   :method  :get
   :edn?    true
   :success (fn [_ data]
              (async/publish bus :gallery-info-loaded (:body data)))
   :error   (fn [status data]
              (warn :response status data))))

(defn generate-timeline
  [parent items]
  (let [{:keys [width item-height color font radius]} (-> config/app :gallery-info)
        height  (* (count items) item-height)
        canvas  (dom/create! "canvas" parent {:width width :height height})
        ctx     (.getContext canvas "2d")
        cx      (- width radius)
        start-y (/ item-height 2)
        lx      (mm/sub cx radius 10)]
    (set! (.-fillStyle ctx) color)
    (set! (.-strokeStyle ctx) "")
    (set! (.-font ctx) font)
    (set! (.-textAlign ctx) "right")
    (set! (.-textBaseline ctx) "middle")
    (.fillRect ctx (dec cx) start-y 2 (- height item-height))
    (loop [items items, y start-y]
      (when items
        (let [{:keys [created]} (first items)
              created (js/Date. created)
              date    (utils/format-date created)
              time    (utils/format-time created)]
          (doto ctx
            (.beginPath)
            (.arc cx y radius 0 TWO_PI true)
            (.fill)
            (.fillText date lx (- y 8))
            (.fillText time lx (+ y 8)))
          (recur (next items) (+ y item-height)))))))

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
         [:p
          "title: " title [:br]
          "author: " author]]])
     items))))

(defn handle-refresh
  [ch bus local]
  (go
    (while true
      (let [[_ items] (<! ch)
            parent (->> :gallery-info-main
                        (config/dom-component)
                        (dom/clear!))]
        (debug :ancestors items)
        (generate-timeline (dom/create! "div" parent) items)
        (generate-item-details (dom/create! "div" parent {:class "versions"}) items)))))

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
          (load-info bus id))))))
