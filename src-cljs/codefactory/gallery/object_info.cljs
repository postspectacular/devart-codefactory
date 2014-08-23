(ns codefactory.gallery.object-info
  (:require-macros
   [cljs.core.async.macros :as asm :refer [go]])
  (:require
   [codefactory.config :as config]
   [codefactory.common :as common]
   [thi.ng.cljs.async :as async]
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.route :as route]
   [thi.ng.cljs.utils :as utils :refer [->px]]
   [thi.ng.cljs.io :as io]
   [thi.ng.cljs.dom :as dom]
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

(defn handle-refresh
  [ch bus local]
  (go
    (while true
      (let [[_ ancestors] (<! ch)]
        (debug :ancestors ancestors)))))

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
