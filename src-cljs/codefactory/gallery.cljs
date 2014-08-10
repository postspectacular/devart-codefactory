(ns codefactory.gallery
  (:require-macros
   [cljs.core.async.macros :as asm :refer [go]])
  (:require
   [codefactory.config :as config]
   [codefactory.common :as common]
   [thi.ng.cljs.async :as async]
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.route :as route]
   [thi.ng.cljs.utils :as utils]
   [thi.ng.cljs.io :as io]
   [thi.ng.cljs.dom :as dom]
   [cljs.core.async :as cas :refer [>! <! chan put! close! timeout]]))

(defn init-fullscreen-button
  []
  (let [tools (dom/query nil "#gallery .tools-extra")
        icons (:icons config/app)
        size (-> config/app :editor :toolbar-icon-size)]
    (common/icon-button
     tools nil size (-> icons :fullscreen :paths) nil
     dom/request-fullscreen
     "fs-toggle")))

(defn init-button-bar
  []
  (dom/add-listeners
   [[(config/dom-component :gallery-cancel) "click"
     #(route/set-route! "home")]]))

(defn load-objects
  [bus offset]
  (io/request
   :uri     (str (config/api-route :gallery) offset)
   :method  :get
   :edn?    true
   :success (fn [_ {:keys [body] :as data}]
              (async/publish bus :gallery-loaded body))
   :error   (fn [status body]
              (warn :response body))))

(defn gallery-item
  [{:keys [id title author preview-uri stl-uri]} parent]
  (let [src (str "/api/1.0/objects/" id "/preview")
        edit-uri (str "#/objects/" id)
        item (dom/create! "div" parent)
        link (dom/create! "a" item {:href edit-uri})
        img  (dom/create! "img" link {:src src})
        credits (dom/create! "div" item)]
    (dom/set-html! credits (str title " by " author))))

(defn build-gallery
  [objects]
  (let [parent (config/dom-component :gallery-main)]
    (dom/set-html! parent "")
    (loop [objects objects]
      (when objects
        (gallery-item (first objects) parent)
        (recur (next objects))))))

(defn handle-refresh
  [ch bus local]
  (go
    (loop []
      (let [[_ objects] (<! ch)]
        (build-gallery objects))
      (recur))))

(defn init
  [bus]
  (let [init    (async/subscribe bus :init-gallery)
        refresh (async/subscribe bus :gallery-loaded)
        local   (atom {})]

    (init-fullscreen-button)
    (init-button-bar)
    (handle-refresh refresh bus local)
    
    (go
      (loop []
        (let [[_ [state params]] (<! init)]
          (load-objects bus 0))
        (recur)))))
