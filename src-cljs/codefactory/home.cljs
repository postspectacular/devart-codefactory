(ns codefactory.home
  (:require-macros
   [cljs.core.async.macros :as asm :refer [go]])
  (:require
   [codefactory.config :as config]
   [codefactory.common :as common]
   [thi.ng.cljs.async :as async]
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.route :as route]
   [thi.ng.cljs.utils :as utils]
   [thi.ng.cljs.dom :as dom]
   [cljs.core.async :as cas :refer [>! <! chan put! close! timeout]]))

(defn init-button-bar
  []
  (dom/add-listeners
   [[(config/dom-component :home-continue) "click"
     #(route/set-route! "select")]])
  (when (and (config/module-enabled? :gallery)
             (-> config/app :home :gallery-bt))
    (let [bt (config/dom-component :home-gallery)]
      (dom/show! bt)
      (dom/add-listeners
       [[bt "click"
         #(route/set-route! "gallery")]]))))

(defn show-credits
  [{:keys [title author date]}]
  (dom/set-style! (dom/query nil "#home .credits") #js {:visibility "visible"})
  (dom/set-text!  (dom/by-id "credits-title") title)
  (dom/set-text!  (dom/by-id "credits-author") author)
  (dom/set-text!  (dom/by-id "credits-date") date))

(defn init
  [bus]
  (let [init (async/subscribe bus :init-home)]

    (init-button-bar)

    (go
      (loop []
        (let [[_ [state]] (<! init)]
          (async/publish bus :broadcast-tree nil)
          (when-let [credits (:credits @state)]
            (show-credits credits)))
        (recur)))))
