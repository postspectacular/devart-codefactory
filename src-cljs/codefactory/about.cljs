(ns codefactory.about
  (:require-macros
   [cljs.core.async.macros :refer [go]])
  (:require
   [codefactory.config :as config]
   [codefactory.common :as common]
   [codefactory.operators :as ops]
   [thi.ng.cljs.async :as async]
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.route :as route]
   [thi.ng.cljs.dom :as dom]
   [cljs.core.async :as cas :refer [>! <! chan put! close! timeout]]))

(defn init-icon
  [parent op size]
  (let [[icon] (common/icon-button
                nil nil size
                (-> config/app :operators op :paths)
                nil nil (ops/op-class op))]
    (dom/insert! icon parent)))

(defn init-icons
  []
  (let [size (-> config/app :about :icon-size)
        els (dom/query-all nil "#about .tools div")
        num (.-length els)]
    (loop [i 0]
      (if (< i num)
        (let [el (aget els i)
              [op] (dom/get-attribs el ["data-op"])]
          (init-icon el (keyword op) size)
          (recur (inc i)))))))

(defn init-links
  []
  (when-not (-> config/app :about :links-clickable?)
    (let [els (dom/query-all nil "#about a")
          num (.-length els)
          nolink (fn [e] (.preventDefault e))]
      (loop [i 0]
        (when (< i num)
          (let [el (aget els i)]
            (dom/add-class! el "nolink")
            (dom/add-listeners [[el "click" nolink]])
            (recur (inc i))))))))

(defn init
  [bus]
  (let [init     (async/subscribe bus :init-about)
        [cancel] (async/event-channel (config/dom-component :about-continue) "click")
        local    (atom {:active? true})]

    (init-icons)
    (init-links)
    
    (go
      (loop []
        (<! init)
        (swap! local assoc :active? true)
        (set! (.-scrollTop (dom/query nil "#about .body-wrapper")) 0)
        (go
          (alts! [cancel (timeout (config/timeout :about))])
          (when (:active? @local)
            (route/set-route! "home")))
        (recur)))))
