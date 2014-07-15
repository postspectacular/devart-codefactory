(ns codefactory.submit
  (:require-macros
   [cljs.core.async.macros :refer [go]])
  (:require
   [thi.ng.cljs.async :as async]
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.io :as io]
   [thi.ng.cljs.route :as route]
   [thi.ng.cljs.utils :as utils]
   [thi.ng.cljs.dom :as dom]
   [cljs.core.async :as cas :refer [>! <! chan put! close! timeout]]))

(defn submit-model
  [bus config data]
  (io/request
   :uri     (get-in config [:api-routes :submit-object])
   :method  :post
   :edn?    true
   :data    data
   :success (fn [_ data]
              (async/publish bus :subit-model-success data))
   :error   (fn [status body]
              (async/publish bus :submit-model-fail [status body]))))

(defn init
  [bus config]
  (let [init-chan    (async/subscribe bus :init-submit)
        release-chan (async/subscribe bus :release-submit)
        tree-chan    (async/subscribe bus :broadcast-tree)
        [submit]     (dom/event-channel (dom/by-id "bt-submit") "click")
        [cancel]     (dom/event-channel (dom/by-id "submit-cancel") "click")
        local        (atom {})]
    ;; TODO enable gallery button
    (go
      (loop []
        (let [[_ [state]] (<! init-chan)]
          (debug :init-submit)
          (recur))))

    (go
      (loop []
        (let [[_ [state]] (<! release-chan)]
          (debug :release-submit)
          (recur))))

    (go
      (loop []
        (let [[_ [tree seed]] (<! tree-chan)]
          (debug :tree-received tree)
          (swap! local assoc :tree tree :seed seed)
          (recur))))

    (go
      (loop []
        (<! submit)
        (let [form (dom/by-id "submit-art-form")]
          (.log js/console form)
          ;;(submit-model bus config form)
          (recur))))
    
    (go
      (loop []
        (<! cancel)
        (route/set-route! "objects" "new" (:seed @local))
        (recur)))))
