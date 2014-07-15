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
   :uri     (get-in config [:api :routes :submit-object])
   :method  :post
   :edn?    true
   :data    (merge (get-in config [:api :inject]) data)
   :success (fn [status body]
              (debug :success-response status body)
              (async/publish bus :submit-model-success body))
   :error   (fn [status body]
              (debug :error-response status body)
              (async/publish bus :submit-model-fail body))))

(defn init
  [bus config]
  (let [init-chan    (async/subscribe bus :init-submit)
        release-chan (async/subscribe bus :release-submit)
        tree-chan    (async/subscribe bus :broadcast-tree)
        success      (async/subscribe bus :submit-model-success)
        [submit]     (dom/event-channel (dom/by-id "bt-submit") "click")
        [cancel]     (dom/event-channel (dom/by-id "submit-cancel") "click")
        local        (atom {})]
    ;; TODO enable gallery button
    (go
      (loop []
        (let [[_ [state]] (<! init-chan)
              form (aget (.-forms js/document) "submit-art-form")]
          (dom/set-attr! (aget form "title") {:value ""})
          (dom/set-attr! (aget form "author") {:value ""})
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
        (let [form   (aget (.-forms js/document) "submit-art-form")
              title  (.-value (aget form "title"))
              author (.-value (aget form "author"))
              {:keys [tree seed]} @local]
          (submit-model
           bus config
           {:tree (pr-str tree)
            :seed seed
            :title title
            :author author})
          (recur))))

    (go
      (loop []
        (let [[_ data] (<! success)
              {:keys [id]} (:body data)]
          (debug :success id)
          (route/set-route! "thanks" id)
          (recur))))
    
    (go
      (loop []
        (<! cancel)
        (route/set-route! "objects" "new" (:seed @local))
        (recur)))))
