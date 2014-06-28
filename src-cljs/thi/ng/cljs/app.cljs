(ns thi.ng.cljs.app
  (:require-macros
   [cljs.core.async.macros :refer [go alt!]]
   [thi.ng.cljs.macros :refer [dochan]])
  (:require
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.utils :as utils]
   [thi.ng.cljs.dom :as dom]
   [goog.events :as events]
   [cljs.core.async :refer [>! <! chan put! close! timeout]]))

(defmulti handle-event (fn [[id] state queue] id))

(defmethod handle-event :default
  [[id] & _]
  (prn :unhandled-event id))

(defn event-dispatcher
  [state queue]
  (go
    (loop []
      (let [event (<! queue)]
        (debug :new-event event)
        (handle-event event state queue)
        (recur)))))

(defn emit
  [queue id body]
  (put! queue [id body]))

(defn merge-state
  [state xs]
  (swap! state #(utils/deep-merge-with (fn [& _] (last _)) % xs)))

(defn listen-state-update!
  [state id path listener]
  (add-watch
   state id
   (fn [_ state old new]
     (debug :listener id)
     (listener state (get-in old path) (get-in new path)))))

(defn listen-state-change!
  [state id path listener]
  (add-watch
   state id
   (fn [_ state old new]
     (debug :listener id)
     (let [old (get-in old path)
           new (get-in new path)]
       (if-not (= old new)
         (listener state old new))))))

(defn unlisten-state!
  [state id]
  (remove-watch state id))

(defn listen
  ([el type] (listen el type nil))
  ([el type f] (listen el type f (chan)))
  ([el type f out]
     (events/listen el (name type)
                    (fn [e] (when f (f e)) (put! out e)))
     out))

(defn add-listeners
  [specs]
  (loop [specs specs]
    (if specs
      (let [[id eid f] (first specs)
            el (if (= "$window" id)
                 js/window (dom/query nil id))]
        (events/listen el (name eid) f)
        (recur (next specs))))))

(defn remove-listeners
  [specs]
  (loop [specs specs]
    (if specs
      (let [[id eid f] (first specs)
            el (if (= "$window" id)
                 js/window (dom/query nil id))]
        (events/unlisten el (name eid) f)
        (recur (next specs))))))

(defn add-hammer-listeners
  [h specs]
  (loop [specs specs]
    (if specs
      (let [[g f] (first specs)]
        (.on h g f)
        (recur (next specs))))))

(defn remove-hammer-listeners
  [h specs]
  (loop [specs specs]
    (if specs
      (let [[g f] (first specs)]
        (.off h g f)
        (recur (next specs))))))
