(ns thi.ng.cljs.app
  (:require-macros
   [cljs.core.async.macros :refer [go alt!]]
   [thi.ng.cljs.macros :refer [dochan]])
  (:require
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.utils :as utils]
   [goog.events :as events]
   [cljs.core.async :refer [>! <! chan put! close! timeout]])
  (:import goog.events.EventType))

(defmulti dispatch-event (fn [[id] state queue] id))

(defmethod dispatch-event :default
  [[id] & _]
  (prn :unhandled-event id))

(defn event-dispatcher
  [state queue]
  (go
    (while true
      (let [event (<! queue)]
        (debug :new-event event)
        (dispatch-event event state queue)))))

(defn emit
  [queue id body]
  (put! queue [id body]))

(defn make-state
  [init-state]
  (atom (assoc init-state :event-queue (chan))))

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

(def keyword->event-type
  {:keyup goog.events.EventType.KEYUP
   :keydown goog.events.EventType.KEYDOWN
   :keypress goog.events.EventType.KEYPRESS
   :click goog.events.EventType.CLICK
   :dblclick goog.events.EventType.DBLCLICK
   :mousedown goog.events.EventType.MOUSEDOWN
   :mouseup goog.events.EventType.MOUSEUP
   :mouseover goog.events.EventType.MOUSEOVER
   :mouseout goog.events.EventType.MOUSEOUT
   :mousemove goog.events.EventType.MOUSEMOVE
   :focus goog.events.EventType.FOCUS
   :blur goog.events.EventType.BLUR})

(defn listen
  ([el type] (listen el type nil))
  ([el type f] (listen el type f (chan)))
  ([el type f out]
     (events/listen el (keyword->event-type type)
                    (fn [e] (when f (f e)) (put! out e)))
     out))
