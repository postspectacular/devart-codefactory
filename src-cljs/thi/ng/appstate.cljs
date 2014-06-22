(ns thi.ng.cljs.appstate
  (:refer-clojure :exclude [map filter remove distinct concat take-while])
  (:require [goog.events :as events]
            [goog.events.EventType]
            [goog.net.Jsonp]
            [goog.Uri]
            [cljs.core.async :refer [>! <! chan put! close! timeout]])
  (:require-macros [cljs.core.async.macros :refer [go alt!]]
                   [thi.ng.cljs.macros :refer [dochan]])
  (:import goog.events.EventType))

(defn log [in]
  (let [out (chan)]
    (dochan [e in]
            (.log js/console e)
            (>! out e))
    out))

(defn make-app-state
  [init-state]
  (atom init-state))

(defn listen-state-update!
  [state id path listener]
  (add-watch
   state id
   (fn [_ state old new]
     (prn :listener id)
     (listener state (get-in old path) (get-in new path)))))

(defn listen-state-change!
  [state id path listener]
  (add-watch
   state id
   (fn [_ state old new]
     (prn :listener id)
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
