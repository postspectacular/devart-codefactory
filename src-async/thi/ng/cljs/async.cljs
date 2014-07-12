(ns thi.ng.cljs.async
  (:require-macros
   [cljs.core.async.macros :as asm :refer [go]])
  (:require
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.dom :as dom]
   [goog.events :as events]
   [cljs.core.async :as async :refer [>! <! chan put! close! timeout]]))

(defprotocol PubSub
  (bus [_])
  (publisher [_])
  (publish [_ id body])
  (subscribe [_ id] [_ id sub])
  (unsubscribe [_] [_ id sub]))

(defn pub-sub
  [topic-fn]
  (let [bus (chan)
        pub (async/pub bus topic-fn)]
    (reify
      PubSub
      (bus [_] bus)
      (publisher [_] pub)
      (publish
        [_ id body] (put! bus [id body]))
      (subscribe
        [_ id] (subscribe _ id (chan)))
      (subscribe
        [_ id sub]
        (async/sub pub id sub)
        (debug :subscribed id)
        sub)
      (unsubscribe
        [_ id] (async/unsub-all pub id))
      (unsubscribe
        [_ id sub]
        (async/unsub pub id sub)
        (debug :unsubscribed id)))))

(defn unsubscribe-and-close-many
  [bus topic-map]
  (loop [coll topic-map]
    (if (seq coll)
      (let [[k v] (first coll)]
        (unsubscribe bus k v)
        (close! v)
        (recur (next coll))))))

(defn subscription-channels
  [bus ids]
  (reduce (fn [subs id] (assoc subs id (subscribe bus id))) {} ids))
