(ns thi.ng.cljs.io
  (:require
   [goog.net.XhrIo :as xhr]
   [goog.events :as ev]
   [goog.Uri.QueryData :as qd]
   [goog.structs :as structs]
   [cljs.reader :refer [read-string]]
   [clojure.string :as str])
  (:import goog.net.EventType))

(defn ->request-data
  [data]
  (->> data
       (clj->js)
       (structs/Map.)
       (qd/createFromMap)
       (str)))

(defn ->callback
  [callback edn?]
  (when callback
    (fn [req]
      (callback
       (.getStatus req)
       (if edn?
         (read-string (.getResponseText req))
         (.getResponseText req))))))

(defn ->headers
  [headers & {:keys [edn xsrf]}]
  (cond-> headers
          edn (assoc "Accept" "application/edn")))

(defn request [& {:keys [uri method data success error headers edn?]}]
  (let [req     (goog.net.XhrIo.)
        method  (str/upper-case (name method))
        data    (->request-data data)
        success (->callback success edn?)
        error   (->callback error edn?)
        headers (->headers headers :edn edn?)]
    (when success
      (ev/listen req goog.net.EventType/SUCCESS #(success req)))
    (when error
      (ev/listen req goog.net.EventType/ERROR #(error req))
      (ev/listen req goog.net.EventType/TIMEOUT #(error req)))
    (.send req uri method data (when headers (clj->js headers)))))

