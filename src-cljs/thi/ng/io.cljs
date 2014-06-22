(ns thi.ng.cljs.io
  (:require
   [goog.net.XhrIo :as xhr]
   [goog.events :as ev]
   [goog.Uri.QueryData :as qd]
   [goog.structs :as structs]
   [cljs.reader :refer [read-string]]
   [clojure.string :as str])
  (:import goog.net.EventType))

(defn ->request-data [data]
  (->> data
       (clj->js)
       (structs/Map.)
       (qd/createFromMap)
       (str)))

(defn ->callback [callback]
  (when callback
    (fn [req]
      (callback (.getStatus req) (.getResponseText req)))))

(defn xhr [& {:keys [uri method data success error headers]}]
  (let [req     (goog.net.XhrIo.)
        method  (str/upper-case (name method))
        data    (->request-data data)
        success (->callback success)
        error   (->callback error)]
    (when success
      (ev/listen req goog.net.EventType/SUCCESS #(success req)))
    (when error
      (ev/listen req goog.net.EventType/ERROR #(error req))
      (ev/listen req goog.net.EventType/TIMEOUT #(error req)))
    (.send req uri method data (when headers (clj->js headers)))))

(defn test-io
  []
  (enable-console-print!)
  (xhr :uri "/api" :method :post :data {:method "foo" :name "toxi"}
       :success (fn [status body] (prn :success status body))
       :error (fn [status body] (prn :error status body))))
