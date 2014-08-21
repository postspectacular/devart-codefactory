(ns thi.ng.cljs.io
  (:require
   [thi.ng.cljs.log :refer [debug info warn]]
   [goog.net.XhrIo :as xhr]
   [goog.events :as ev]
   [goog.net.EventType]
   [goog.Uri.QueryData :as qd]
   [goog.structs :as structs]
   [cljs.reader :refer [read-string]]
   [clojure.string :as str]))

(defn format-query-params
  [params]
  (when params
    (->> params
         (map (fn [[k v]] (str (name k) "=" v "&")))
         (apply str "?"))))

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

(defn append-signature
  [uri data key]
  (let [sig (.hash js/Sha256 (str uri ";" data ";" key))]
    (debug :signature sig)
    (str data "&sig=" sig)))

(defn request [& {:keys [uri method params data success error headers edn? sign-key]}]
  (let [req     (goog.net.XhrIo.)
        method  (str/upper-case (name method))
        data    (->request-data data)
        success (->callback success edn?)
        error   (->callback error edn?)
        headers (->headers headers :edn edn?)
        uri     (str uri (format-query-params params))
        data    (if sign-key (append-signature uri data sign-key) data)]
    (debug :request uri :data data)
    (when success
      (ev/listen req goog.net.EventType/SUCCESS #(success req)))
    (when error
      (ev/listen req goog.net.EventType/ERROR #(error req))
      (ev/listen req goog.net.EventType/TIMEOUT #(error req)))
    (.send req uri method data (when headers (clj->js headers)))))
