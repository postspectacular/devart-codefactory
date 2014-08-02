(ns thi.ng.gae.services.url-shortener
  (:require
   [clojure.java.io :as io]
   [clojure.data.json :as json])
  (:import
   [java.net HttpURLConnection URL]))

(defn short-url
  [long-url api-key]
  (try
    (let [url (URL. (str "https://www.googleapis.com/urlshortener/v1/url?key=" api-key))
          conn (doto(.openConnection url)
                 (.setDoOutput true)
                 (.setRequestMethod "POST")
                 (.addRequestProperty "Content-Type" "application/json"))]
      (prn :shorten-url long-url)
      (with-open [out (io/writer (.getOutputStream conn))]
        (json/write {:longUrl long-url} out))
      (prn :shortener-response-code (.getResponseCode conn))
      (if (= (.getResponseCode conn) HttpURLConnection/HTTP_OK)
        (with-open [in (io/reader (.getInputStream conn))]
          (let [resp (json/read in :key-fn keyword)]
            (prn :shortener-response resp)
            (:id resp)))))
    (catch Exception e
      (prn :shortener-error (.getMessage e))
      (.printStackTrace e))))
