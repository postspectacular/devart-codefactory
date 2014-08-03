(ns thi.ng.gae.services.taskqueue
  (:require
   [thi.ng.gae.util :as util]
   [clojure.edn :as edn]
   [clojure.java.io :as io])
  (:import
   [com.google.appengine.api.taskqueue
    Queue QueueFactory]
   [com.google.appengine.api.taskqueue
    TaskOptions TaskOptions$Builder]
   [java.io PushbackReader]))

(defn- option-params
  [^TaskOptions opts params]
  (reduce (fn [opts [k v]] (.param opts (name k) v)) opts params))

(defn- option-headers
  [^TaskOptions opts headers]
  (reduce (fn [opts [k v]] (.header opts (name k) v)) opts headers))

(defn- option-tags
  [^TaskOptions opts tags]
  (reduce (fn [opts t] (.tag opts (name t))) opts tags))

(defn- option-payload
  [^TaskOptions opts payload]
  (.payload opts
            (cond
             (string? payload) payload
             (util/byte-array? payload) payload
             :else (pr-str payload))))

(defn task-options
  [{:keys [url params payload headers tags]}]
  (cond-> (TaskOptions$Builder/withUrl url)
          params  (option-params params)
          headers (option-headers headers)
          tags    (option-tags tags)
          payload (option-payload payload)))

(defn queue!
  [q opts]
  (let [^Queue q (or q (QueueFactory/getDefaultQueue))]
    (.add q (task-options opts))
    q))

(defn get-edn-payload
  [req]
  (with-open [in (PushbackReader. (io/reader (:body req)))]
    (try (edn/read in) (catch Exception e))))
