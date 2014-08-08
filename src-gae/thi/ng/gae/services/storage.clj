(ns thi.ng.gae.services.storage
  (:refer-clojure :exclude [get])
  (:require
   [clojure.java.io :as io]
   [thi.ng.gae.streams :as streams])
  (:import
   [com.google.appengine.tools.cloudstorage
    GcsService GcsServiceFactory
    GcsFilename GcsFileOptions GcsFileOptions$Builder
    GcsOutputChannel
    RetryParams RetryParams$Builder]
   [java.nio ByteBuffer]
   [java.nio.channels Channels]))

(defn ^RetryParams retry-options
  [{:keys [initial-delay max-attempts total]
    :or   {initial-delay 100, max-attempts 10, total 30000}}]
  (cond-> (RetryParams$Builder.)
          initial-delay (.initialRetryDelayMillis initial-delay)
          max-attempts  (.retryMaxAttempts max-attempts)
          total         (.totalRetryPeriodMillis total)
          :then         (.build)))

(defn ^GcsFileOptions file-options
  [{:keys [acl mime] :or {acl :public-read}}]
  (cond-> (GcsFileOptions$Builder.)
          acl   (.acl (name acl))
          mime  (.mimeType mime)
          :then (.build)))

(defn ^GcsService get-service
  [& [opts]] (GcsServiceFactory/createGcsService (retry-options opts)))

(defn get
  [service bucket key]
  (let [key-path (GcsFilename. bucket key)]
    (-> service
        (.openPrefetchingReadChannel key-path 0 0x4000)
        (Channels/newInputStream))))

(defn put!
  [service bucket key data & [opts]]
  (let [key-path    (GcsFilename. bucket key)
        opts        (file-options opts)]
    (prn :chan-opts opts)
    (.createOrReplace service key-path opts (ByteBuffer/wrap data))
    (prn :written key-path (alength data) "bytes")))

(defn get-meta
  [service bucket key]
  (let [key-path (GcsFilename. bucket key)
        meta     (.getMetadata service key-path)
        opts     (.getOptions meta)]
    {:path          key-path
     :length        (.getLength meta)
     :last-modified (.getLastModified meta)
     :mime-type     (.getMimeType opts)
     :acl           (.getAcl opts)
     :options       opts}))
