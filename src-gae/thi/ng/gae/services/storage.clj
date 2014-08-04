(ns thi.ng.gae.services.storage
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

(defn put!
  [service bucket key data & [opts]]
  (let [key-path    (GcsFilename. bucket key)
        opts        (file-options opts)]
    (prn :chan-opts opts)
    (.createOrReplace service key-path opts (ByteBuffer/wrap data))
    (prn :written key-path (alength data) "bytes")))
