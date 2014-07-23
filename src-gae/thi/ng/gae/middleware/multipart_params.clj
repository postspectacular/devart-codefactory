(ns thi.ng.gae.middleware.multipart-params
  (:import [org.apache.commons.fileupload.servlet ServletFileUpload]
           [org.apache.commons.fileupload.util Streams]
           [org.apache.commons.fileupload FileItemIterator FileItemStream FileUpload]
           org.apache.commons.io.IOUtils))

(defn- multipart-form?
  "Returns true if request has a multipart form?"
  [request]
  (if-let [^String content-type (:content-type request)]
    (.startsWith content-type "multipart/form-data")))

(defn- file-item-iterator-seq
  "Create a lazy seq from a FileItemIterator instance."
  [^FileItemIterator it]
  (lazy-seq
    (if (.hasNext it)
      (cons (.next it) (file-item-iterator-seq it)))))

(defn parse-file-item
  [^FileItemStream i ^String encoding]
  [(.getFieldName i)
   (if (.isFormField i)
     (Streams/asString (.openStream i) encoding)
     (let [upload-bytes (IOUtils/toByteArray (.openStream i))
           size (alength upload-bytes)
           upload-bytes (if (zero? size) nil upload-bytes)]
       {:content-type (.getContentType i)
        :filename (.getName i)
        :size size
        :bytes upload-bytes}))])

(defn- extract-file-items
  "Map field names to values, which will either be a simple string or map.
   Multipart values will be maps with content-type, name (original filename),
   and stream (an open input stream object)."
  [request ^String encoding]
  (->> (:request request)
       (.getItemIterator (ServletFileUpload.))
       (file-item-iterator-seq)
       (map #(parse-file-item % encoding))
       (into {})))

(defn- multipart-params-request
  [request & [opts]]
  (if (multipart-form? request)
    (let [encoding (or (:encoding opts)
                       (:character-encoding request)
                       "UTF-8")
          params   (extract-file-items request encoding)]
      (merge-with merge request
                  {:multipart-params params}
                  {:params params}))
    request))

(defn wrap-multipart-params
  "Works just like ring.middleware.multipart-params/wrap-multipart-params:
   adds :multipart-params and :params to the request map (the latter requires
   ring.middleware.params/wrap-params). Takes a map with an optional :encoding
   map."
  [handler & [opts]]
  (fn [request]
    (-> request
        (multipart-params-request opts)
        handler)))
