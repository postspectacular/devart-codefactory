(ns thi.ng.gae.servlet
  (:gen-class :extends javax.servlet.http.HttpServlet)
  (:import
   [java.io
    ByteArrayInputStream File FileInputStream
    InputStream OutputStream PrintWriter]
   [javax.servlet.http
    HttpServlet HttpServletRequest HttpServletResponse])
  (:require
   [codefactory.handlers :refer [app]]
   [thi.ng.gae.streams :as streams]))

(set! *warn-on-reflection* true)

(defprotocol PServletResponseBody
  (set-response-body [_ response]))

(extend-protocol PServletResponseBody

  String
  (set-response-body [_ response]
    (with-open [^PrintWriter out (.getWriter ^HttpServletResponse response)]
      (.print out ^String _)))

  clojure.lang.Seqable
  (set-response-body [_ response]
    (with-open [^PrintWriter out (.getWriter ^HttpServletResponse response)]
      (doseq [chunk _]
        (.print out (pr-str chunk))
        (.flush out))))

  java.io.InputStream
  (set-response-body [_ response]
    (with-open [^OutputStream out (.getOutputStream ^HttpServletResponse response)
                ^InputStream in _]
      (streams/copy-stream in out)
      (.flush out)))

  java.io.File
  (set-response-body [_ response]
    (with-open [stream (FileInputStream. ^File _)]
      (set-response-body stream response)))

  nil
  (set-response-body [_ _] nil))

(extend
    (class (byte-array 0))
  PServletResponseBody
  {:set-response-body
   (fn [_ response]
     (with-open [in (ByteArrayInputStream. ^"[B" _)]
       (set-response-body in response)))})

(defn- set-response-headers
  [headers ^HttpServletResponse response]
  (doseq [[k vals] headers]
    (if (string? vals)
      (.setHeader response k vals)
      (doseq [v vals] (.addHeader response k v))))
  (.setCharacterEncoding response "UTF-8")
  (when-let [content-type (get headers "Content-Type")]
    (.setContentType response content-type)))

(defn- servlet-response
  [^HttpServletResponse response
   {:keys [commit? status headers body]
    :or {commit? true}}]
  (when commit?
    (if status
      (.setStatus response status)
      (throw (RuntimeException. "handler response status not set")))
    (when headers (set-response-headers headers response))
    (when body (set-response-body body response))))

(defn- get-headers
  "Returns map of all request headers with their names as keywords."
  [^HttpServletRequest request]
  (reduce
   (fn [headers ^String name]
     (assoc headers (keyword (.toLowerCase name)) (.getHeader request name)))
   {} (enumeration-seq (.getHeaderNames request))))

(defn- request-map
  [^HttpServlet servlet ^HttpServletRequest request ^HttpServletResponse response]
  {:servlet            servlet
   :response           response
   :request            request
   :servlet-context    (.getServletContext servlet)
   :server-port        (.getServerPort request)
   :server-name        (.getServerName request)
   :remote-addr        (.getRemoteAddr request)
   :uri                (.getRequestURI request)
   :query-string       (.getQueryString request)
   :scheme             (keyword (.getScheme request))
   :request-method     (keyword (.toLowerCase (.getMethod request)))
   :headers            (get-headers request)
   :content-type       (.getContentType request)
   :content-length     (.getContentLength request)
   :character-encoding (.getCharacterEncoding request)
   :body               (.getInputStream request)})

(defn -service
  [^HttpServlet _ ^HttpServletRequest request ^HttpServletResponse response]
  (if-let [response-map (doall (app (request-map _ request response)))]
    (servlet-response response response-map)
    (throw (RuntimeException. "handler returned nil (no response map)"))))
