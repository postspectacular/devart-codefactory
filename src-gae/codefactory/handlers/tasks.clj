(ns codefactory.handlers.tasks
  (:require
   [codefactory.config :as config]
   [codefactory.model :as model]
   [codefactory.validate :as cv]
   [codefactory.handlers.shared :as shared]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.mesh.io :as mio]
   [thi.ng.morphogen.core :as mg]
   [thi.ng.gae.services.datastore :as ds]
   [thi.ng.gae.services.taskqueue :as task]
   [thi.ng.gae.services.storage :as store]
   [thi.ng.gae.util :as util]
   [thi.ng.validate.core :as v]
   [compojure.core :refer [routes GET POST]]
   [ring.util.response :as resp]
   [simple-time.core :as time]
   [clojure.java.io :as io])
  (:import
   [java.io ByteArrayOutputStream]
   [codefactory.model CodeTree PrintJob]))

(defn build-stl
  [tree seed-id]
  (prn :generate-stl tree seed-id)
  (if-let [seed (get-in config/seeds [(keyword seed-id) :seed])]
    (let [mesh (-> seed
                   (mg/walk tree)
                   (mg/union-mesh)
                   (g/tessellate))]
      (with-open [out (ByteArrayOutputStream. 0x4000)]
        (mio/write-stl out mesh)
        (.toByteArray out)))
    (prn :error "invalid seed id" seed-id)))

(def handlers
  (routes
   (POST "/process-object" [:as req]
         (let [{:keys [id tree seed]} (task/get-edn-payload req)
               stl-bytes (build-stl tree seed)
               base-path (str "objects/" id "/")
               stl-path  (str base-path id ".stl")]
           (when stl-bytes
             (prn :stl-path stl-path (alength stl-bytes))
             (try
               (let [obj (ds/retrieve CodeTree id)
                     obj (assoc obj
                           :stl-uri (shared/storage-url stl-path)
                           :stl-created (time/datetime->epoch (time/utc-now)))]
                 (prn :update-obj obj)
                 (store/put!
                  (store/get-service)
                  (-> config/app :storage :bucket)
                  stl-path stl-bytes
                  {:acl :public-read :mime (:stl config/mime-types)})
                 (ds/save! obj))
               (catch Exception e
                 (prn :warn (.getMessage e)))))
           (resp/response "ok")))))
