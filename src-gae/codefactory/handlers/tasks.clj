(ns codefactory.handlers.tasks
  (:require
   [codefactory.config :as config]
   [codefactory.model :as model]
   [codefactory.geom :as geom]
   [codefactory.validate :as cv]
   [codefactory.handlers.shared :as shared]
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
   [codefactory.model CodeTree PrintJob]))

(def handlers
  (routes
   (POST "/process-object" [:as req]
         (let [{:keys [id tree seed]} (task/get-edn-payload req)
               mesh (geom/generate-mesh tree seed)]
           (when mesh
             (try
               (let [base-path (str "objects/" id "/")
                     stl-path  (str base-path id ".stl")
                     svg-path  (str base-path id ".svg")
                     lux-path  (str base-path id "-lux.zip")
                     obj       (ds/retrieve CodeTree id)
                     obj       (assoc obj
                                 :stl-uri (shared/storage-url (str "/" stl-path))
                                 :stl-created (time/datetime->epoch (time/utc-now))
                                 :preview-uri (shared/storage-url (str "/" svg-path)))
                     service   (store/get-service)
                     bucket    (-> config/app :storage :bucket)]
                 (store/put!
                  service bucket
                  stl-path (geom/mesh->stl-bytes mesh)
                  {:acl :public-read :mime (:stl config/mime-types)})
                 (store/put!
                  service bucket
                  svg-path (geom/render-preview mesh (-> config/app :preview))
                  {:acl :public-read :mime (:svg config/mime-types)})
                 (store/put!
                  service bucket
                  lux-path (-> (geom/generate-lux-scene mesh (-> config/app :lux))
                               (geom/lux->zip-bytes)))
                 (ds/save! obj))
               (catch Exception e
                 (.printStackTrace e))))
           (resp/response "ok")))))
