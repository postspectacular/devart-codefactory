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
   [thi.ng.gae.util :as util]
   [thi.ng.validate.core :as v]
   [compojure.core :refer [routes GET POST]]
   [ring.util.response :as resp]
   [simple-time.core :as time]
   [clojure.java.io :as io])
  (:import
   [java.io ByteArrayOutputStream]))

(defn build-stl
  [tree seed-id]
  (prn :generate-stl tree seed-id)
  (if-let [seed (get-in config/seeds [(keyword seed-id) :seed])]
    (let [mesh (-> seed
                   (mg/walk tree)
                   (mg/union-mesh)
                   (g/tessellate))]
      (with-open [out (ByteArrayOutputStream. 0x10000)]
        (mio/write-stl out mesh)
        (.toByteArray out)))
    (prn :error "invalid seed id" seed-id)))

(def handlers
  (routes
   (POST "/process-object" [:as req]
         (let [{:keys [id tree seed]} (task/get-edn-payload req)
               stl (build-stl tree seed)]
           (prn :stl (if stl (alength stl)))
           (resp/response "ok")))))
