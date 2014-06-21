(ns codefactory.handler
  (:require
   [codefactory.config :as config]
   [codefactory.model :as model]
   [codefactory.view :as view]
   [thi.ng.gae.services.datastore :as ds]
   [thi.ng.gae.middleware.multipart-params :refer [wrap-multipart-params]]
   [compojure.core :refer [defroutes GET POST]]
   [compojure.route :as route]
   [ring.middleware.params :refer [wrap-params]]
   [ring.util.response :as resp]
   [simple-time.core :as time]
   [clojure.data.json :as json]
   [clojure.java.io :as io])
  (:import
   [codefactory.model CodeTree]))

(defroutes handlers
  (GET "/" []
       (resp/response (view/main-wrapper config/app)))
  (GET "/templates/:id" [id]
       (if-let [tpl (view/templates (keyword id))]
         (resp/response (tpl config/app {:video-id "virus"}))
         (resp/not-found "Invalid template ID")))
  (POST "/submit" {:keys [body] :as req}
        (try
          (let [data (json/read (io/reader body) :key-fn keyword)
                entity (model/make-code-tree
                        {:id (str (java.util.UUID/randomUUID))
                         :title "session 1"
                         :user "toxi"
                         :tree data
                         :stl? false
                         :date (time/format (time/now) "YYYY,mm,DD,HH,MM,ss")})]
            (prn :created-entity (:id entity))
            (ds/save! entity)
            (resp/response (json/write-str entity)))
          (catch Exception e
            (.printStackTrace e)
            {:status 400
             :body "Error saving tree"})))
  (route/not-found "404"))

(def app
  (-> handlers
      wrap-params
      wrap-multipart-params))
