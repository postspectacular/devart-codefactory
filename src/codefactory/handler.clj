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
   [ring.util.response :as resp])
  (:import
   [codefactory.model CodeTree]))

(defroutes handlers
  (GET "/" []
       (resp/response (view/main-wrapper config/app)))
  (GET "/templates/:id" [id]
       (if-let [tpl (view/templates (keyword id))]
         (resp/response (tpl config/app {:video-id "virus"}))
         (resp/not-found "Invalid template ID")))  
  (route/not-found "404"))

(def app
  (-> handlers
      wrap-params
      wrap-multipart-params))
