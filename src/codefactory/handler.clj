(ns codefactory.handler
  (:require
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
       (resp/response (view/view-home)))
  (GET "/templates/home" []
       (resp/response (view/template-home {:video-id "virus"})))
  
  (route/not-found "404"))

(def app
  (-> handlers
      wrap-params
      wrap-multipart-params))
