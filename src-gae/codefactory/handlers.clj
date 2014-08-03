(ns codefactory.handlers
  (:require
   [codefactory.config :as config]
   [codefactory.handlers.api :as api]
   [codefactory.handlers.tasks :as tasks]
   [thi.ng.gae.middleware.multipart-params :refer [wrap-multipart-params]]
   [compojure.core :refer [defroutes context routes GET POST]]
   [compojure.route :as route]
   [ring.middleware.params :refer [wrap-params]]
   [ring.util.response :as resp]))

(defroutes handlers
  (context "/api/1.0" [] api/handlers)
  (context "/tasks" [] tasks/handlers)
  (route/not-found "That's a 404 (Resource not found)"))

(def app
  (-> handlers
      wrap-params
      wrap-multipart-params))
