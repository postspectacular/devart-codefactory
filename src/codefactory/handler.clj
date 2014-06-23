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

(def mime-types
  {:edn "application/edn"
   :json "application/json"
   :text "text/plain"})

(defn api-response
  [req data]
  (let [accept (:accept (:headers req))
        {:keys [edn json text] mime-types}]
    (cond
     (or (= accept "*/*") (= accept edn))
     (-> data pr-str resp/response (resp/content-type edn))

     (= accept json)
     (-> data json/write-str resp/response (resp/content-type json))

     :else (-> (str "Only the following content types are supported: "
                    edn ", " json)
               (resp/response)
               (resp/status 406)
               (resp/content-type text)))))

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
  (POST "/api" {:keys [params] :as req}
        (prn :api params)
        (resp/response "ok"))
  (GET "/api/models/:id" [id :as req]
       (let [data {:seed :box :tree {:op :sd :args {:cols 3} :out [{} nil {}]}}]
         (api-response req data)))
  (route/not-found "404"))

(def app
  (-> handlers
      wrap-params
      wrap-multipart-params))
