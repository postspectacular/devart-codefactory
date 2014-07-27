(ns codefactory.handler
  (:require
   [codefactory.config :as config]
   [codefactory.model :as model]
   [codefactory.view :as view]
   [thi.ng.gae.services.datastore :as ds]
   [thi.ng.gae.middleware.multipart-params :refer [wrap-multipart-params]]
   [thi.ng.gae.util :as util]
   [thi.ng.validate.core :as v]
   [compojure.core :refer [defroutes context routes GET POST]]
   [compojure.route :as route]
   [ring.middleware.params :refer [wrap-params]]
   [ring.util.response :as resp]
   [simple-time.core :as time]
   [clojure.data.json :as json]
   [clojure.java.io :as io]
   [clojure.edn :as edn])
  (:import
   [codefactory.model CodeTree]))

(def mime-types
  {:edn "application/edn"
   :json "application/json"
   :text "text/plain"})

(def api-mime-types [:edn :json])

(defn validate-params
  [params & val-keys]
  (v/validate params (get-in config/app (cons :validators val-keys))))

(defn valid-accept?
  [req & types]
  (let [^String accept (:accept (:headers req))]
    (or (= accept "*/*")
        (some (fn [^String mime] (util/str-contains? accept mime))
              (vals (select-keys mime-types types))))))

(defn valid-api-accept?
  [req] (apply valid-accept? req api-mime-types))

(defn basic-api-response-body
  [data status]
  (if (< status 400)
    {:status "ok" :body data}
    {:status "error" :errors data}))

(defn api-response
  [req data status]
  (let [^String accept (:accept (:headers req))
        {:keys [edn json text]} mime-types
        body (basic-api-response-body data status)
        [body type] (cond
                     (or (= accept "*/*") (util/str-contains? accept edn))
                     [(pr-str body) edn]

                     (util/str-contains? accept json)
                     [(json/write-str body) json]

                     :else [(pr-str body) text])]
    (-> (resp/response body)
        (resp/status status)
        (resp/content-type type))))

(defn invalid-api-response
  []
  (-> (apply str
             "Only the following content types are supported: "
             (interpose ", " (vals (select-keys mime-types api-mime-types))))
      (resp/response)
      (resp/status 406)
      (resp/content-type (:text mime-types))))

(defn public-entity
  [e key-id]
  (select-keys e (get-in config/app [:db key-id])))

(def api-v1-handlers
  (routes

   (GET "/objects" [:as req]
        (prn :params (:query-params req))
        (if (valid-api-accept? req)
          (let [[params err] (validate-params (:query-params req) :api :query-objects)]
            (if (nil? err)
              (let [{:strs [limit offset]} params
                    entities (ds/query CodeTree
                                       :sort [[:created :desc]]
                                       :limit limit
                                       :offset offset)
                    entities (mapv #(public-entity % :public-codetree-keys) entities)]
                (api-response req entities 200))
              (api-response req err 400)))
          (invalid-api-response)))

   (POST "/objects" {:keys [params] :as req}
         (prn :api params)
         (if (valid-api-accept? req)
           (let [[params err] (validate-params params :api :new-object)]
             (if (nil? err)
               (try
                 (let [{:strs [tree seed author title parent location]} params
                       entity (model/make-code-tree
                               {:id (str (java.util.UUID/randomUUID))
                                :parent-id parent
                                :tree (edn/read-string tree)
                                :seed seed
                                :author author
                                :author-location location
                                :title title
                                :created (time/datetime->epoch (time/utc-now))})]
                   (prn :created-entity (:id entity))
                   (ds/save! entity)
                   (api-response req (public-entity entity :public-codetree-keys) 201))
                 (catch Exception e
                   (.printStackTrace e)
                   {:status 500
                    :body "Error saving tree"}))
               (api-response req err 400)))
           (invalid-api-response)))

   (GET "/objects/:id" [id :as req]
        (if (valid-api-accept? req)
          (let [[params err] (validate-params {:id id} :api :get-object)]
            (if (nil? err)
              (if-let [entity (ds/retrieve CodeTree id)]
                (api-response
                 req (public-entity entity :public-codetree-keys) 200)
                (api-response req {:reason (str "Unknown ID: " id)} 404))
              (api-response req err 400)))
          (invalid-api-response)))))

(defroutes handlers
  (context "/api/1.0" [] api-v1-handlers)
  (route/not-found "That's a 404"))

(def app
  (-> handlers
      wrap-params
      wrap-multipart-params))
