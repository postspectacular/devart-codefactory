(ns codefactory.handler
  (:require
   [codefactory.config :as config]
   [codefactory.model :as model]
   [codefactory.view :as view]
   [thi.ng.gae.services.datastore :as ds]
   [thi.ng.gae.middleware.multipart-params :refer [wrap-multipart-params]]
   [thi.ng.validate.core :as v]
   [compojure.core :refer [defroutes context routes GET POST]]
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

(defn validate-params
  [params & val-keys]
  (v/validate params (get-in config/app (cons :validators val-keys))))

(defn format-validation-errors
  [errs]
  (->> errs
       (map (fn [[k msg]] (str "param("(name k) "): " (first msg) "\n")))
       (concat ["The request had the following errors:\n"])
       (apply str)))

(defn valid-accept?
  [req & types]
  (let [^String accept (:accept (:headers req))]
    (or (= accept "*/*")
        (some (fn [^String mime] (not (neg? (.indexOf accept mime))))
              (vals (select-keys mime-types types))))))

(defn valid-api-request?
  [req]
  (valid-accept? req :edn :json))

(defn invalid-api-response
  []
  (-> (str "Only the following content types are supported: "
           (:edn mime-types) ", " (:json mime-types))
      (resp/response)
      (resp/status 406)
      (resp/content-type (:text mime-types))))

(defn api-response
  [req data]
  (let [^String accept (:accept (:headers req))
        {:keys [edn json text]} mime-types]
    (cond
     (or (= accept "*/*") (not (neg? (.indexOf accept ^String edn))))
     (-> data pr-str resp/response (resp/content-type edn))

     (not (neg? (.indexOf accept ^String json)))
     (-> data json/write-str resp/response (resp/content-type json))

     :else (invalid-api-response))))

(def api-v1-handlers
  (routes
   (GET "/objects" [:as req]
        (if (valid-api-request? req)
          (let [entities (ds/query CodeTree :sort [[:created :desc]])]
            (api-response req (map #(into {} %) entities)))
          (invalid-api-response)))
   (POST "/objects" {:keys [params] :as req}
         (prn :api params)
         (if (valid-api-request? req)
           (let [[params err] (validate-params params :api :new-object)]
             (if (nil? err)
               (try
                 (let [{:strs [tree author title parent]} params
                       entity (model/make-code-tree
                               {:id (str (java.util.UUID/randomUUID))
                                :parent-id parent
                                :tree tree
                                :author author
                                :title title
                                :created (time/datetime->epoch (time/utc-now))})]
                   (prn :created-entity (:id entity))
                   (ds/save! entity)
                   (api-response req entity))
                 (catch Exception e
                   (.printStackTrace e)
                   {:status 500
                    :body "Error saving tree"}))
               {:status 406
                :body (format-validation-errors err)}))
           (invalid-api-response)))
   (GET "/objects/:id" [id :as req]
        (if (valid-api-request? req)
          (let [[params err] (validate-params {:id id} :api :get-object)]
            (if (nil? err)
              (if-let [entity (ds/retrieve CodeTree id)]
                (api-response req (into {} entity))
                {:status 404})
              {:status 406
               :body (format-validation-errors err)}))
          (invalid-api-response)))))

(defroutes handlers
  (context "/api/1.0" [] api-v1-handlers)
  (route/not-found "That's a 404"))

(def app
  (-> handlers
      wrap-params
      wrap-multipart-params))
