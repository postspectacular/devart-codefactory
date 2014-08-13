(ns codefactory.handlers.api
  (:require
   [codefactory.config :as config]
   [codefactory.model :as model]
   [codefactory.validate :as cv]
   [codefactory.handlers.shared :as shared]
   [thi.ng.gae.services.datastore :as ds]
   [thi.ng.gae.services.url-shortener :as shortener]
   [thi.ng.gae.services.taskqueue :as task]
   [thi.ng.gae.services.storage :as store]
   [thi.ng.gae.util :as util]
   [thi.ng.validate.core :as v]
   [compojure.core :refer [routes GET POST]]
   [ring.util.response :as resp]
   [simple-time.core :as time]
   [clojure.data.json :as json]
   [clojure.java.io :as io]
   [clojure.edn :as edn])
  (:import
   [codefactory.model
    CodeTree PrintJob]))

(defn validate-params
  [params id]
  (cv/validate-params params (get-in config/app [:validators :api id])))

(defn valid-api-accept?
  [req] (cv/valid-accept? req config/api-mime-types))

(defn basic-api-response-body
  [data status]
  (if (< status 400)
    {:status "ok" :body data}
    {:status "error" :errors data}))

(defn api-response
  [req data status]
  (let [accept (:accept (:headers req))
        {:keys [edn json text]} config/mime-types
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
             (interpose ", " config/api-mime-types))
      (resp/response)
      (resp/status 406)
      (resp/content-type (:text config/mime-types))))

(defn new-entity-request
  [req validate-id handler]
  (if (valid-api-accept? req)
    (let [[params err] (validate-params (:params req) validate-id)]
      (if (nil? err)
        (try
          (handler req params)
          (catch Exception e
            (.printStackTrace e)
            (api-response req "Error saving entity" 500)))
        (api-response req err 400)))
    (invalid-api-response)))

(defn get-current-job
  []
  (let [[job] (ds/query
               PrintJob
               :filter [:or [:= :status "printing"] [:= :status "complete"]]
               :sort [[:created :desc]] ;; FIXME :started
               :limit 1)
        object (if job (ds/retrieve CodeTree (:object-id job)))]
    [job object]))

(defn object-query-opts
  [filter]
  (case (keyword filter)
    :approved   [[[:status :desc] [:created :desc]] [:=  :status "approved"]]
    :unapproved [[[:status :desc] [:created :desc]] [:!= :status "approved"]]
    [[[:created :desc]] nil]))

(def handlers
  (routes

   (GET "/jobs/current" [:as req]
        (if (valid-api-accept? req)
          (let [[job object] (get-current-job)]
            (if job
              (api-response
               req
               {:job    (model/public-entity job :public-job-keys)
                :object (model/public-entity object :public-codetree-keys)}
               200)
              (api-response
               req {:reason "No print jobs"} 404)))
          (invalid-api-response)))

   (POST "/jobs" [:as req]
         (new-entity-request
          req :new-job
          (fn [req {:strs [object-id]}]
            (let [job (model/make-print-job
                       {:id        (str (java.util.UUID/randomUUID))
                        :object-id object-id
                        :status    "complete" ;; FIXME "created"
                        :created   (time/datetime->epoch (time/utc-now))})]
              (prn :created-job job)
              (ds/save! job)
              (api-response
               req (model/public-entity job :public-job-keys) 201)))))

   (GET "/objects" [:as req]
        (if (valid-api-accept? req)
          (let [[params err] (validate-params (:params req) :query-objects)]
            (if (nil? err)
              (let [{:strs [limit offset filter] :or {limit 25 offset 0}} params
                    [sort filter] (object-query-opts filter)
                    entities (ds/query
                              CodeTree
                              :sort   sort
                              :filter filter
                              :limit  limit
                              :offset offset)
                    entities (mapv #(model/public-entity % :public-codetree-keys) entities)]
                (api-response req entities 200))
              (api-response req err 400)))
          (invalid-api-response)))

   (POST "/objects" [:as req]
         (new-entity-request
          req :new-object
          (fn [req {:strs [tree seed author title parent location]}]
            (let [id        (str (java.util.UUID/randomUUID))
                  long-url  (shared/server-url req "/#/objects" id)
                  short-url (shortener/short-url long-url (-> config/app :google :api-key))
                  tree      (edn/read-string tree)
                  entity    (model/make-code-tree
                             {:id              id
                              :parent-id       parent
                              :status          "unapproved"
                              :tree            tree
                              :tree-depth      (cv/compute-tree-depth tree 0)
                              :seed            seed
                              :author          author
                              :author-location location
                              :title           title
                              :short-uri       short-url
                              :created         (time/datetime->epoch (time/utc-now))})]
              (prn :created-entity entity)
              (ds/save! entity)
              (try
                (task/queue!
                 nil {:url "/tasks/process-object"
                      :headers {"Content-Type" (:edn config/mime-types)}
                      :payload {:id id :tree tree :seed (keyword seed)}})
                (catch Exception e
                  (prn :warn "couldn't initiate object processing" (.getMessage e))))
              (api-response
               req (model/public-entity entity :public-codetree-keys) 201)))))

   (GET "/objects/:id" [id :as req]
        (if (valid-api-accept? req)
          (let [[params err] (validate-params {:id id} :get-object)]
            (if (nil? err)
              (if-let [entity (ds/retrieve CodeTree id)]
                (api-response
                 req (model/public-entity entity :public-codetree-keys) 200)
                (api-response req {:reason (str "Unknown ID: " id)} 404))
              (api-response req err 400)))
          (invalid-api-response)))

   (GET ["/objects/:id/:type" :type #"(stl|preview|lux)"] [id type :as req]
        (let [[params err] (validate-params {:id id} :get-object)]
          (if (nil? err)
            (if-let [entity (ds/retrieve CodeTree id)]
              (try
                (let [path    (str "objects/" id "/" id)
                      ext     (condp = type
                                "stl"     ".stl"
                                "preview" ".svg"
                                "lux"     "-lux.zip")
                      path    (str path ext)
                      fname   (str id ext)
                      bucket  (-> config/app :storage :bucket)
                      service (store/get-service)
                      data    (store/get service bucket path)
                      meta    (store/get-meta service bucket path)]
                  (-> (resp/response data)
                      (resp/content-type (:mime-type meta))
                      (resp/header "Content-Disposition"
                                   (str "attachment; filename=\"" fname "\""))))
                (catch Exception e
                  {:status 204}))
              (api-response req {:reason (str "Unknown ID: " id)} 404))
            (api-response req err 400))))))
