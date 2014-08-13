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

(defn content-attachment
  [path]
  (str "attachment; filename=\"" (util/get-filename path) "\""))

(defn validate-params
  [params id]
  (cv/validate-params params (get-in config/app [:validators :tasks id])))

(defn update-state-with-try
  [f state]
  (try
    (f state)
    (catch Exception e
      (prn :warn (.getMessage e))
      state)))

(defn generate-stl-asset
  [obj mesh service bucket base-path]
  (update-state-with-try
   (fn [_]
     (let [stl-path (str base-path (:id obj) ".stl")]
       (store/put!
        service bucket
        stl-path (geom/mesh->stl-bytes mesh)
        {:acl :public-read :mime (:stl config/mime-types)
         :disposition (content-attachment stl-path)})
       (assoc obj :stl-uri (shared/storage-url (str "/" stl-path)))))
   obj))

(defn generate-svg-asset
  [obj mesh service bucket base-path]
  (update-state-with-try
   (fn [_]
     (let [svg-path (str base-path (:id obj) ".svg")]
       (store/put!
        service bucket
        svg-path (geom/render-preview mesh (-> config/app :preview))
        {:acl :public-read :mime (:svg config/mime-types)})
       (assoc obj :preview-uri (shared/storage-url (str "/" svg-path)))))
   obj))

(defn generate-lux-asset
  [obj mesh service bucket base-path]
  (update-state-with-try
   (fn [_]
     (let [lux-path (str base-path (:id obj) "-lux.zip")]
       (store/put!
        service bucket
        lux-path (-> (geom/generate-lux-scene mesh (-> config/app :lux))
                     (geom/lux->zip-bytes))
        {:mime (:binary config/mime-types)
         :disposition (content-attachment lux-path)})
       (assoc obj :lux-uri (shared/storage-url (str "/" lux-path)))))
   obj))

(def handlers
  (routes
   (POST "/process-object" [:as req]
         (let [{:keys [id tree seed]} (task/get-edn-payload req)]
           (if (and id tree seed)
             (if-let [mesh (geom/generate-mesh tree seed)]
               (try
                 (let [obj       (ds/retrieve CodeTree id)
                       service   (store/get-service)
                       bucket    (-> config/app :storage :bucket)
                       base-path (str "objects/" id "/")]
                   (-> obj
                       (generate-stl-asset mesh service bucket base-path)
                       (generate-svg-asset mesh service bucket base-path)
                       (generate-lux-asset mesh service bucket base-path)
                       (ds/save!))
                   (resp/response "ok"))
                 (catch Exception e
                   (.printStackTrace e)
                   (resp/response (str "error: " (.getMessage e)))))
               (resp/response "error: mesh gen failed"))
             (resp/response "error: wrong/empty payload"))))
   (POST "/regenerate-assets" [:as req]
         (let [[params err] (validate-params (:params req) :regen-assets)]
           (if (nil? err)
             (try
               (let [{:strs [since until] :or {since 0 until 0}} params
                     f-since [:>= :created since]
                     f-until [:<= :created until]
                     filter (if (pos? since)
                              (if (pos? until) [:and f-since f-until] f-since)
                              f-until)
                     _ (prn :filter filter)
                     entities (ds/query CodeTree
                                        :filter filter
                                        :limit 100)
                     _ (prn :regenerate (count entities) "objects")
                     [ok err] (reduce
                               (fn [[ok err] {:keys [id tree seed]}]
                                 (try
                                   (prn :queue id)
                                   (task/queue!
                                    nil {:url "/tasks/process-object"
                                         :headers {"Content-Type" (:edn config/mime-types)}
                                         :payload {:id id :tree tree :seed (keyword seed)}})
                                   [(conj ok id) err]
                                   (catch Exception e
                                     (prn "error: couldn't initiate object processing: "
                                          (.getMessage e))
                                     [ok (conj err id)])))
                               [[] []] entities)]
                 (-> (pr-str {:ok ok :err err
                              :last (select-keys (last entities) [:id :created])})
                     (resp/response)
                     (resp/content-type (:edn config/mime-types))))
               (catch Exception e
                 (.printStackTrace e)
                 (resp/response (str "error: " (.getMessage e)))))
             (resp/response (pr-str err)))))))
