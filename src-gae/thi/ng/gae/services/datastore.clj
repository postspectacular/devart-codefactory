(ns thi.ng.gae.services.datastore
  (:require
   [thi.ng.gae.services.core :refer [defservice]]
   [thi.ng.gae.util :as util]
   [clojure.edn :as edn])
  (:import
   [com.google.appengine.api.datastore
    DatastoreService DatastoreServiceFactory
    Entity EntityNotFoundException
    Key KeyFactory]
   [com.google.appengine.api.datastore
    Blob ShortBlob Text Link]
   [com.google.appengine.api.datastore
    Query PreparedQuery
    Query$Filter Query$FilterOperator Query$FilterPredicate Query$CompositeFilterOperator
    Query$SortDirection
    FetchOptions$Builder]))

(defservice datastore-service
  *datastore-service* DatastoreService
  (DatastoreServiceFactory/getDatastoreService))

(defprotocol PEntity
  (clj-properties [_])
  (entity-kind [_])
  (gae-entity [_]))

(defprotocol PEntityKeyLookup
  (entity-key [this] [this parent]))

(extend-protocol PEntityKeyLookup
  Key
  (entity-key [this] this)
  String
  (entity-key [this] (KeyFactory/createKey this 1)))

(defn generate-key
  [kind id & [parent]]
  (let [id (if (number? id) (long id) (str id))
        pk (when (satisfies? PEntityKeyLookup parent)
             (entity-key parent))
        k (KeyFactory/createKey ^Key pk kind id)]
    ;;(prn :gen-key k :args [kind id parent pk])
    k))

(defn as-gae-entity
  [e]
  (let [e* (Entity. ^Key (entity-key e))
        clj-props (clj-properties e)]
    (doseq [[k v] e]
      (.setProperty e* (name k) (if (clj-props k) (Text. (pr-str v)) v)))
    e*))

(defn as-entity
  ([type e]
     (let [[ns kind] (util/class-name-vec type)]
       (as-entity ns kind e)))
  ([ns kind e]
     (let [ctor      (->> (util/->kebab-case kind)
                          (str "make-")
                          (symbol ns))
           key       (.getKey e)
           parent    (.getParent e)
           e'        ((find-var ctor) {} :key key :parent parent)
           clj-props (clj-properties e')]
       (->> e
            (.getProperties)
            (reduce
             (fn [e' [k v]]
               (let [k (keyword k)]
                 (assoc e' k (if (clj-props k)
                               (edn/read-string (.getValue ^Text v))
                               v))))
             e')))))

(defn retrieve
  [type id & [parent]]
  (try
    (let [[ns kind] (util/class-name-vec type)
          ;;_ (prn :ns ns :kind kind)
          e (.get (datastore-service)
                  (generate-key kind id parent))]
      (as-entity ns kind e))
    (catch EntityNotFoundException _)))

(defn save!
  [e]
  (.put (datastore-service) ^Entity (gae-entity e))
  e)

(defmacro defentity
  [name props & {:keys [key parent]}]
  (let [kind      (str name)
        kind*     (util/->kebab-case kind)
        props     (vec props)
        clj-props (->> props
                       (filter #(contains? (meta %) :clj))
                       (map keyword)
                       (set))
        arglist   '[args & {k :key p :parent}]
        ->name    (symbol (str "->" kind))
        map->name (symbol (str "map->" kind))
        ctor-name (symbol (str "make-" kind*))]
    `(do
       (defrecord ~name ~props
         PEntityKeyLookup
         (entity-key
           [this#] (:key (meta this#)))
         PEntity
         (clj-properties
           [this#] ~clj-props)
         (entity-kind
           [this#] ~kind)
         (gae-entity
           [this#] (as-gae-entity this#)))
       (defn ~ctor-name ~arglist
         (let [e#   (if (map? ~'args)
                      (~map->name ~'args)
                      (apply ~->name ~'args))
               kn#  (if (fn? ~key)
                      (~key e#)
                      (or ~'k (~key e#)))
               key# (generate-key ~kind kn# (or ~'p ~parent))]
           (vary-meta e# merge {:key key#}))))))

(def ^:private filter-ops
  {:=  Query$FilterOperator/EQUAL
   :!= Query$FilterOperator/NOT_EQUAL
   :<  Query$FilterOperator/LESS_THAN
   :<= Query$FilterOperator/LESS_THAN_OR_EQUAL
   :>  Query$FilterOperator/GREATER_THAN
   :>= Query$FilterOperator/GREATER_THAN_OR_EQUAL
   :in Query$FilterOperator/IN})

(def ^:private sort-ops
  {:asc  Query$SortDirection/ASCENDING
   :desc Query$SortDirection/DESCENDING})

(defn ^Query$Filter compile-filter
  [[op id v :as exp]]
  (if-let [op* (filter-ops op)]
    (Query$FilterPredicate. (name id) op* v)
    (condp = op
      :and (Query$CompositeFilterOperator/and (util/array-of Query$Filter compile-filter [id v]))
      :or  (Query$CompositeFilterOperator/or  (util/array-of Query$Filter compile-filter [id v]))
      (throw (IllegalArgumentException. (str "Illegal filter expression: " exp))))))

(defn attach-query-sorting
  [^Query q sort]
  (doseq [[id dir] sort]
    (.addSort q (name id) (sort-ops dir)))
  q)

(defn apply-fetch-options
  [{:keys [limit offset]} ^PreparedQuery q]
  (let [fetch (cond-> (FetchOptions$Builder/withDefaults)
                      limit (.limit limit)
                      offset (.offset offset))]
    (.asIterable q fetch)))

(defn query
  [type & {:keys [parent filter sort keys-only? limit offset]}]
  (let [[ns kind] (util/class-name-vec type)]
    (->> (cond-> (Query. kind)
                 filter     (.setFilter (compile-filter filter))
                 parent     (.setAncestor (entity-key parent))
                 sort       (attach-query-sorting sort)
                 keys-only? (.setKeysOnly)) ;; TODO handle results
         (.prepare (datastore-service))
         (apply-fetch-options {:limit limit :offset offset})
         (map #(as-entity ns kind %)))))
