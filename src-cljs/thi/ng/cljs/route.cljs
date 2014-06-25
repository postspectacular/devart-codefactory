(ns thi.ng.cljs.route
  (:require
   [thi.ng.validate.core :as v]
   [thi.ng.cljs.app :as app]
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.utils :as utils]
   [clojure.string :as str]))

(defn get-route
  []
  (.slice (.split (.-hash (.-location js/window)) "/") 1))

(defn set-route!
  [id & params]
  (let [hash (apply str (interpose "/" (concat ["#" (name id)] params)))]
    (set! (.-hash (.-location js/window)) hash)))

(defn match-route*
  [curr route]
  (if (= (count curr) (count route))
    (reduce
     (fn [acc [a b]]
       (cond
        (= a b) acc
        (keyword? b) (assoc acc b a)
        :else (reduced nil)))
     {} (partition 2 (interleave curr route)))))

(defn coerce-route-params
  [specs params]
  (reduce
   (fn [params [k {:keys [coerce]}]]
     (if coerce
       (if-let [pv (coerce (params k))]
         (assoc params k pv)
         (reduced nil))
       params))
   params specs))

(defn validate-route-params
  [specs params]
  (if-let [params (coerce-route-params specs params)]
    (let [valspecs (filter #(comp :validate val) specs)]
      (if (seq valspecs)
        (let [[params err] (->> valspecs
                                (reduce #(assoc % (key %2) (:validate (val %2))) {})
                                (v/validate params))]
          (if-not err params))
        params))))

(defn match-route
  [routes]
  (let [curr (get-route)]
    (some
     (fn [{:keys [match bindings controller]}]
       (if-let [params (match-route* curr match)]
         (if-let [params (if bindings (validate-route-params bindings params) params)]
           {:controller controller
            :params params
            :route curr})))
     routes)))

(defn router
  [routes default queue]
  (fn []
    (let [route-info (match-route routes)]
      (if route-info
        (app/emit queue :route-changed route-info)
        (do
          (debug "no matching route:" (get-route) ", redirect to default...")
          (apply set-route! (:hash default) (:params default)))))))

(defn start-router!
  [router]
  (.addEventListener js/window "hashchange" router)
  (router))
