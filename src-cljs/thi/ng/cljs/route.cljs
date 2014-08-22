(ns thi.ng.cljs.route
  (:require
   [thi.ng.validate.core :as v]
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.utils :as utils]
   [clojure.string :as str]))

(defn set-location!
  [url] (set! (.-location js/window) url))

(defn get-route
  [] (-> js/window (.-location) (.-hash) (.split "/") (.slice 1)))

(defn format-route
  [id & params]
  (apply str (interpose "/" (into ["#" (name id)] params))))

(defn set-route!
  [id & params]
  (set! (.-hash (.-location js/window)) (apply format-route id params)))

(defn replace-route!
  [id & params]
  (.replace (.-location js/window) (apply format-route id params)))

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
  [routes default route-changed]
  (let [routes (filter (complement :disabled) routes)]
    (fn []
      (let [info (match-route routes)]
        (if info
          (route-changed info)
          (do
            (warn "no matching route:" (get-route) "redirect to default...")
            (apply set-route! (:hash default) (:params default))))))))

(defn start-router!
  [router]
  (.addEventListener js/window "hashchange" router)
  (router))

(defn local?
  []
  (let [host (.-hostname (.-location js/window))]
    (re-find #"localhost|(192\.168\.)|(127\.0\.0\.1)" host)))
