(ns thi.ng.gae.util
  (:require
   [camel-snake-kebab :as csk]))

(def ->kebab-case (memoize csk/->kebab-case))

(defn unqualified-name
  "Returns the non-namespaced part of a symbol."
  [sym]
  (let [name (str sym)
        idx (.lastIndexOf name "/")]
    (.substring name (inc (if (neg? idx) (.lastIndexOf name ".") idx)))))

(defn class-name-vec
  "Takes a class and returns vector of [ns localname]."
  [clz]
  (let [name (.getName clz)
        idx (.lastIndexOf name ".")]
    (if (neg? idx)
      ["" name]
      [(.substring name 0 idx) (.substring name (inc idx))])))

(defn array-of
  "Creates a new array of given type and fills it with the elements of
  given coll, each passed through f. Returns array."
  [type f xs]
  (->> xs (map f) (into-array type)))

(defn str-contains?
  [^String str ^String x] (not (neg? (.indexOf str x))))

(defn parse-int
  [x nf] (try (Integer/parseInt x) (catch Exception e)))

(defn parse-double
  [x nf] (try (Double/parseDouble x) (catch Exception e)))
