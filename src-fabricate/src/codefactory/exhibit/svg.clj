(ns codefactory.exhibit.svg
  (:require
   [hiccup.core :refer [html]]
   [hiccup.def :refer [defelem]]))

(defn ->xml
  [dom]
  (html dom))

(defelem svg
  [body]
  [:svg
   {"xmlns" "http://www.w3.org/2000/svg"
    "version" "1.0"}
   body])

(defelem path
  [points]
  (let [coords (map (fn [[x y]] (format "%1.5f,%1.5f" (double x) (double y))) points)
        coords (apply str (concat "M" (interpose " L" coords) "Z"))]
    [:path {:d coords}]))

(defn rgb->hex
  ([[r g b]] (rgb->hex r g b))
  ([r g b] (format "#%02x%02x%02x" r g b)))

