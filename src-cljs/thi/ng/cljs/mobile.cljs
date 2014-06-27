(ns thi.ng.cljs.mobile
  (:require
   [goog.style :as style]))

(defn mobile?
  []
  (and (re-find #"(?i)mobile|tablet|ip(ad|hone|od)|android|silk" (.-userAgent js/navigator))
       (not (re-find #"(?i)crios" (.-userAgent js/navigator)))))

(defn remove-address-bar
  []
  (let [[overflow height] (if (zero? (.-orientation js/window))
                            ["scroll" "120%"]
                            ["" "100%"])]
    (style/setStyle (.-documentElement js/document) "overflow" overflow)
    (style/setStyle (.-body js/document) "height" height)
    (js/setTimeout (fn [] (.scrollTo js/window 0 1)) 10)))

(defn hide-address-bar
  []
  (when (mobile?)
    (.addEventListeer js/window "load" remove-address-bar false)
    (.addEventListeer js/window "orientationchange" remove-address-bar false)))
