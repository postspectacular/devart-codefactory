(ns thi.ng.cljs.detect
  (:require
   [goog.style :as style]))

(def mobile?
  (and (re-find #"(?i)mobile|tablet|ip(ad|hone|od)|android|silk" (.-userAgent js/navigator))
       (not (re-find #"(?i)crios" (.-userAgent js/navigator)))))

(def safari? (re-find #"(?i)safari" (.-userAgent js/navigator)))
