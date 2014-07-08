(ns codefactory.shared
  (:require
   [thi.ng.cljs.dom :as dom]))

(defn show-nav
  [] (dom/remove-class! (dom/query nil "nav") "hidden"))

