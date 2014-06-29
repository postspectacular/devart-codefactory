(ns codefactory.controllers.home
  (:require
   [codefactory.config :as config]
   [codefactory.protocols :as proto]
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.route :as route]
   [thi.ng.cljs.app :as app]
   [thi.ng.cljs.dom :as dom]
   [thi.ng.cljs.io :as io]))

(defn launch-intro
  [] (route/set-route! "select-seed"))

(def dom-listeners
  [["#home-continue" "click" launch-intro]
   ["#home-fs-toggle" "click" dom/request-fullscreen]])

(deftype HomeController
    [state ^:mutable shared ^:mutable queue]
  proto/PController
  (init [_ opts]
    (debug :init-home)
    (set! shared (:state opts))
    (set! queue (:queue opts))
    (app/add-listeners dom-listeners))
  (release [_]
    (debug :release-home)
    (app/remove-listeners dom-listeners)))

(def instance (HomeController. nil nil nil))
