(ns codefactory.controllers.home
  (:require
   [codefactory.config :as config]
   [codefactory.protocols :as proto]
   [thi.ng.cljs.app :as app]
   [thi.ng.cljs.dom :as dom]
   [thi.ng.cljs.io :as io]))

(deftype HomeController
    [^:mutable shared state]
  proto/PLifecycle
  (init [_ s]
    (prn :init-home)
    (set! shared s))
  (release [_]
    (prn :release-home)))

(def instance (HomeController. nil nil))
