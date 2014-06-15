(ns codefactory.gallery
  (:require
   [codefactory.config :as config]
   [thi.ng.geom.webgl.core :as gl]))

(def module-spec
  {:controllers
   [{:id "GalleryController"
      :spec #js ["$scope" "$routeParams"
                 (fn [$scope $routeParams]
                   (prn :init "GalleryController" $routeParams))]}]})
