(ns codefactory.editor
  (:require
   [codefactory.config :as config]))

(def module-spec
  {:controllers
   [{:id "EditObjectController"
     :spec #js ["$scope" "$routeParams"
                (fn [$scope $routeParams]
                  (prn :init "EditObjectController" $routeParams))]}]})
