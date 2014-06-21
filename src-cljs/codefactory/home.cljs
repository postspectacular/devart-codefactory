(ns codefactory.home)

(def controller-id "HomeController")

(def module-spec
  {:directives
   [{:id "videoBackground"
     :spec
     #js
     ["$window"
      (fn [$window]
        #js
        {:restrict "A"
         :scope #js {:aspect "="}
         :link (fn [scope element attribs]
                 (let [aspect (.-aspect scope)]
                   (set! (.-resizeBg scope)
                         (fn []
                           (let [ww (.-innerWidth $window)
                                 wh (.-innerHeight $window)]
                             (if (< (/ ww wh) aspect)
                               (let [iw (bit-or (* wh aspect) 0)
                                     left (bit-shift-right (- ww iw) 1)]
                                 (.css element #js {:left (str left "px")
                                                    :width (str iw "px")
                                                    :height (str wh "px")
                                                    :top "0px"}))
                               (let [ih (bit-or (/ ww aspect) 0)
                                     top (bit-shift-right (- wh ih) 1)]
                                 (.css element #js {:left "0px"
                                                    :width (str ww "px")
                                                    :height (str ih "px")
                                                    :top (str top "px")}))))))

                   (set! (.-init scope)
                         (fn []
                           (.css element #js {:position "fixed" :z-index "-100"})
                           (.resizeBg scope)
                           (.play (aget element 0))
                           (.addEventListener $window "resize" (.-resizeBg scope) false)))



                   (.$on scope "$destroy"
                         (fn [] (.removeEventListener $window "resize" (.-resizeBg scope))))

                   (.init scope)))})]}]
   :controllers
   [{:id controller-id
     :spec #js ["$scope" "$routeParams" "$window"
                (fn [$scope $routeParams $window]
                  (prn :init "HomeController" $routeParams)

                  (set! (.-launchFullScreen $scope)
                        (fn []
                          #_(let [doc (.-documentElement js/document)]
                            (cond
                             (.-requestFullscreen doc) (.requestFullscreen doc)
                             (.-mozRequestFullscreen doc) (.mozRequestFullscreen doc)
                             (.-webkitRequestFullscreen doc) (.webkitRequestFullscreen doc)
                             (.-msRequestFullscreen doc) (.msRequestFullscreen doc)
                             :default nil)))))]}]})
