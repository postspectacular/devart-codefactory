(ns codefactory.view
  (:require
   [codefactory.config :as config]
   [hiccup
    [core :refer [html]]
    [element :refer :all]
    [page :refer :all]]))

(defn meta-tag
  [name content]
  [:meta {:name name :content content}])

(defn ie-comment
  [pred version body]
  (concat
   [(str "<!--[if " (name pred) " IE " version "] ")]
   body
   ["<![endif]-->"]))

(defn ga
  [id domain]
  (javascript-tag
   (str
    "(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
})(window,document,'script','//www.google-analytics.com/analytics.js','ga');
ga('create', '" id "', '" domain "');
ga('send', 'pageview');")))

(defn head
  [config & {:keys [title css js]}]
  [:head
   [:meta {:charset "utf-8"}]
   [:meta {:http-equiv "X-UA-Compatible" :content "IE=edge"}]
   (meta-tag :viewport "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no")
   (meta-tag :author (:author config))
   [:title (str (:title-prefix config) title)]
   (apply include-css (concat (get-in config [:includes :css]) css))
   (ie-comment :lt 9 (apply include-js (get-in config [:includes :js-ie9])))
   (apply include-js (concat (get-in config [:includes :js]) js))])

(defn html-app
  [app-id & body]
  (html
   {:mode :html}
   (doctype :html5)
   [:html {:lang "en" :ng-app (name app-id)} body]))

(def main-wrapper
  (memoize
   (fn [config]
     (let [ng (:angular config)]
       (html-app
        (:module ng)
        (head config :title "Welcome")
        [:body
         {:ng-controller (:controller ng)}
         [:div.view-container [:div.view-frame {:ng-view true}]]
         (when-let [tracking (:tracking config)] (apply ga tracking))])))))

(defn featured-video
  [config id]
  (let [{:keys [aspect formats]} (:video config)]
    [:video {:autoplay true
             :loop true
             :video-background true
             :aspect aspect}
     (map
      (fn [{:keys [ext type]}] [:source {:src (str "/img/" id ext) :type type}])
      formats)]))

(def template-home
  (memoize
   (fn [config {:keys [video-id]}]
     (html
      {:mode :html}
      (ie-comment :lt 9 (javascript-tag "document.createElement(\"video\");"))
      (featured-video config video-id)
      [:div.container-fluid
       [:div.row.home-content
        [:h1.text-center "Welcome to Co(de)Factory"]]
       [:div.row.home-content.footer
        [:div.col-md-6.col-md-offset-3.text-center
         [:a.btn.btn-primary.btn-lg {:href "#/edit/new"} "Start coding"]]]]))))

(def template-editor
  (memoize
   (fn [config opts]
     (html
      {:mode :html}
      [:div.container-fluid
       [:div.row
        [:div.col-xs-12 [:resizable-canvas {:width 1 :height 0.66}]]]
       [:div.row {:tool-bar true}
        [:div.col-xs-8.col-xs-offset-2.text-center
         (map
          (fn [op]
            [:button.btn.btn-default.btn-sm op])
          ["split" "inset" "mirror" "stretch" "scale" "tilt" "delete"])]]
       [:div.row {:tree-map true} "treemap"]
       [:div.row.footer.editor-footer
        [:div.col-xs-6 [:a.btn.btn-danger.btn-lg.btn-block {:href "#/"} "Cancel"]]
        [:div.col-xs-6 [:a.btn.btn-primary.btn-lg.btn-block {:href "#/submit"} "Submit"]]]]))))

(def templates
  {:home template-home
   :editor template-editor})
