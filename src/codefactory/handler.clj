(ns codefactory.handler
  (:require
   [codefactory.model :as model]
   [compojure.core :refer [defroutes GET POST]]
   [compojure.route :as route]
   [ring.middleware.params :refer [wrap-params]]
   [ring.util.response :as resp]
   [thi.ng.gae.middleware.multipart-params :refer [wrap-multipart-params]]
   [thi.ng.gae.services
    [user :as user]
    [datastore :as ds :refer [defentity]]]
   [hiccup
    [core :refer [h]]
    [page :refer [html5]]])
  (:import
   [codefactory.model CodeTree]))

(defentity Triple [s p o] :key :s)

(defroutes handlers
  (GET "/object/:id" [id :as req]
       (let [o (or
                (ds/retrieve CodeTree id)
                (ds/save! (model/make-code-tree
                           {:id id :user "toxi" :tree {:a 42} :title "Foo" :has-stl? false})))]
         {:status 200
          :body [(pr-str o) (ds/clj-properties o)]}))
  (GET "/graph/:gid/:subj" [gid subj :as req]
       (let [graph (ds/generate-key "Graph" gid)
             t (or
                (ds/retrieve Triple subj graph)
                (ds/save! (make-triple [subj "created" (java.util.Date.)] :parent graph)))]
         {:status 200
          :body (html5
                 [:body
                  [:div "/graph" gid "/" subj " (" (h graph) ")"]
                  [:div (h (pr-str :triple t (meta t)))]
                  [:div (h (pr-str req))]])}))
  (GET "/graph/:gid" [gid]
       (let [graph (ds/generate-key "Graph" gid)
             items (ds/query Triple :parent graph :filter [:and [:>= :s "a"] [:<= :s "z"]] :sort [[:s :desc]])]
         {:status 200
          :body (html5
                 [:body
                  [:ul
                   (map (fn [e] [:li (h (pr-str {:triple e :meta (meta e)}))]) items)]])}))
  (GET "/upload" []
       (if (user/logged-in?)
         {:status 200
          :body (html5
                 [:body
                  [:form {:action "/post"
                          :method "POST"
                          :enctype "multipart/form-data"}
                   [:input {:type "file" :name "f1"}]
                   [:input {:type "file" :name "f2"}]
                   [:input {:type "submit" :value "upload"}]]
                  [:div [:pre (h (pr-str (user/current-user)))]]
                  [:div [:a {:href (user/logout-url "/")} "logout"]]])}
         (resp/redirect (user/login-url "/upload"))))
  (POST "/post" [:as req]
        {:status 200
         :body (pr-str req)})
  (route/not-found "404"))

(def app
  (-> handlers
      wrap-params
      wrap-multipart-params))
