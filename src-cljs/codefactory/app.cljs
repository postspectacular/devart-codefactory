(ns codefactory.app
  (:require-macros
   [cljs.core.async.macros :refer [go]])
  (:require
   [codefactory.config :as config]
   [codefactory.home :as home]
   [codefactory.editor :as editor]
   [codefactory.selector :as selector]
   [codefactory.object-loader :as obj]
   [codefactory.submit :as submit]
   [codefactory.thanks :as thanks]
   [codefactory.about :as about]
   [thi.ng.cljs.async :as async]
   [thi.ng.cljs.io :as io]
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.route :as route]
   [thi.ng.cljs.utils :as utils]
   [thi.ng.cljs.dom :as dom]
   [thi.ng.cljs.detect :as detect]
   [goog.events :as events]
   [cljs.core.async :as cas :refer [>! <! chan put! close! timeout]]))

(defn transition-dom
  [a b]
  (when-not (= a b)
    (let [ea (dom/by-id (name a))
          eb (dom/by-id (name b))
          dir (if (pos? (config/transition a b)) "next" "prev")]
      (dom/set-class! ea dir)
      (dom/set-class! eb "current"))))

(defn transition-controllers
  [state {new-id :controller params :params :as route}]
  (let [{:keys [bus ctrl-id]} @state
        delay      (config/timeout :controller)
        init-id    (keyword (str "init-" (name new-id)))
        release-id (keyword (str "release-" (name ctrl-id)))]
    (swap!
     state merge
     {:route             route
      :ctrl-id           new-id
      :last-route-change (utils/now)})
    (async/publish bus init-id [state params])
    (js/setTimeout #(async/publish bus release-id nil) delay)))

(defn listen-route-change
  [bus]
  (let [ch (async/subscribe bus :route-changed)]
    (go
      (loop []
        (let [[_ [state new-route]] (<! ch)
              {curr-id :ctrl-id} @state
              new-id (:controller new-route)]
          (debug :new-route new-route)
          (transition-controllers state new-route)
          (transition-dom curr-id new-id)
          (recur))))))

(defn listen-dom
  [bus]
  (dom/add-listeners
   [[js/window "resize"
     (fn [_]
       (async/publish
        bus :window-resize
        [(.-innerWidth js/window) (.-innerHeight js/window)]))]]))

(defn init-router
  [bus state routes default-route-id]
  (let [router (route/router
                routes (routes default-route-id)
                #(async/publish bus :route-changed [state %]))]
    (listen-route-change bus)
    (route/start-router! router)))

(defn init-modules
  [bus state]
  (listen-dom bus)
  (let [{:keys [modules routes default-route]} config/app]
    (when (:home modules)          (home/init bus))
    (when (:selector modules)      (selector/init bus))
    (when (:editor modules)        (editor/init bus))
    (when (:object-loader modules) (obj/init bus))
    (when (:submit modules)        (submit/init bus))
    (when (:thanks modules)        (thanks/init bus))
    (when (:about modules)         (about/init bus))
    (init-router bus state routes default-route)))

(defn init-fallback
  [bus state]
  (init-router
   bus state
   (:routes-unsupported config/app)
   (:default-route-unsupported config/app)))

(defn load-featured-image
  [bus url]
  (debug :load-bg url)
  (let [img (js/Image.)]
    (set! (.-onload img)
          (fn []
            (dom/set-style!
             (dom/by-id "home")
             #js {:background-image (str "url(" url ")")})
            (async/publish bus :app-ready nil)))
    (set! (.-src img) url)))

(defn load-credits
  [bus state]
  (io/request
   :uri (config/api-route :credits)
   :method :get
   :edn? true
   :success (fn [_ body]
              ;;(info :success-credits body)
              (let [{:keys [title author id created preview-uri]} (-> body :body :object)
                    preview-uri (or preview-uri (-> config/app :home :default-bg))]
                (swap!
                 state assoc :credits
                 {:title title
                  :author author
                  :id id
                  :date (utils/format-date (js/Date. created))})
                (load-featured-image bus preview-uri)))
   :error   (fn [_ body]
              ;;(warn :error-credits body)
              (load-featured-image bus (-> config/app :home :default-bg)))))

(defn check-requirements
  []
  (let [[w h :as min-size] (:min-window-size config/app)
        satisfied? (and detect/webgl?
                        (or detect/chrome? detect/firefox?)
                        (or (not min-size) (detect/min-window-size w h)))]
    ;; (debug :detect detect/webgl? detect/chrome? detect/firefox?)
    ;; false
    satisfied?))

(defn start
  []
  (config/set-config! "__APP_CONFIG__")
  (let [bus        (async/pub-sub
                    ;;(fn [e] (debug :bus (first e)) (first e))
                    first
                    )
        state      (atom {:bus bus :ctrl-id :loader})]

    (load-credits bus state)
    (go
      (<! (async/subscribe bus :app-ready))
      (if (check-requirements)
        (init-modules bus state)
        (init-fallback bus state)))))

(.addEventListener js/window "load" start)
