(ns codefactory.nav
  (:require-macros
   [cljs.core.async.macros :refer [go]])
  (:require
   [codefactory.config :as config]
   [thi.ng.cljs.async :as async]
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.dom :as dom]
   [cljs.core.async :refer [<! timeout]]))

(defn hide-active-section
  [ctrl nav-body]
  (loop [ids [:home :about :gallery :selector]]
    (when ids
      (let [id (first ids)
            el (dom/by-id (str "nav-" (name id)))
            visible? (if (= id ctrl) "none" "block")]
        (if el
          (dom/set-style! el {:display visible?}))
        (recur (next ids))))))

(defn nav-item
  [id & [route label]]
  [:li {:id (str "nav-" id)}
   [:a {:href (str "#/" (or route id))} (or label id)]])

(defn init
  [bus state edit?]
  (let [nav-body  (config/dom-component :nav-body)
        nav-bt    (config/dom-component :nav-toggle)
        nav-items (cond->
                   [:ul (nav-item "home") (nav-item "about")]

                   (config/module-enabled? :gallery)
                   (conj (nav-item "gallery"))

                   edit?
                   (conj (nav-item "selector" "select" "create")))
        toggle    (async/subscribe bus :nav-toggle)
        hide      (async/subscribe bus :nav-hide)]

    (-> nav-bt
        (dom/set-style! {:visibility "visible"})
        (dom/add-class! "fade-in"))

    (dom/create-dom! nav-items nav-body)
    (dom/add-listeners
     [[nav-bt "click"
       (fn [e] (.preventDefault e) (async/publish bus :nav-toggle nil))]])
    (swap! state assoc :nav-active? false)

    (go
      (loop []
        (<! toggle)
        (if (:nav-active? @state)
          (async/publish bus :nav-hide nil)
          (do
            (hide-active-section (-> @state :route :controller) nav-body)
            (-> nav-body
                (dom/set-style! {:visibility "visible"})
                (dom/remove-class! "fade-out")
                (dom/add-class! "fade-in"))
            (-> nav-bt
                (dom/add-class! "rotate-right")
                (dom/remove-class! "rotate-left"))
            (swap! state assoc :nav-active? true)))
        (recur)))

    (go
      (loop []
        (<! hide)
        (-> nav-body
            (dom/remove-class! "fade-in")
            (dom/add-class! "fade-out"))
        (-> nav-bt
            (dom/remove-class! "rotate-right")
            (dom/add-class! "rotate-left"))
        (swap! state assoc :nav-active? false)
        (js/setTimeout
         #(when-not (:nav-active? @state)
            (dom/set-style! nav-body {:visibility "hidden"}))
         500)
        (recur)))))
