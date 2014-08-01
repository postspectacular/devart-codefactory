(ns codefactory.common
  (:require
   [thi.ng.cljs.dom :as dom]))

(defn show-nav
  [] (dom/remove-class! (dom/query nil "nav") "hidden"))

(defn icon-button
  [parent id [w h] paths label handler & classes]
  (let [el     (dom/create! "div" parent)
        svg    (dom/create-ns!
                dom/svg-ns "svg" el
                {:width w
                 :height h
                 :viewBox "-0.05 -0.05 1.1 1.1"
                 :preserveAspectRatio "none"})
        [spec] (if handler
                 (dom/add-listeners [[el "click" handler]])
                 [[el]])
        attrs  {:class (apply str "tool " classes)}]
    (dom/set-attribs! el (if id (assoc attrs :id (name id)) attrs))
    (when label
      (-> (dom/create! "div" el)
          (dom/set-html! label)))
    (loop [paths paths]
      (when-let [p (first paths)]
        (-> (dom/create-ns! dom/svg-ns "path" svg {:d (:d p)})
            (dom/set-style! (clj->js (:style p))))
        (recur (next paths))))
    spec))

(defn next-parent-id
  [el]
  (loop [el el]
    (if-let [id (first (dom/get-attribs el ["id"]))]
      id
      (if-let [el (dom/parent el)]
        (recur el)))))
