(ns thi.ng.gae.services.core)

(defmacro defservice
  [name var type ctor]
  `(do
     (defonce ~(vary-meta var assoc :dynamic true) (atom nil))
     (defn ~(vary-meta name assoc :tag type) []
       (when (nil? (deref ~var)) (reset! ~var ~ctor))
       (deref ~var))))
