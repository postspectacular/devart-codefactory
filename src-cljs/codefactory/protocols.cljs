(ns codefactory.protocols)

(defprotocol PLifecycle
  (init [_ shared-state])
  (release [_]))

(defprotocol PState
  (get-state [_])
  (update-state [_ s]))
