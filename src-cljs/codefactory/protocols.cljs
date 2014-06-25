(ns codefactory.protocols)

(defprotocol PController
  (init [_ opts])
  (release [_]))

(defprotocol PState
  (get-state [_])
  (update-state [_ s]))
