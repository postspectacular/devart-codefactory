(ns codefactory.protocols)

(defprotocol PLifeCycle
  (init [_ shared-state])
  (release [_]))
