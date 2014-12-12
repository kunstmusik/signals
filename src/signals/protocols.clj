(ns signals.protocols)

(defprotocol Reactor
  (deactivate [r] "Activate Reactor (removes watches on dependents)")
  (activate [r] "Activate Reactor (adds watches on dependents)")
  (activated? [r] "Returns if reactor is currently activated."))

(defprotocol KeyPathSignal
  (get-source-signal!*! [kps])
  (get-key-path!*! [kps])
  (reset!*! [kps v]))
