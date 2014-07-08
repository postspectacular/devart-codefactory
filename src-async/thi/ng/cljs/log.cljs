(ns thi.ng.cljs.log)

(enable-console-print!)

(def log-level 1)

(defn log
  [prefix msg]
  (.log js/console (js/Date.) (apply str (interpose " " (cons prefix msg)))))

(defn debug
  [& msg] (if (>= log-level 1) (log "DEBUG:" msg)))

(defn info
  [& msg] (if (>= log-level 2) (log "INFO:" msg)))

(defn warn
  [& msg] (if (>= log-level 3) (log "WARN:" msg)))
