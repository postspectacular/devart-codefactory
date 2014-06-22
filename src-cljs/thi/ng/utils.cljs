(ns thi.ng.cljs.utils)

(defn parse-int
  ([x] (parse-int x 10 nil))
  ([x radix nf]
     (let [x' (js/parseInt x radix)]
       (if (js/isNaN x') nf x'))))

(defn now
  [] (.getTime (js/Date.)))
