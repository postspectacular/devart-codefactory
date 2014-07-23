(ns thi.ng.cljs.utils)

(defn deep-merge-with
  "Like merge-with, but merges maps recursively, applying the given fn
  only when there's a non-map at a particular level.

      (deep-merge-with +
        {:a {:b {:c 1 :d {:x 1 :y 2}} :e 3} :f 4}
        {:a {:b {:c 2 :d {:z 9} :z 3} :e 100}})
      => {:a {:b {:z 3, :c 3, :d {:z 9, :x 1, :y 2}}, :e 103}, :f 4}"
  ^{:author "Chris Houser"}
  [f & maps]
  (apply
   (fn m [& maps]
     (if (every? map? maps)
       (apply merge-with m maps)
       (apply f maps)))
   maps))

(defn parse-int
  ([x] (parse-int x 10 nil))
  ([x radix nf]
     (let [x' (js/parseInt x radix)]
       (if (js/isNaN x') nf x'))))

(defn parse-float
  ([x] (parse-float x nil))
  ([x nf]
     (let [x' (js/parseFloat x)]
       (if (js/isNaN x') nf x'))))

(defn now
  [] (.getTime (js/Date.)))

(defn float-formatter
  [prec]
  (fn [x] (.toFixed (js/Number. x) prec)))

(defn ->px [x] (str x "px"))
