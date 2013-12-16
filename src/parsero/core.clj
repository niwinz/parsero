(ns parsero.core
  (:require [clojure.algo.monads :refer [defmonad]]))

(def parse-error? (partial = ::parse-error))

(defn m-result-parser
  [v]
  (fn [s] (list v s)))

(defn m-bind-parser
  [p f]
  (fn [s]
    (let [result (p s)]
      (if-not (parse-error? result)
        (let [np (f (first result))]
          (np (second result)))
        ::parse-error))))

(defn m-zero-parser
  [s]
  ::parse-error)

(defn m-plus-parser
  [& ps]
  (fn [s]
    (let [pvs (map #(% s) ps)
          v (first (drop-while parse-error? pvs))]
      (if (nil? v)
        ::parse-error
        v))))

(defmonad parser-m
  [m-result m-result-parser
   m-bind   m-bind-parser
   m-zero   m-zero-parser
   m-plus   m-plus-parser])
