(ns parsero.core
  (:require [clojure.algo.monads :refer [defmonad domonad defmonadfn m-chain]]))

; The parser monad

(defn- m-result-parser
  [v]
  (fn [s] (list v s)))

(defn- m-bind-parser
  [p f]
  (fn [s]
    (let [result (p s)]
      (when (not= nil result)
        (let [np (f (first result))]
          (np (second result)))))))

(defn- m-zero-parser
  [s]
  nil)

(defn- m-plus-parser
  [& ps]
  (fn [s]
    (first
      (drop-while nil?
        (map #(% s) ps)))))

(defmonad parser-m
  [m-result m-result-parser
   m-bind   m-bind-parser
   m-zero   m-zero-parser
   m-plus   m-plus-parser])

; 

(defn any-char
  "Accepts a single character."
  [s]
  (if (empty? s)
    nil
    (list (first s) (.substring s 1))))

(defn char-satisfies
  [p]
  (domonad parser-m
    [x any-char
     :when (p x)]
    x))

;(def is-c (char-satisfies #(= % \c)))
;(is-c "color")
