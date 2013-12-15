(ns parsero.core
  (:require [clojure.algo.monads :refer [defmonad domonad m-plus]])
  (:import [java.lang Character]))

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

(def digit (char-satisfies #(Character/isDigit %)))
(def lower (char-satisfies #(Character/isLowerCase %)))
(def upper (char-satisfies #(Character/isUpperCase %)))

(def letter (m-plus-parser lower upper))
(def alphanumeric (m-plus-parser letter digit))

(def word
  (m-plus-parser (domonad parser-m
                    [x letter
                     xs word]
                    (str x xs))
                 (m-result-parser "")))

(defn string
  "Create a parser that parses the given string."
  [s]
  (if (empty? s)
    (m-result-parser "")
    (let [char-p (char-satisfies #(= (first s) %))]
      (domonad parser-m
        [x char-p
         xs (string (.substring s 1))]
        (str x xs)))))
