(ns parsero.combinators
  (:require [clojure.algo.monads :refer [domonad]])
  (:require [parsero.core :refer [parser-m m-plus-parser m-result-parser m-result-parser]]))


(defn any-char
  "Accepts a single character."
  [s]
  (if (empty? s)
    nil
    (list (first s) (.substring s 1))))

(defn char-satisfies
  [pred]
  (domonad parser-m
    [x any-char
     :when (pred x)]
    x))

(defn is-char
  [^Character c]
  (char-satisfies #(= c %)))

; TODO: make it return a number instead of char
(def digit (char-satisfies #(Character/isDigit %)))
(def lower (char-satisfies #(Character/isLowerCase %)))
(def upper (char-satisfies #(Character/isUpperCase %)))
(def letter (m-plus-parser lower upper))
(def alphanumeric (m-plus-parser letter digit))

(defn many
  "Given a parser, return another that
  tries to apply it to the input string
  as many times as possible."
  [p]
  (m-plus-parser
    (domonad parser-m
      [x p
       xs (many p)]
      (cons x xs))
    (m-result-parser [])))

(defn many1
  "Like `many` but it fails if the given
  parser doesn't succeed at least once."
  [p]
  (domonad parser-m
    [x p
     xs (many p)]
    (cons x xs)))

(def word (many1 letter))
; TODO: make it return a number instead of char
(def number
  (domonad parser-m
    [digits (many1 digit)]
    (reduce #(+ (* 10 %1) %2)
      (map #(- (int %) (int \0)) digits))))

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

(defn sep-by1
  [p sep-p]
  (domonad parser-m
    [x p
     xs (many (domonad parser-m
                [_ sep-p
                 y p]
                 y))]
    (cons x xs)))

(defn surrounded-by
  [prefix-p p suffix-p]
  (domonad parser-m
    [_ prefix-p
     x p
     _ suffix-p]
     x))
