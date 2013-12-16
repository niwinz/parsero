(ns parsero.combinators
  (:require [clojure.algo.monads :refer [domonad]])
  (:require [parsero.core :refer [parser-m m-plus-parser m-result-parser m-zero-parser]]))


(defn any-char
  "Accepts a single character."
  [s]
  (if (empty? s)
    (m-zero-parser s)
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

(def unsigned-number
  (domonad parser-m
    [digits (many1 digit)]
    (reduce #(+ (* 10 %1) %2)
      (map #(- (int %) (int \0)) digits))))

(def number
  (m-plus-parser
    (domonad parser-m
      [_ (is-char \-)
       n unsigned-number]
       (- n))
    unsigned-number))

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

(defn drop-first
  [fp sp]
  (domonad parser-m
    [_ fp
     x sp]
     x))

(defn sep-by
  [p sep-p]
  (m-plus-parser
    (domonad parser-m
      [x p
       xs (many (drop-first sep-p p))]
      (cons x xs))
    (m-result-parser [])))

(defn sep-by1
  [p sep-p]
  (domonad parser-m
    [x p
     xs (many1 (drop-first sep-p p))]
    (cons x xs)))

(defn surrounded-by
  [prefix-p p suffix-p]
  (domonad parser-m
    [_ prefix-p
     x p
     _ suffix-p]
     x))

; TODO: skip, skip1, spaces
