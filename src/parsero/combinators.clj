(ns parsero.combinators
  (:require [parsero.core :refer [parser one-of gives any-char]]))

(defn char-satisfies
  [pred]
  (parser
    [x any-char
     :when (pred x)]
    x))

(defn is-char
  [^Character c]
  (char-satisfies #(= c %)))

(def digit (char-satisfies #(Character/isDigit %)))
(def lower (char-satisfies #(Character/isLowerCase %)))
(def upper (char-satisfies #(Character/isUpperCase %)))
(def letter (one-of lower upper))
(def alphanumeric (one-of letter digit))

(defn many
  "Given a parser, return another that
  tries to apply it to the input string
  as many times as possible."
  [p]
  (one-of
    (parser
      [x p
       xs (many p)]
      (cons x xs))
    (gives [])))

(defn many1
  "Like `many` but it fails if the given
  parser doesn't succeed at least once."
  [p]
  (parser
    [x p
     xs (many p)]
    (cons x xs)))

(def word (many1 letter))

(def unsigned-number
  (parser
    [digits (many1 digit)]
    (reduce #(+ (* 10 %1) %2)
      (map #(- (int %) (int \0)) digits))))

(def number
  (one-of
    (parser
      [_ (is-char \-)
       n unsigned-number]
       (- n))
    unsigned-number))

(defn string
  "Create a parser that parses the given string."
  [s]
  (if (empty? s)
    (gives "")
    (let [char-p (char-satisfies #(= (first s) %))]
      (parser
        [x char-p
         xs (string (.substring s 1))]
        (str x xs)))))

(defn drop-first
  [fp sp]
  (parser
    [_ fp
     x sp]
     x))

(defn sep-by
  [p sep-p]
  (one-of
    (parser
      [x p
       xs (many (drop-first sep-p p))]
      (cons x xs))
    (gives [])))

(defn sep-by1
  [p sep-p]
  (parser
    [x p
     xs (many1 (drop-first sep-p p))]
    (cons x xs)))

(defn surrounded-by
  [prefix-p p suffix-p]
  (parser
    [_ prefix-p
     x p
     _ suffix-p]
     x))

(defn skip-many
  [p]
  (one-of
    (parser
      [_ (many p)]
      nil)
    (gives nil)))

(defn skip-many1
  [p]
  (parser
    [_ (many1 p)]
    nil))

; TODO: chainl, chainr, spaces

(defn times
  [n p]
  (if (<= n 0)
    (gives [])
    (parser
      [x p
       xs (times (dec n) p)]
      (cons x xs))))
