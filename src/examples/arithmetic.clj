(ns examples.arithmetic
  (:require [clojure.algo.monads :refer [defmonad domonad]])
  (:require [parsero.core :refer [parser one-of parse]])
  (:require [parsero.combinators :refer [is-char number chainl1 space skip-many]]))

; Production rules
(declare expr
         term
         factor
         add-op
         mul-op)

(defn trim
  [p]
  (parser
    [_ (skip-many space)
     r p
     _ (skip-many space)]
     r))

; Parser (Int -> Int -> Int)
(def add-op
  (one-of
    (trim (parser [_ (is-char \+)] +))
    (trim (parser [_ (is-char \-)] -))))

; Parser (Int -> Int -> Int)
(def mul-op
  (one-of
    (trim (parser [_ (is-char \*)] *))
    (trim (parser [_ (is-char \/)] /))))

; Parser Int
(def factor
  (one-of
    number
    (parser
      [_ (is-char \()
       r expr
       _ (is-char \))]
      r)))

; Parser Int
(def term
  (chainl1 factor mul-op))

; Parser Int
(def expr
  (chainl1 term add-op))

(def parse-expr (partial parse expr))
