(ns parsero.core)

; Parser a = Parser (String -> [(a, String)])
;   Parser [s -> (a , s')]
;   s : input string
;   a : parsed value from the prefix of `s`
;   s': unparsed suffix of `s`
;
; A parser is a function that takes a string of characters as its argument, and
; returns a list of results.

; `nil` will be used to denote the failure of the parser and the non-empty list
; will represent success.

(def parse first)

(defn item
  "Accepts a single character."
  [s]
  (if (empty? s)
    nil
    [(first s) (rest s)]))

(parse (item "Foo")) ; \F
