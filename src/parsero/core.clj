(ns parsero.core
  (:require [clojure.algo.monads :refer [defmonad domonad]]))

; TODO: error reporting

(defrecord PosString [string line col])

(defn- is-newline?
  [c]
  (= c \newline))

(defn- line-after-consuming
  [line s]
  (let [consumed-newlines (count (filter is-newline? s))]
    (+ line consumed-newlines)))

(defn- col-after-consuming
  [col s]
  (let [consumed-newlines (count (filter is-newline? s))
        last-chars (last (partition-by is-newline? s))]
    (if (zero? consumed-newlines)
      (+ col (count s))
      (- (count last-chars) 1))))

(defn- update-pstring
  [pstr new-s consumed-s]
  (let [old-line (:line pstr)
        old-col (:col pstr)]
    (PosString.
      new-s
      (line-after-consuming old-line consumed-s)
      (col-after-consuming old-col consumed-s))))

(def parse-error ::parse-error)
(def parse-error? (partial = parse-error))

(defn- m-result-parser
  [v]
  (fn [pst] (list v pst)))

(defn- m-bind-parser
  [p f]
  (fn [pstring-s]
    (let [result (p pstring-s)]
      (if-not (parse-error? result)
        (let [res (first result)
              nparser (f res)
              npstring (second result)]
          (nparser npstring))
        parse-error))))

(defn- m-zero-parser
  [s]
  parse-error)

(defn- m-plus-parser
  [& ps]
  (fn [s]
    (let [pvs (map #(% s) ps)
          v (first (drop-while parse-error? pvs))]
      (if (nil? v)
        parse-error
        v))))

(defmonad parser-m
  [m-result m-result-parser
   m-bind   m-bind-parser
   m-zero   m-zero-parser
   m-plus   m-plus-parser])

(defn parse
  [p s]
  (let [r (p (PosString. s 0 0))]
    (if-not (parse-error? r)
      (first r)
      r)))

(defn raw-parse
  [p s]
  (p (PosString. s 0 0)))

(defn parsed-value-and-remainder
  [p s]
  (let [r (raw-parse p s)]
    (if (parse-error? r)
      r
      (list (first r) (:string (second r))))))

(defn any-char
  "Accepts a single character."
  [pst]
  (if (empty? (:string pst))
    (m-zero-parser (:string pst))
    (let [s (:string pst)
          r (first s)
          new-s (.substring s 1)
          consumed-s (.substring s 0 1)]
      (list r
            (update-pstring
              pst
              new-s
              consumed-s)))))

(def one-of m-plus-parser)

(def gives m-result-parser)

(defmacro parser [bindings res]
  `(domonad parser-m
    ~bindings
    ~res))
