(ns logic-examples.seq
  (:refer-clojure :exclude [inc reify ==])
  (:use [clojure.core.logic minikanren prelude nonrel match disequality]))

(defne prefixo [x y]
  ([[?a . ?xs] [?a . ?ys]] (prefixo ?xs ?ys))
  ([[] ?ys]))

(defne suffixo [x y]
  ([_ [?b . x]])
  ([_ [?b . ?ys]] (suffixo x ?ys))
  ([[] ?ys]))

(defne sublisto [x y]
  ([_ [?y . ?ys]]
     (sublisto x ?ys))
  ([_ [?y . ?ys]]
     (fresh [a]
            (== (prefixo x ?ys) ())
            (appendo x a ?ys)))
  ([?xs ?ys] (prefixo ?xs ?ys)))

