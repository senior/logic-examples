(ns logic-examples.seq
  (:refer-clojure :exclude [inc reify ==])
  (:use [clojure.core.logic minikanren prelude nonrel match disequality]
        [logic-examples.arithmetic]))

(defne prefixo [x y]
  ([[?a . ?xs] [?a . ?ys]] (prefixo ?xs ?ys))
  ([[] ?ys]))

(defn prefixo2 [x y]
  (fresh [a]
         (appendo x a y)))

(defne suffixo [x y]
  ([_ [?b . x]])
  ([_ [?b . ?ys]] (suffixo x ?ys))
  ([[] ?ys]))

(defn suffixo2 [x y]
  (fresh [a]
         (appendo a x y)))

(defn sublisto [x y]
  (fresh [a]
         (prefixo a y)
         (suffixo x a)))

(defn sublisto2 [x y]
  (fresh [a b c]
         (appendo a b y)
         (appendo x c b)))

(defn my-membero [x y]
  (fresh [a b]
         (appendo a (llist x b) y)))

(defn lasto [x y]
  (fresh [a]
         (appendo a [x] y)))

(defn adjacent [x y z]
  (fresh [a b]
         (appendo a (llist x y b) z)))

(defne reverseo [xs ys]
  ([[] []])
  ([[?x . ?xs] _]
     (fresh [a]
            (reverseo ?xs a)
            (appendo a [?x] ys))))

