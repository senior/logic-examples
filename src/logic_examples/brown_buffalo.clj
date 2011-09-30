(ns logic-examples.brown-buffalo
  (:refer-clojure :exclude [inc reify ==])
  (:require [logic-examples.arithmetic :as ar])
  (:use [clojure.core.logic minikanren prelude nonrel match disequality]))

;; Some CLP(FD) problem(s) in miniKanren from http://brownbuffalo.sourceforge.net/

(defne !=anyo [item items]
  ([_ [?x . ?xs]]
     (!= item ?x)
     (!=anyo item ?xs))
  ([_ ()]))

(defne differento [items]
  ([[?x . ?xs]]
     (!=anyo ?x ?xs)
     (differento ?xs))
  ([()]))

(defne ino [n r]
  ([_ [n . ?xs]])
  ([_ [_ . ?xs]] (ino n ?xs)))

(defne all-ino [ns r]
  ([() _])
  ([[?a . ?b] _]
     (ino ?a r)
     (all-ino ?b r)))


(defn problem-1 []
  (map #(map ar/spit-number %)
       (first (run* [q]
                    (fresh [shoe-ee shoe-ff shoe-pp shoe-ss
                            store-hh store-ff store-sp store-t x]
                           (all-ino [shoe-ee shoe-ff shoe-pp shoe-ss store-hh store-ff store-sp store-t]
                                    (map ar/build-num (range 1 5)))
                           (differento [shoe-ee shoe-ff shoe-pp shoe-ss])
                           (differento [store-hh store-ff store-sp store-t])
                           (== shoe-ff store-hh)
                           (ar/pluso shoe-pp (ar/build-num 1) x)
                           (!= x store-t)
                           (== (ar/build-num 2) store-ff)
                           (ar/pluso store-sp (ar/build-num 2) shoe-ss)
                           (== q [ [shoe-ee shoe-ff shoe-pp shoe-ss]
                                   [store-hh store-ff store-sp store-t]]))))))
;([[(0 1) (0 0 1) (1) (1 1)] [(0 0 1) (0 1) (1) (1 1)]])
