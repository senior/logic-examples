(ns logic-examples.brown-buffalo
  (:refer-clojure :exclude [inc reify ==])
  (:require [logic-examples.arithmetic :as ar])
  (:use [clojure.core.logic minikanren prelude nonrel match disequality]))

;; Some CLP(FD) problem(s) in miniKanren from http://brownbuffalo.sourceforge.net/

(defn !=anyo [item items]
  (exist [x xs]
         (matche [items]
                 ([[x . xs]]
                    (!= item x)
                    (!=anyo item xs))
                 ([()] s#))))

(defn differento [items]
  (exist [x xs]
         (matche [items]
                 ([[x . xs]]
                    (!=anyo x xs)
                    (differento xs))
                 ([()] s#))))

(defn ino [n r]
  (exist [x xs]
         (matche [r]
                 ([[n . xs]])
                 ([[_ . xs]] (ino n xs)))))

(defn all-ino [ns r]
  (exist [a b]
   (matche [ns]
           ([()])
           ([[a . b]]
              (ino a r)
              (all-ino b r)))))

(defn problem-1 []
  (run* [q]
        (exist [shoe-ee shoe-ff shoe-pp shoe-ss
                store-hh store-ff store-sp store-t possible-values x]
               (== possible-values (map ar/build-num (range 1 5)))
               (all-ino [shoe-ee shoe-ff shoe-pp shoe-ss
                               store-hh store-ff store-sp store-t] possible-values)
               (differento [shoe-ee shoe-ff shoe-pp shoe-ss])
               (differento [store-hh store-ff store-sp store-t])
               (== shoe-ff store-hh)
               (ar/pluso shoe-pp (ar/build-num 1) x)
               (!= x store-t)
               (== (ar/build-num 2) store-ff)
               (ar/pluso store-sp (ar/build-num 2) shoe-ss)
               (== q [ [shoe-ee shoe-ff shoe-pp shoe-ss]
                       [store-hh store-ff store-sp store-t]]))))
;([[(0 1) (0 0 1) (1) (1 1)] [(0 0 1) (0 1) (1) (1 1)]])
