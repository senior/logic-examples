(ns logic-examples.arithmetic
  (:refer-clojure :exclude [inc reify ==])
  (:use [clojure.core.logic minikanren prelude nonrel match disequality]))

(defn build-num [n]
  (cond
   (zero? n) ()
   (and (even? n) (not (zero? n)))  (cons 0 (build-num (/ n 2)))
   (odd? n) (cons 1 (build-num (/ (dec n) 2)))))

(defn remove* [pred & colls]
  (remove pred (apply map list colls)))

(def powers-o-two (iterate #(* 2 %) 1))

(defn spit-number [n]
  (reduce (fn [acc [num ptwo]]
            (+ acc (* num ptwo)))
          0 (remove* (comp zero? first) n powers-o-two)))


(defne poso [x]
  ([[_ . _]]))

(defne >1o [x]
  ([[_ _ . _]]))

;; First version of pluso from miniKanren paper
#_(defn pluso [n m s]
  (fresh [b res x y z]
   (matche [n m s]
           ([x () x])
           ([() (x . y) (x . y)])
           ([[0 . x] [0 . y] [0 . res]]
              (poso x)
              (poso y)
              (pluso x y res))
           ([[0 . x] [1 . y] [1 . res]]
              (poso x)
              (pluso x y res))
           ([[1 . x] [0 . y] [1 . res]]
              (poso y)
              (pluso x y res))
           ([[1 . x] [1 . y] [0 . res]]
              (fresh [res-1]
                     (pluso x y res-1)
                     (pluso [1] res-1 res))))))

(defne bit-xoro [x y r]
  ([1 1 0])
  ([0 1 1])
  ([1 0 1])
  ([0 0 0]))

(defne bit-ando [x y c]
  ([1 1 1])
  ([0 1 0])
  ([1 0 0])
  ([0 0 0]))

(defn half-addero [x y r c]
  (fresh []
         (bit-xoro x y r)
         (bit-ando x y c)))

(defn full-addero [b x y r c]
  (fresh [w xy wz]
         (half-addero x y w xy)
         (half-addero w b r wz)
         (bit-xoro xy wz c)))

(declare addero)

(defne gen-addero [d n m r]
  ([_ [?a . ?x] [?b . ?y] [?c . ?z]]
     (fresh [e]
            (poso ?y)
            (poso ?z)
            (full-addero d ?a ?b ?c e)
            (addero e ?x ?y ?z))))

(defne addero [d n m r]
  ([0 _ () _] (== n r))
  ([0 () _ _] (== m r) (poso m))
  ([1 _ () _] (addero 0 n [1] r))
  ([1 () _ _] (poso m) (addero 0 [1] m r))
  ([_ [1] [1] _]
     (fresh [a c]
            (== [a c] r)
            (full-addero d 1 1 a c)))
  ([_ [1] _ _]
     (gen-addero d n m r))
  ([_ _ [1] _]
     (>1o n)
     (>1o r)
     (addero d [1] n r))
  ([_ _ _ _]
     (>1o n)
     (gen-addero d n m r)))

(defn pluso [n m k]
  (addero 0 n m k))

(defn minuso [n m k]
  (pluso m k n))

(defne =lo [n m]
  ([() ()])
  ([[1] [1]])
  ([[?x . ?xs] [?x . ?ys]]
     (poso ?xs)
     (poso ?ys)
     (=lo ?xs ?ys)))

(defne <lo [n m]
  ([[] _] (poso m))
  ([[1] _] (>1o m))
  ([[0 . ?xs] [1 . ?ys]]
     (poso ?xs)
     (poso ?ys)
     (=lo ?xs ?ys))
  ([[_ . ?xs] [_ . ?ys]]
     (poso ?xs)
     (poso ?ys)
     (<lo ?xs ?ys)))

(defne >lo [n m]
  ([_ []] (poso n))
  ([_ [1]] (>1o n))
  ([[_ . ?xs] [_ . ?ys]]
     (poso ?xs)
     (poso ?ys)
     (>lo ?xs ?ys)))

(defn <=lo [n m]
  (conde
   ((=lo n m))
   ((<lo n m))))

