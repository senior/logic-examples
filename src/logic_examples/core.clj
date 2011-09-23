(ns logic-examples.core
  (:refer-clojure :exclude [inc reify ==])
  (:use [clojure.core.logic minikanren prelude nonrel match disequality]))


(defn build-num [n]
  (cond
   (zero? n) ()
   (and (even? n) (not (zero? n)))  (cons 0 (build-num (/ n 2)))
   (odd? n) (cons 1 (build-num (/ (dec n) 2)))))


(defn poso [x]
  (matche [x]
          ([[_ . _]] s#)))

(defn >1o [x]
  (matche [x]
          ([[_ _ . _]] s#)))

;; First version of pluso from miniKanren paper
#_(defn pluso [n m s]
  (exist [b res x y z]
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
              (exist [res-1]
                     (pluso x y res-1)
                     (pluso [1] res-1 res))))))

(defn bit-xoro [x y r]
  (matche [x y r]
          ([1 1 0])
          ([0 1 1])
          ([1 0 1])
          ([0 0 0])))

(defn bit-ando [x y c]
  (matche [x y c]
          ([1 1 1])
          ([0 1 0])
          ([1 0 0])
          ([0 0 0])))

(defn half-addero [x y r c]
  (exist []
         (bit-xoro x y r)
         (bit-ando x y c)))

(defn full-addero [b x y r c]
  (exist [w xy wz]
         (half-addero x y w xy)
         (half-addero w b r wz)
         (bit-xoro xy wz c)))

(declare addero)

(defn gen-addero [d n m r]
  (exist [a b c x y z]
         (matche [n m r]
                 ([[a . x] [b . y] [c . z]]
                    (exist [e]
                           (poso y)
                           (poso z)
                           (full-addero d a b c e)
                           (addero e x y z))))))

(defn addero [d n m r]
  (matche [d n m]
          ([0 _ ()] (== n r))
          ([0 () _] (== m r) (poso m))
          ([1 _ ()] (addero 0 n [1] r))
          ([1 () _] (poso m) (addero 0 [1] m r))
          ([_ [1] [1]]
             (exist [a c]
                    (== [a c] r)
                    (full-addero d 1 1 a c)))
          ([_ [1] _]
             (gen-addero d n m r))
          ([_ _ [1]]
             (>1o n)
             (>1o r)
             (addero d [1] n r))
          ([_ _ _]
             (>1o n)
             (gen-addero d n m r))))

(defn pluso [n m k]
  (addero 0 n m k))

(defn minuso [n m k]
  (pluso m k n))

(defn =lo [n m]
  (exist [x xs y ys]
         (matche [n m]
                 ([() ()])
                 ([[1] [1]])
                 ([[x . xs] [y . ys]]
                    (poso xs)
                    (poso ys)
                    (=lo xs ys)))))

(defn <lo [n m]
  (exist [x xs y ys]
         (matche [n m]
                 ([[] _] (poso m))
                 ([[1] _] (>1o m))
                 ([[x . xs] [y . ys]]
                    (poso xs)
                    (poso ys)
                    (<lo xs ys)))))

(defn <=lo [n m]
  (conde
   ((=lo n m))
   ((<lo n m))))

(defn rangeo [a max]
  (conde
   ((>1o a))
   ((<=lo a max))))

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

#_(run* [q]
      (exist [shoe-ee shoe-ff shoe-pp shoe-ss
              store-hh store-ff store-sp store-t possible-values x]
             (== possible-values [(build-num 1) (build-num 2) (build-num 3) (build-num 4)])
             (all-ino [shoe-ee shoe-ff shoe-pp shoe-ss] possible-values)
             (all-ino [store-hh store-ff store-sp store-t] possible-values)
             (differento [shoe-ee shoe-ff shoe-pp shoe-ss])
             (differento [store-hh store-ff store-sp store-t])
             (== shoe-ff store-hh)
             (pluso shoe-pp (build-num 1) x)
             (!= x store-t)
             (== (build-num 2) store-ff)
             (pluso store-sp (build-num 2) shoe-ss)
             (== q [ [shoe-ee shoe-ff shoe-pp shoe-ss]
                     [store-hh store-ff store-sp store-t]])))
;([[(0 1) (0 0 1) (1) (1 1)] [(0 0 1) (0 1) (1) (1 1)]])





