(ns logic-examples.ancenstors
  (:refer-clojure :exclude [inc reify ==])
  (:use [clojure.core.logic minikanren prelude nonrel match disequality]))

(defrel father Father Son)
(defrel mother Mother Son)
(defrel brother Brother Sib)
(defrel male M)
(defrel female F)

(defn parent [p child]
  (conde
   ((father p child))
   ((mother p child))))

(defn brother [bro sib]
  (exist [p]
         (parent p bro)
         (parent p sib)
         (male bro)
         (!= bro sib)))

(defn uncle [u person]
  (exist [p]
         (brother u p)
         (parent p person)))

(facts father [['terach 'abraham]
               ['terach 'nachor]
               ['terach 'haran]
               ['abraham 'isaac]
               ['haran 'lot]
               ['haran 'milcah]
               ['haran 'yiscah]
               ['sarah 'isaac]])

(facts male (map list ['terach
                       'abraham
                       'nachor
                       'haran
                       'isaac
                       'lot
                       'sarah
                       'milcah
                       'yiscah]))
