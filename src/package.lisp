;;;; package.lisp

(defpackage #:gases (:use #:cl)
            (:export molar-mass
                     molar-isobaric-heat-capacity 
                     molar-isochoric-heat-capacity 
                     molar-enthalpy 
                     molar-entropy 
                     adiabatic-index 
                     molar-fraction-summ 
                     mass-fraction-summ  
                     mix-composition 
                     check-mole-fraction 
                     check-mass-fraction 
                     reference 
                     elemental-mass-fraction 
                     dump 
                     adapt-mole-fractions 
                     adapt-mass-fractions 
                     insert 
                     combustion-reaction 
                     relativ-oxigen-mass-for-burning 
                     relativ-air-mass-for-burning  
                     wobber-hight 
                     wobber-low 
                     thermal-effect 
                     Q-work-low 
                     density 
                     density-relative)) 




;;;; (declaim (optimize (compilation-speed 0) (debug 3) (safety 0) (space 0) (speed 0)))
