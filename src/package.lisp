;;;; package.lisp

(defpackage #:gases
  (:use #:cl)
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
           density-relative)
  (:export <sp>
           <sp>-name
           <sp>-reccords
           <sp>-phase
           <sp>-comments
           <sp>-heat-formation
           <sp-rec>-integration-constants
           <sp>-number-temperature-intervals
           <sp>-molar-mass
           <sp>-chemical-formula
           
           get-sp
           cp/r-new
           mole-fraction

           )
  (:export <sp-rec>
           <sp-rec>-polynomial-exponents
           <sp-rec>-temperature-range
           <sp-rec>-number-coeff
           <sp-rec>-h_298.15-h-0
           <sp-rec>-coefficients

           )
  (:export make-instance-component
           make-instance-composition
           )
  (:export 

   <product>
   <component>
   <composition>
   <reactant>
   <reaction>

   
   species
   composition-components

   culc-mass-fractions
   h/rt-old
   h/rt-new
   mass-fraction
   elements


             
   clear-db

   init-db
             
   *t-normal*
   *p-normal*
   s/r-new
             
   reaction-products
             
   moles-number

   get-db
   <sp>-reference-date-code


   culc-molar-fractions
             
   cp/r-old reaction-reactants

   *c-0*
   *kal*             
   *t-standard*
   *p-standard*
   *rμ*
   find-atoms q-of find-by-atoms

   )) 


;;;; (declaim (optimize (compilation-speed 0) (debug 3) (safety 0) (space 0) (speed 0)))
