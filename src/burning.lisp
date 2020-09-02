;;;;burning.lisp

(in-package :gases)

(defparameter *O2*
  (make-instance-composition
   `(("O2" ,(/ 100. 100)))))

(defparameter *Air*
  (make-instance-composition
   `(("N2" ,(/ 78.084 100))
     ("O2" ,(/ 20.9476 100))
     ("Ar" ,(/ .9365 100))
     ("CO2",(/ .0319 100)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; relativ-air-mass-for-burning

(export 'relativ-air-mass-for-burning )
(defmethod relativ-air-mass-for-burning ((sp <sp>))
  (/ (relativ-oxigen-mass-for-burning sp)
     (mass-fraction (reference "O2" *air*))))

(export 'relativ-air-mass-for-burning )
(defmethod relativ-air-mass-for-burning ((cmp <component>))
  (/ (relativ-oxigen-mass-for-burning cmp)
     (mass-fraction (reference "O2" *air*))))

(export 'relativ-air-mass-for-burning )
(defmethod relativ-air-mass-for-burning ((cmp <composition>))
  (/ (relativ-oxigen-mass-for-burning cmp)
     (mass-fraction (reference "O2" *air*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Число Воббе низшее

(defmethod wobber-low ((sp <sp>))
  (let ((ρ-f (density sp *p-normal* *t-normal*))
	(ρ-a (density *air* *p-normal* *t-normal*)))
    (/ (* (Q-work-low sp) ρ-f) (sqrt (/ ρ-f ρ-a)))))

(defmethod wobber-low ((c-t <component>))
  (error "Not defined")
  )

(defmethod wobber-low ((c-n <composition>))
  (let ((ρ-f (density c-n *p-normal* *t-normal*))
	(ρ-a (density *air* *p-normal* *t-normal*)))
    (/ (* (Q-work-low c-n) ρ-f) (sqrt (/ ρ-f ρ-a)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Число Воббе высшее

;;;; Число Воббе высшее
(defmethod wobber-hight ((sp <sp>))
    (error "Not defined yet")  
)

(defmethod wobber-hight ((c-t <component>))
  (error "Not defined")
  )

(defmethod wobber-hight ((c-n <composition>))
  (error "Not defined")
  )
