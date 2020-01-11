;;;; package.lisp

(defpackage #:gases)

(defpackage #:gases
  (:use #:cl #:mnas-defclass)
  (:export
   J-wet-air
   p-wet-air-water-full
   p-wet-air-water-full-1 
   d-wet-air
   d-wet-air-by-temp
   )) 

;;;; (declaim (optimize (compilation-speed 0) (debug 3) (safety 0) (space 0) (speed 0)))
