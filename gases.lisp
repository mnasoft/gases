;;;; gases.lisp

(in-package #:gases)

;;; "gases" goes here. Hacks and glory await!

(defun μ (x)
  "Возвращает мольную массу компонента кг/моль"
  (fourth x))

(defun μCp (x temp)
  "Возвращает мольную изобарную теплоемкость в ккал/(моль*К)"
  (let ((a (first x))
	(b (second x))
	(c (third x)))
    (+ a (* b temp 0.001) (* c temp temp 0.001 0.001))))

(defun μCv (x temp)
    "Возвращает мольную изохорную теплоемкость в ккал/(моль*К)"
  (- (μCp x temp) 2))

(defun Cp (x temp)
  "Возвращает массовую изобарную теплоемкость в ккал/(кг*К)"
  (let ((μ (μ x)))
    (/ (μCp x temp) μ 1000.0)))

(defun Cv (x temp)
    "Возвращает массовую изохорную теплоемкость в ккал/(моль*К)"
  (let ((μ (μ x)))
    (/ (μCv x temp) μ 1000.0)))

(defun k (x temp)
  (/  (Cp x temp) (Cv x temp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun μ-mixture (m-stuff)
  "Возвращает мольную массу компонента кг/моль"
  (let ((summ-ri-μi 0))
    (mapcar
     #'(lambda (el)
	 (let
	     ((x-i (first el))
	      (r-i (second el)))
	   (setf summ-ri-μi (+ summ-ri-μi (* r-i (μ x-i))))))
     m-stuff)
     summ-ri-μi))

(defun Cp-mixture (m-stuff temp)
  "Возвращает массовую изобарную теплоемкость [ккал/(кг*К)] для смеси газов m-stuff при температере temp[К].
Пример использования:
(Cp-mixture *running-gas* 373) => 0.5388392"
  (let ((summ-ri-μi-cpi 0)
	(summ-ri-μi 0))
    (mapcar
     #'(lambda (el)
	 (let
	     ((x-i (first el))
	      (r-i (second el)))
	   (setf summ-ri-μi-cpi (+ summ-ri-μi-cpi (* r-i (μ x-i) (Cp x-i temp)))
		 summ-ri-μi (+ summ-ri-μi (* r-i (μ x-i))))))
     m-stuff)
    (/ summ-ri-μi-cpi summ-ri-μi)))

(defun Cv-mixture (m-stuff temp)
    "Возвращает массовую изохорную теплоемкость [ккал/(кг*К)] для смеси газов m-stuff при температере temp[К].
Пример использования:
(Cv-mixture *running-gas* 373) => 0.45559332"
  (let ((summ-ri-μi-cpi 0)
	(summ-ri-μi 0))
    (mapcar
     #'(lambda (el)
	 (let
	     ((x-i (first el))
	      (r-i (second el)))
	   (setf summ-ri-μi-cpi (+ summ-ri-μi-cpi (* r-i (μ x-i) (Cv x-i temp)))
		 summ-ri-μi (+ summ-ri-μi (* r-i (μ x-i))))))
     m-stuff)
    (/ summ-ri-μi-cpi summ-ri-μi)))

(defun k-mixture (m-stuff temp)
  "Возвращает коэффициент адиабаты для смеси газов m-stuff при температере temp[К]."
  (/ (Cp-mixture m-stuff temp)
     (Cv-mixture m-stuff temp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod molar-mass ((x molecule))
  "Возвращает молекулярную массу, [g/mol]
Пример использования
(molar-mass (gethash \"N2\" *sp-db*)) => 28.0134
(molar-mass (gethash \"CH4\" *sp-db*)) => 16.04246
"
  (molecule-mass x))

(molar-mass *air*)
