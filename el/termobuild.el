(setq tb-data nil)

(defun tb-check ()
  "Предназначена для формирования вставки блока кода"
  (interactive)
  (delete-blank-lines)
  (setq end-pos (point))
  (forward-line -1)
  (while (progn (setq tb-data (char-after))
		(or (= tb-data 32)
		    (= tb-data 45)))
    (forward-line -1))
  (setq cur-pos (point))
  (setq str-orig (buffer-substring cur-pos end-pos))
  (replace-string str-orig "" nil (point-min) (point-max))
  (beginning-of-buffer)
  (forward-line 2)
  (insert str-orig)
  (end-of-buffer)
  (recenter-top-bottom 35))


(defun my-replace-allbuffer (str-orig str-replace)
     (interactive "sString ? \nsReplace with ? ")
     (replace-string str-orig str-replace nil (point-min) (point-max)))

































 
