(defmacro fn (expr) '#',(rbuild expor))
(defun rbuild (expr)
  (if (or (atom expr) (eq (car expr) 'lambda))
      expr
    (if (eq (car expr) 'compose)
	(build-compose (cdr expr))
      (build-call (car expr) (cdr expr)))))

(defun build-call (op fns)
  (let ((g (gensym)))
    `(lambda (,g) (,op ,@(mapcar #'(lambda (f) `(,(rbuild f) ,g)) fns)))))

(defun build-compose (fns)
  (let ((g (gensym)))
    `(lambda (,g) ,(labels ((rec (fns) (if fns `(,(rbuild (car fns)) ,(rec (cdr fns))) g))) (rec fns)))))

(macroexpand (fn (and integerp oddp)))

(fn (and integerp oddp))

(values
  (and t t)
  (and t nil)
  (and nil t)
  (and nil nil))

(-flatten `(1 2 (3 4)))
(-map (lambda (x) (+ x 1)) `(1 2 3 4))
(-map (lambda (x) (+ x 1)) `(1 2 3 4 5 6 7 8 9))

(mapcar '1+ `(1 2 3))


(setq animals '(gazelle giraffe lion tiger))

(defun print-elements-of-list (list)
  "Print each element of LIST on a line of its own."
  (while list
    (print (car list))
    (setq list (cdr list))))

(setq animals '(gazelle giraffe lion tiger))

(defun print-elements-of-list (list)
  "Print each element of LIST on a line of its own."
  (while list
    (print (car list))
    (setq list (cdr list))))

(print-elements-of-list animals)
