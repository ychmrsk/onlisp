(defun bad-reverse (lst)
  (let* ((len (length lst))
         (ilimit (truncate (/ len 2))))
    (do ((i 0 (1+ i))
         (j (1- len) (1- j)))
        ((>= i ilimit))
      (rotatef (nth i lst) (nth j lst)))))


(defun good-reverse (lst)
  (labels ((rev (lst acc)
             (if (null lst)
                 acc
                 (rev (cdr lst) (cons (car lst) acc)))))
    (rev lst nil)))


;; multiple-value
(truncate 26.21875)
(= (truncate 26.21875) 26)

(multiple-value-bind (int frac) (truncate 26.21875)
  (list int frac))

(defun powers (x)
  (values x (sqrt x) (expt x 2)))
(multiple-value-bind (base root square) (powers 4)
  (list base root square))



(defun fun (x)
  (list 'a (expt (car x) 2)))

(defun imp (x)
  (let (y sqr)
    (setq y (car x))
    (setq sqr (expt y 2))
    (list 'a sqr)))

(list 'a sqr)
(list 'a (expt y 2))
(list 'a (expt (car x) 2))



;; functional interface
(defun qualify (expr)
  (nconc (copy-list expr) (list 'maybe)))

(defun ok (x)
  (nconc (list 'a x) (list 'c)))

(defun not-ok (x)
  (nconc (list 'a) x (list 'c)))


(defun exclaim (expression)
  (append expression '(oh my)))

(defun exclaim (expression)
  (append expression (list oh my)))

