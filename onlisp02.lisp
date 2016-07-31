(defun double (x) (* x 2))

(double 1)  ; 2

#'double  ; #<FUNCTION DOUBLE (X) (DECLARE (SYSTEM::IN-DEFUN DOUBLE)) (BLOCK DOUBLE (* X 2))>

(eq #'double (car (list #'double)))  ; T



(lambda (x) (* x 2))

#'(lambda (x) (* x 2))

(double 3)  ; 6
((lambda (x) (* x 2)) 3)  ; 6

(setq double 2)
(double double)  ; 4

(symbol-value 'double)  ; 2
(symbol-function 'double)  ; #<FUNCTION DOUBLE (X) (DECLARE (SYSTEM::IN-DEFUN DOUBLE)) (BLOCK DOUBLE (* X 2))>



(setq x #'append)
(eq (symbol-value 'x) (symbol-function 'append))  ; T

(defun double (x) (* x 2))
(setf (symbol-function 'double) #'(lambda (x) (* x 2)))



(+ 1 2)
(apply #'+ '(1 2))
(apply (symbol-function '+) '(1 2))
(apply #'(lambda (x y) (+ x y)) '(1 2))
(apply #'+ 1 '(2))
(funcall #'+ 1 2)


(mapcar #'(lambda (x) (+ x 10)) '(1 2 3))
(mapcar #'+ '(1 2 3) '(10 100 1000))

(sort '(1 4 2 5 6 7 3) #'<)

(remove-if #'evenp '(1 2 3 4 5 6 7))

(defun our-remove-if (fn lst)
  (if (null lst)
      nil
      (if (funcall fn (car lst))
          (our-remove-if fn (cdr lst))
          (cons (car lst) (our-remove-if fn (cdr lst))))))



(defun behave (animal)
  (case animal
    (dog (wag-tail)
         (bark))
    (rat (scurry)
         (squeak))
    (cat (rub-legs)
         (scratch-carpet))))


(defun behave (animal)
  (funcall (get animal 'behavior)))

(setf (get 'dog 'behavior)
      #'(lambda ()
          (wag-tail)
          (bark)))



(let ((y 7))
  (defun scope-test (x)
    (list x y)))

(let ((y 5))
  (scope-test 3))


(defun list+ (lst n)
  (mapcar #'(lambda (x) (+ x n))
          lst))

(let ((counter 0))
  (defun new-id () (incf counter))
  (defun reset-id () (setq counter 0)))


(defun make-adder (n)
  #'(lambda (x) (+ x n)))

(setq add2 (make-adder 2)
      add10 (make-adder 10))
(funcall add2 5)
(funcall add10 3)


(defun make-adderb (n)
  #'(lambda (x &optional change)
      (if change
          (setq n x)
          (+ x n))))

(setq addx (make-adderb 1))
(funcall addx 3)

(funcall addx 100 t)



(setq cities (make-dbms '((boston . us) (paris . france))))

(defun make-dbms (db)
  (list
   #'(lambda (key)
       (cdr (assoc key db)))
   #'(lambda (key val)
       (push (cons key val) db)
       key)
   #'(lambda (key)
       (setf db (delete key db :key #'car))
       key)))

(defun lookup (key db)
  (funcall (car db) key))

(defun setrec (key val db)
  (funcall (second db) key val))




(mapcar #'(lambda (x) (+ 2 x))
        '(2 5 7 3))

(mapcar #'copy-tree '((a b) (c d e)))


(labels ((inc (x) (1+ x)))
  (inc 3))


(defun count-instances (obj lsts)
  (labels ((instances-in (lst)
             (if (consp lst)
                 (+ (if (eq (car lst) obj) 1 0)
                    (instances-in (cdr lst)))
                 0)))
    (mapcar #'instances-in lsts)))



(defun our-length (lst)
  (if (null lst)
      0
      (1+ (our-length (cdr lst)))))

(defun our-find-if (fn lst)
  (if (funcall fn (car lst))
      (car lst)
      (our-find-if fn (cdr lst))))


(defun our-length (lst)
  (labels ((rec (lst acc)
             (if (null lst)
                 acc
                 (rec (cdr lst) (1+ acc)))))
    (rec lst 0)))

;; tail recursion
(proclaim '(optimize speed)) ; at top of file


(defun triangle (n)
  (labels ((tri (c n)
             (declare (type fixnum n c))
             (if (zerop n)
                 c
                 (tri (the fixnum (+ n c))
                      (the fixnum (- n 1))))))
    (tri 0 n)))




;; compile
(defun foo (x) (1+ x))

(compiled-function-p #'foo)

(compile 'foo)

(compiled-function-p #'foo)


(compile nil '(lambda (x) (+ x 2)))
(progn (compile 'bar '(lambda (x) (* x 3)))
       (compiled-function-p #'bar))


(defun 50th (lst) (nth 49 lst))

(proclaim '(inline 50th))

(defun foo (lst)
  (+ (50th lst) 1))

(defun foo (lst)
  (+ (nth 49 lst) 1))
