;; how macro works
(defmacro nil! (var)
  (list 'setq var nil))

;; backquote
'(a b c)
`(a b c)
(list 'a 'b 'c)

`(a ,b c ,d)
(list 'a b 'c d)

(setq a 1 b 2 c 3)
`(a ,b c)
`(a (,b c))

`(a b ,c (',(+ a b c)) (+ a b) 'c '((,a ,b)))
;; (A B 3 ('6) (+ A B) 'C '((1 2)))

(defmacro nil! (var)
  (list 'setq var nil))

(defmacro nil! (var)
  `(setq ,var nil))


(defmacro nif (expr pos zero neg)
  `(case (truncate (signum ,expr))
     (1 ,pos)
     (0 ,zero)
     (-1 ,neg)))

(defmacro nif (expr pos zero neg)
  (list 'case
        (list 'truncate (list 'signum expr))
        (list 1 pos)
        (list 0 zero)
        (list -1 neg)))


(setq b '(1 2 3))
`(a ,b c)
;; (A (1 2 3) C)
`(a ,@b c)
;; (A 1 2 3 C)


(when (eligible obj)
  (do-this)
  (do-that)
  obj)

(defmacro our-when (test &body body)
  `(if ,test
       (progn
         ,@body)))

(if (eligible obj)
    (progn (do-this)
           (do-that)
           obj))


(defun greet (name)
  `(hello ,name))

;; define of simple macro
(member x choices :test #'eq)

(memq x choices)

(defmacro memq (obj list)
  `(member ,obj ,list :test #'eq))


(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

;; confirm macro expand
(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(pprint (macroexpand '(while (able) (laugh))))
(BLOCK NIL
 (LET NIL
  (TAGBODY #:LOOP-3259 (IF (NOT (ABLE)) (GO #:END-3260)) (LAUGH) (PSETQ) (GO #:LOOP-3259)
   #:END-3260 (RETURN-FROM NIL (PROGN)))))

(pprint (macroexpand-1 '(while (able) (laugh))))
(DO NIL ((NOT (ABLE))) (LAUGH))

(pprint (macroexpand-1 '(while (able) ((laugh1) (laugh2)))))
(DO NIL ((NOT (ABLE))) ((LAUGH1) (LAUGH2)))

(pprint (macroexpand-1 '(while (able) (laugh1 laugh2))))
(DO NIL ((NOT (ABLE))) (LAUGH1 LAUGH2))


(defmacro mac (expr)
  `(pprint (macroexpand-1 ',expr)))
(pprint (macroexpand-1 '(or x y)))
(mac (or x y))

;; for debug
(setq exp (macroexpand-1 '(memq 'a '(a b c))))
(eval exp)

;; destructuring
(defun foo (x y z)
  (+ x y z))

(foo 1 2 3)

(destructuring-bind (x (y) . z) '(a (b) c d)
    (list x y z))


(dolist (x '(a b c))
  (print x))

(defmacro our-dolist ((var list &optional result) &body body)
  `(progn
     (mapc #'(lambda (,var) ,@body)
           ,list)
     (let ((,var nil))
       ,result)))


(defmacro when-bind ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

(when-bind (input (get-user-input))
           (process input))


;; model of macro
(defmacro our-expander (name) `(get ,name 'expander))

(defmacro our-defmacro (name parms &body body)
  (let ((g (gensym)))
    `(progn
       (setf (our-expander ',name)
             #'(lambda (,g)
                 (block ,name
                   (destructuring-bind ,parms (cdr ,g)
                     ,@body))))
       ',name)))

(defun our-macroexpand-1 (expr)
  (if (and (consp expr) (our-expander (car expr)))
      (funcall (our-expander (car expr)) expr)
      expr))


;; macro as program
(do ((w 3)
     (x 1 (1+ x))
     (y 2 (1+ y))
     (z))
    ((> x 10) (princ z) y)
  (princ x)
  (princ y))

(prog ((w 3) (x 1) (y 2) (z nil))
 foo
 (if (> x 10)
     (return (progn (princ z) y)))
 (princ x)
 (princ y)
 (psetq x (1+ x) y (1+ y))
 (go foo))


(defmacro our-do (bindforms (test &rest result) &body body)
  (let ((label (gensym)))
    `(prog ,(make-initforms bindforms)
        ,label
        (if ,test
            (return (progn ,@result)))
        ,@body
        (psetq ,@(make-stepforms bindforms))
        (go ,label))))


(defun make-initforms (bindforms)
  (mapcar #'(lambda (b)
              (if (consp b)
                  (list (car b) (cadr b))
                  (list b nil)))
          bindforms))

(defun make-stepforms (bindforms)
  (mapcan #'(lambda (b)
              (if (and (consp b) (third b))
                  (list (car b) (third b))
                  nil))
          bindforms))



(let ((a 1))
  (setq a 2 b a)
  (list a b))
;; (2 2)
(let ((a 1))
  (psetq a 2 b a)
  (list a b))
;; (2 1)



;; style of macro
(defmacro our-and (&rest args)
  (case (length args)
    (0 t)
    (1 (car args))
    (t `(if ,(car args)
            (our-and ,@(cdr args))))))

(defmacro our-andb (&rest args)
  (if (null args)
      t
      (labels ((expander (rest)
                 (if (cdr rest)
                     `(if ,(car rest)
                          ,(expander (cdr rest)))
                     (car rest))))
        (expander args))))

;; depend on macro
(defmacro mac (x) `(1+ ,x))  ; MAC

(setq fn (compile nil `(lambda (y) (mac y))))  ; <COMPILED-FUNCTION NIL>

(defmacro mac (x) `(+ ,x 100))  ;MAC

(funcall fn 1)  ; 2

(setq fn (compile nil `(lambda (y) (mac y))))  ; MAC

(funcall fn 1)  ; 101

;; マクロはそれを呼び出す関数（またはマクロ）の前で定義する。
;; マクロが再定義されたときは、それを（直接または間接的に）呼び出している関数とマクロをすべて再コンパイルする。




;; from function to macro
(defun second (x) (cadr x))
(defmacro second (x) `(cadr ,x))

(defun noisy-second (x)
  (princ "Someone is talking a cadr!")
  (cadr x))
(defmacro noisy-second (x)
  `(progn
     (princ "Someone is talking a cadr!")
     (cadr ,x)))

(defun sum (&rest args)
  (apply #'+ args))
(defmacro sum (&rest args)
  `(apply #'+ (list ,@args)))
(defmacro sum (&rest args)
  `(+ ,@args))

(defun foo (x y z)
  (list x (let ((x y))
            (list x z))))
(defmacro foo (x y z)
  `(list ,x (let ((x ,y))
              (list x ,z))))

;; symbol-macro
(symbol-macrolet ((hi (progn (print "Howdy")
                             1)))
  (+ hi 2))
