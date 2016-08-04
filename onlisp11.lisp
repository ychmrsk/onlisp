;; ============================================================
;; 11. 古典的なマクロ
;; ============================================================
;; 1. コンテキストの生成

(let ((x 'b))
  (list x))

(defmacro our-let (binds &body body)
  `((lambda ,(mapcar #'(lambda (x)
                         (if (consp x) (car x) x))
                     binds)
      ,@body)
    ,@(mapcar #'(lambda (x)
                  (if (consp x) (cadr x) nil))
              binds)))

(our-let ((x 1) (y 2))
         (+ x y))

((LAMBDA (X Y) (+ X Y)) 1 2)



(defmacro when-bind ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var
       ,@body)))



(defmacro when-bind* (binds &body body)
  (if (null binds)
      `(progn ,@body)
      `(let (,(car binds))
         (if ,(caar binds)
             (when-bind* ,(cdr binds) ,@body)))))

(when-bind* ((x (find-if #'consp '(a (1 2) b)))
             (y (find-if #'oddp x)))
            (+ y 10))



(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))

(defmacro with-redraw ((var objs) &body body)
  (let ((gob (gensym))
        (x0 (gensym)) (y0 (gensym))
        (x1 (gensym)) (y1 (gensym)))
    ...))

(defmacro with-redraw ((var objs) &body body)
  (with-gensyms (gob x0 y0 x1 y1)
    ...))


;; ------------------------------------------------------------
;; condlet

;; from
(condlet (((= 1 2) (x (princ 'a)) (y (princ 'b)))
          ((= 1 1) (y (princ 'c)) (x (princ 'd)))
          (t       (x (princ 'e)) (z (princ 'f))))
         (list x y z))
;; to
(cond ((= 1 2)
       (let ((x (princ 'a))
             (y (princ 'b)))
         (list x y z)))
      ((= 1 1)
       (let ((y (princ 'c))
             (x (princ 'd)))
         (list x y z)))
      (t
       (let ((x (princ 'e))
             (z (princ 'f)))
         (list x y z))))

;; without gensym
(defmacro condlet (clauses &body body)
  (let ((bodfn 'name-of-body-func)
        (vars (remove-duplicates
               (mapcar #'car
                       (mappend #'cdr
                                clauses)))))
    `(labels ((,bodfn ,vars
                ,@body))
       (cond ,@(mapcar #'(lambda (cl)
                           (condlet-clause vars cl bodfn))
                       clauses)))))

(defun condlet-clause (vars cl bodfn)
  `(,(car cl)
     (let ,vars
       (let ,(condlet-binds vars cl)
         (,bodfn ,@vars)))))

(defun condlet-binds (vars cl)
  (mapcar #'(lambda (bindform)
              (if (consp bindform)
                  (cons (car bindform)
                        (cdr bindform))))
          (cdr cl)))

;; definition with gensym
(defmacro condlet (clauses &body body)
  (let ((bodfn (gensym))
        (vars (mapcar #'(lambda (v) (cons v (gensym)))
                      (remove-duplicates
                       (mapcar #'car
                               (mappend #'cdr clauses))))))
    `(labels ((,bodfn ,(mapcar #'car vars)
                ,@body))
       (cond ,@(mapcar #'(lambda (cl)
                           (condlet-clause vars cl bodfn))
                       clauses)))))

(defun condlet-clause (vars cl bodfn)
  `(,(car cl) (let ,(mapcar #'cdr vars)
                (let ,(condlet-binds vars cl)
                  (,bodfn ,@(mapcar #'cdr vars))))))

(defun condlet-binds (vars cl)
  (mapcar #'(lambda (bindform)
              (if (consp bindform)
                  (cons (cdr (assoc (car bindform) vars))
                        (cdr bindform))))
          (cdr cl)))

(defun mappend (fn &rest lsts)
  (apply #'append (apply #'mapcar fn lsts)))

(condlet (((= 1 2) (x (princ 'a)) (y (princ 'b)))
          ((= 1 1) (y (princ 'c)) (x (princ 'd)))
          (t (x (princ 'e)) (z (princ 'f))))
         (list x y z))

(LABELS ((#:G7022849 (Y X Z) (LIST X Y Z)))
 (COND
  ((= 1 2)
   (LET (#:G7022850 #:G7022851 #:G7022852)
    (LET ((#:G7022851 (PRINC 'A)) (#:G7022850 (PRINC 'B)))
     (#:G7022849 #:G7022850 #:G7022851 #:G7022852))))
  ((= 1 1)
   (LET (#:G7022850 #:G7022851 #:G7022852)
    (LET ((#:G7022850 (PRINC 'C)) (#:G7022851 (PRINC 'D)))
     (#:G7022849 #:G7022850 #:G7022851 #:G7022852))))
  (T
   (LET (#:G7022850 #:G7022851 #:G7022852)
    (LET ((#:G7022851 (PRINC 'E)) (#:G7022852 (PRINC 'F)))
     (#:G7022849 #:G7022850 #:G7022851 #:G7022852)))))) 

;;------------------------------------------------------------
;; 2. with-系マクロ

(with-open-file (s "dump" :direction :output)
  (princ 99 s))

(setq x 'a)
(unwind-protect
     (progn (princ "What error?")
            (error "This error."))
  (setq x 'b))


(let ((temp *db*))
  (setq *db* db)
  (lock *db*)
  (prog1 (eval-query q)
    (release *db*)
    (setq *db* temp)))

(with-db db
  (eval-query q))

;; pure macro
(defmacro with-db (db &body body)
  (let ((temp (gensym)))
    `(let ((,temp *db*))
       (unwind-protect
            (progn (setq *db* ,db)
                   (lock *db*)
                   ,@body)
         (progn (release *db*)
                (setq *db* ,temp))))))

;; combination of macro and function
(defmacro with-db (db &body body)
  (let ((gbod (gensym)))
    `(let ((,gob #'(lambda () ,@body)))
       (declare (dynamic-extent ,gbod))
       (with-db-fn *db* ,db ,gbod))))

(defun with-db-fn (old-db new-db body)
  (unwind-protect
       (progn (setq *db* new-db)
              (lock *db*)
              (funcall body))
    (progn (release *db*)
           (setq *db* old-db))))


;; ------------------------------------------------------------
;; 3. 条件付き評価

(if t
    'phew
    (/ x 0))

;; three-value if
(defmacro if3 (test t-case nil-case ?-case)
  `(case ,test
     ((nil) ,nil-case)
     (?     ,?-case)
     (t     ,t-case)))

;; numeric if
(defmacro nif (expr pos zero neg)
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (cond ((plusp ,g) ,pos)
             ((zerop ,g) ,zero)
             (t ,neg)))))

;; in
(defmacro in (obj &rest choices)
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,@(mapcar #'(lambda (c) `(eql ,insym ,c))
                     choices)))))

(let ((x (foo)))
  (or (eql x (bar)) (eql x (baz))))

(member (foo) (list (bar) (baz)))

(macroexpand-1
 '(in (foo) (bar) (baz)))
(LET ((#:G3230 (FOO)))
  (OR (EQL #:G3230 (BAR)) (EQL #:G3230 (BAZ))))


;; inq
(defmacro inq (obj &rest args)
  `(in ,obj ,@(mapcar #'(lambda (a) `',a)
                      args)))

(macroexpand-1
 '(inq operator + - *))
(IN OPERATOR '+ '- '*)


;; in-if
(defmacro in-if (fn &rest choices)
  (let ((fnsym (gensym)))
    `(let ((,fnsym ,fn))
       (or ,@(mapcar #'(lambda (c) `(funcall ,fnsym ,c))
                     choices)))))

(member x (list a b) :test #'equal)
(in-if #'(lambda (y) (equal x y)) a b)

(macroexpand-1
 '(in-if #'(lambda (y) (equal x y)) a b))
(LET ((#:G3231 #'(LAMBDA (Y) (EQUAL X Y))))
  (OR (FUNCALL #:G3231 A) (FUNCALL #:G3231 B)))

(some #'oddp (list a b))
(in-if #'oddp a b)
(macroexpand-1
 '(in-if #'oddp a b))
(LET ((#:G3232 #'ODDP))
  (OR (FUNCALL #:G3232 A) (FUNCALL #:G3232 B)))


;; >case
(defmacro >case (expr &rest clauses)
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (cond ,@(mapcar #'(lambda (cl) (>casex g cl))
                       clauses)))))

(defun >casex (g cl)
  (let ((key (car cl))
        (rest (cdr cl)))
    (cond ((consp key) `((in ,g ,@key) ,@rest))
          ((inq key t otherwise) `(t ,@rest))
          (t (error "bad >case clause")))))


;; ------------------------------------------------------------
;; 4. 反復

;; do
(defmacro forever (&body body)
  `(do ()
       (nil)
     ,@body))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defmacro till (test &body body)
  `(do ()
       (,test)
     ,@body))

(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))


;; dolist
(defun map0-n (fn n)
  (mapa-b fn 0 n))

(defun map1-n (fn n)
  (mapa-b fn 1 n))

(defun mapa-b (fn a b &optional (step 1))
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))

(defun map-> (fn start test-fn succ-fn)
  (do ((i start (funcall succ-fn i))
       (result nil))
      ((funcall test-fn i) (nreverse result))
    (push (funcall fn i) result)))

(defun mapa-b (fn a b &optional (step 1))
  (map-> fn
         a
         #'(lambda (x) (> x b))
         #'(lambda (x) (+ x step))))

;; open
(defmacro do-tuples/o (parms source &body body)
  (if parms
      (let ((src (gensym)))
        `(prog ((,src ,source))
            (mapc #'(lambda ,parms ,@body)
                  ,@(map0-n #'(lambda (n)
                                `(nthcdr ,n ,src))
                            (1- (length parms))))))))

(do-tuples/o (x y) '(a b c d) (princ (list x y)))  ; (a b)(b c)(c d)


(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))

;; closed
(defmacro do-tuples/c (parms source &body body)
  (if parms
      (let ((src (gensym))
            (rest (gensym))
            (bodfn (gensym)))
        (let ((len (length parms)))
          `(let ((,src ,source))
             (when (nthcdr ,(1- len) ,src)
               (labels ((,bodfn ,parms ,@body))
                 (do ((,rest ,src (cdr ,rest)))
                     ((not (nthcdr ,(1- len) ,rest))
                      ,@(mapcar #'(lambda (args)
                                    `(,bodfn ,@args))
                                (dt-args len rest src))
                      nil)
                   (,bodfn ,@(map1-n #'(lambda (n)
                                         `(nth ,(1- n) ,rest))
                                     len))))))))))

(defun dt-args (len rest src)
  (map0-n #'(lambda (m)
              (map1-n #'(lambda (n)
                          (let ((x (+ m n)))
                            (if (>= x len)
                                `(nth ,(- x len) ,src)
                                `(nth ,(1- x) ,rest))))
                      len))
          (- len 2)))

(do-tuples/c (x y) '(a b c d) (princ (list x y)))  ; (a b)(b c)(c d)(d a)


(do-tuples/o (x y) points (drawline x y))  ; polygonal line
(do-tuples/c (x y) points (drawline x y))  ; polygon

(do-tuples/o (x) '(a b c) (princ x))  ; ABC
(do-tuples/c (x) '(a b c) (princ x))  ; ABC

(do-tuples/o (x y z) '(a b c d) (princ (list x y z)))  ; (A B C)(B C D)
(do-tuples/c (x y z) '(a b c d) (princ (list x y z)))  ; (A B C)(B C D)(C D A)(D A B)

(LET ((#:G3257 '(A B C D)))
 (WHEN (NTHCDR 2 #:G3257)
  (LABELS ((#:G3259 (X Y Z) (PRINC (LIST X Y Z))))
   (DO ((#:G3258 #:G3257 (CDR #:G3258)))
       ((NOT (NTHCDR 2 #:G3258))
        (#:G3259 (NTH 0 #:G3258)
                 (NTH 1 #:G3258)
                 (NTH 0 #:G3257))
        (#:G3259 (NTH 1 #:G3258)
                 (NTH 0 #:G3257)
                 (NTH 1 #:G3257))
        NIL)
     (#:G3259 (NTH 0 #:G3258)
              (NTH 1 #:G3258)
              (NTH 2 #:G3258))))))

;; ------------------------------------------------------------
;; 5. 複数の値に渡る反復

(defmacro mvdo* (parm-cl test-cl &body body)
  (mvdo-gen parm-cl parm-cl test-cl body))

(defun mvdo-gen (binds rebinds test body)
  (if (null binds)
      (let ((label (gensym)))
        `(prog nil
            ,label
            (if ,(car test)
                (return (progn ,@(cdr test))))
            ,@body
            ,@(mvdo-rebind-gen rebinds)
            (go ,label)))
      (let ((rec (mvdo-gen (cdr binds) rebinds test body)))
        (let ((var/s (caar binds)) (expr (cadar binds)))
          (if (atom var/s)
              `(let ((,var/s ,expr)) ,rec)
              `(multiple-value-bind ,var/s ,expr ,rec))))))

(defun mvdo-rebind-gen (rebinds)
  (cond ((null rebinds) nil)
        ((< (length (car rebinds)) 3)
         (mvdo-rebind-gen (cdr rebinds)))
        (t (cons (list (if (atom (caar rebinds))
                           'setq
                           'multiple-value-setq)
                       (caar rebinds)
                       (third (car rebinds)))
                 (mvdo-rebind-gen (cdr rebinds))))))

(mvdo* ((x 1 (1+ x))
        ((y z) (values 0 0) (values z x)))
       ((> x 5) (list x y z))
       (princ (list x y z)))


(mvdo* (((px py) (pos player) (move player mx my))
        ((x1 y1) (pos obj1) (move obj1 (- px x1) (- py y1)))
        ((x2 y2) (pos obj2) (move obj2 (- px x2) (- py y2)))
        ((mx my) (mouse-vector) (mouse-vector))
        (win nil (touch obj1 obj2))
        (lose nil (and (touch obj1 player) (touch obj2 player))))
       ((or win lose) (if win 'win 'lose))
       (clear)
       (draw obj1)
       (draw obj2)
       (draw player))



;; multi-value psetq (p means parallel)
(defmacro mvpsetq (&rest args)
  (let* ((pairs (group args 2))
         (syms (mapcar #'(lambda (p)
                           (mapcar #'(lambda (x) (gensym))
                                   (mklist (car p))))
                       pairs)))
    (labels ((rec (ps ss)
               (if (null ps)
                   `(setq
                     ,@(mapcan #'(lambda (p s)
                                   (shuffle (mklist (car p))
                                            s))
                               pairs syms))
                   (let ((body (rec (cdr ps) (cdr ss))))
                     (let ((var/s (caar ps))
                           (expr (cadar ps)))
                       (if (consp var/s)
                           `(multiple-value-bind ,(car ss) ,expr
                              ,body)
                           `(let ((,@(car ss) ,expr))
                              ,body)))))))
      (rec pairs syms))))

(defun shuffle (x y)
  (cond ((null x) y)
        ((null y) x)
        (t (list* (car x) (car y)
                  (shuffle (cdr x) (cdr y))))))

(let ((w 0) (x 1) (y 2) (z 3))
  (mvpsetq (w x) (values 'a 'b) (y z) (values w x))
  (list w x y z))   ; (A B 0 1)

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(group '(a b c d e f g) 2)  ; ((A B) (C D) (E F) (G))

(defun mklist (obj)
  (if (listp obj) obj (lsit obj)))



(mvdo ((x 1 (1+ x))
       ((y z) (values 0 0) (values z x)))
      ((> x 5) (list x y z))
      (princ (list x y z)))


(defmacro mvdo (binds (test &rest result) &body body)
  (let ((label (gensym))
        (temps (mapcar #'(lambda (b)
                           (if (listp (car b))
                               (mapcar #'(lambda (x) (gensym))
                                       (car b))
                               (gensym)))
                       binds)))
    `(let ,(mappend #'mklist temps)
       (mvpsetq ,@(mapcan #'(lambda (b var)
                              (list var (cadr b)))
                          binds
                          temps))
       (prog ,(mapcar #'(lambda (b var) (list b var))
                      (mappend #'mklist (mapcar #'car binds))
                      (mappend #'mklist temps))
          ,label
          (if ,test
              (return (progn ,@result)))
          ,@body
          (mvpsetq ,@(mapcan #'(lambda (b)
                                 (if (third b)
                                     (list (car b)
                                           (third b))))
                             binds))
          (go ,label)))))


;; ------------------------------------------------------------
;; 6. マクロの必要性

(defun fnif (test then &optional else)
  (if test
      (funcall then)
      (if else (funcall else))))

(fnif (rich)
      #'(lambda () (go-sailing))
      #'(lambda () (rob-bank)))

(dolist (b bananas)
  (peel b)
  (eat b))

(mapc #'(lambda (b)
          (peel b)
          (eat b))
      bananas)


(defun forever (fn)
  (do ()
      (nil)
    (funcall fn)))

;; ------------------------------------------------------------
;; end
