;; ============================================================
;; アナフォリックマクロ
;; ============================================================

(let ((result (big-long-calculation)))
  (if result
      (foo result)))

(if (big-long-calculation)
    (foo it))

;; ------------------------------------------------------------
;; 1. アナフォリックな変種オペレータ

;; anapholic if
(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(aif (big-long-calculation)
     (foo it))

(let ((it (big-long-calculation)))
  (if it (foo it) nil))

;; anapholic when
(defmacro awhen (test-form &body body)
  `(aif ,test-form
        (progn ,@body)))

(awhen (big-long-calculation)
       (foo it)
       (bar it))

(aif (big-long-calculation)
     (progn (foo it)
            (bar it)))

(let ((it (big-long-calculation)))
  (if it
      (progn (foo it)
             (bar it))
      nil))

;; anapholic while
(defmacro awhile (expr &body body)
  `(do ((it ,expr ,expr))
       ((not it))
     ,@body))

(awhile (poll *fridge*)
        (eat it))

(do ((it (poll *fridge*)))
    ((not it))
  (eat it))

;; anapholic and
(defmacro aand (&rest args)
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(aif ,(car args)
                 (aand ,@(cdr args))))))

(aand (owner x) (address it) (town it))

(aif (owner x)
     (aif (address it)
          (town it)))

(let ((it (owner x)))
  (if it
      (let ((it (address it)))
        (if it
            (town it)
            nil))))

;; anapholic cond
(defmacro acond (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (sym (gensym)))
        `(let ((,sym ,(car cl1)))
           (if ,sym
               (let ((it ,sym)) ,@(cdr cl1))
               (acond ,@(cdr clauses)))))))


;; anapholic lambda
(defmacro alambda (parms &body body)
  `(labels ((self ,parms ,@body))
     #'self))

(defun count-instances (obj lists)
  (labels ((instances-in (list)
             (if list
                 (+ (if (eq (car list) obj) 1 0)
                    (instances-in (cdr list)))
                 0)))
    (mapcar #'instances-in lists)))

(defun count-instances (obj lists)
  (mapcar (alambda (list)
                   (if list
                       (+ (if (eq (car list) obj) 1 0)
                          (self (cdr list)))
                       0))
          lists))

;; recursion using alambda
(alambda (x) (if (= x 0) 1 (* x (self (1- x)))))



(defmacro ablock (tag &rest args)
  `(block ,tag
     ,(funcall (alambda (args)
                        (case (length args)
                          (0 nil)
                          (1 (car args))
                          (t `(let ((it ,(car args)))
                                ,(self (cdr args))))))
               args)))

;; ------------------------------------------------------------
;; 2. 失敗

(cdr '(a))  ; NIL

(= 1 0)  ; NIL

(find-if #'oddp '(2 4 6))  ; NIL


(setq synonyms '((yes . t) (no . nil)))
(assoc 'yes synonyms)  ; (YES . T)
(assoc 'no synonyms)   ; (NO)
(assoc 'foo synonyms)  ; NIL


(member-if #'null '(2 nil 6))  ; (NIL 6)


(setf edible (make-hash-table)
      (gethash 'olive-oil edible) t
      (gethash 'motor-oil edible) nil)
(gethash 'motor-oil edible)  ; NIL ; T

(defun edible? (x)
  (multiple-value-bind (val found?) (gethash x edible)
    (if found?
        (if val 'yes 'no)
        'maybe)))

(mapcar #'edible? '(motor-oil olive-oil iguana))  ; (NO YES MAYBE)

(defun edible? (x)
  (aif2 (gethash x edible)
        (if it 'yes 'no)
        'maybe))

;; anapholic if
(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro aif2 (test &optional then else)
  (let ((win (gensym)))
    `(multiple-value-bind (it ,win) ,test
       (if (or it ,win) ,then ,else))))


;; anapholic when
(defmacro awhen (test-form &body body)
  `(aif ,test-form
        (progn ,@body)))

(defmacro awhen2 (test &body body)
  `(aif2 ,test
         (progn ,@body)))


;; anapholic while
(defmacro awhile (expr &body body)
  `(do ((it ,expr ,expr))
       ((not it))
     ,@body))

(defmacro awhile2 (test &body body)
  (let ((flag (gensym)))
    `(let ((,flag t))
       (while ,flag
         (aif2 ,test
               (progn ,@body)
               (setq ,flag nil))))))


;; anapholic cond
(defmacro acond (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (sym (gensym)))
        `(let ((,sym ,(car cl1)))
           (if ,sym
               (let ((it ,sym)) ,@(cdr cl1))
               (acond ,@(cdr clauses)))))))

(defmacro acond2 (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (val (gensym))
            (win (gensym)))
        `(multiple-value-bind (,val win) ,(car cl1)
           (if (or ,val ,win)
               (let ((it ,val)) ,@(cdr cl1))
               (acond2 ,@(cdr clauses))))))))

(let ((g (gensym)))
  (defun read2 (&optional (str *standard-input*))
    (let ((val (read str nil g)))
      (unless (equal val g)
        (values val t)))))

(defmacro do-file (filename &body body)
  (let ((str (gensym)))
    `(with-open-file (,str ,filename)
       (awhile2 (read2 ,str)
                ,@body))))

(defun our-load (filename)
  (do-file filename (eval it)))


;; ------------------------------------------------------------
;; 3. 参照の透明性

(list x
      (setq x (not x))
      x)

;; rewrite context
(defmacro if (test then &optional else)
  `(let ((that ,test))
     (if that ,then ,else)))

;; new context
(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

;; ------------------------------------------------------------
;; 
