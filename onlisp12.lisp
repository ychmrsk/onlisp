;; ============================================================
;; 12. 汎変数   generalized variable
;; ============================================================
;; 1. 概念

(setq lst '(a b c))
(setf (car lst) 480)
lst

;; inversion = 変数を求める式から決定的な命令へ変換すること
;; car cdr nth aref get gethash defstruct etc.

;; (setf 'generalized-variable' new-value)

(defmacro toggle (obj)   ; error
  `(setf ,obj (not ,obj)))

(let ((lst '(a b c)))
  (toggle (car lst))
  lst)



(defvar *friends* (make-hash-table))

(setf (gethash 'mary *friends*) (make-hash-table))

(setf (gethash 'john (gethash 'mary *friends*)) t)


(setf (gethash x (gethash y *friends*))
      (not (gethash x (gethash y *friends*))))


(defmacro friend-of (p q)
  `(gethash ,p (gethash ,q *friends*)))

(toggle (friend-of x y))

;; ------------------------------------------------------------
;; 2. 複数回の評価に関わる問題

(defmacro toggle (obj)
  `(setf ,obj (not ,obj)))

(toggle (nth (incf i) lst))
;; macroexpand
(SETF (NTH (INCF I) LST) (NOT (NTH (INCF I) LST)))

(let ((lst '(t nil t))
      (i -1))
  (toggle (nth (incf i) lst))
  lst)
;; > t nil t


;; マクロの名前、（汎変数のあとに）付加的に取るかもしれない引数、汎変数の新しい値を返す関数
(define-modify-macro toggle () not)

(let ((lst '(t nil t))
      (i -1))
  (toggle (nth (incf i) lst))
  lst)

;; ------------------------------------------------------------
;; 3. 新しいユーティリティ

;; setq := set quoted
;; setf := set field

(defmacro with-my-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))

(defmacro allf (val &rest args)
  (with-my-gensyms (gval)
    `(let ((,gval ,val))
       (setf ,@(mapcan #'(lambda (a) (list a gval))
                       args)))))

(defmacro nilf (&rest args) `(allf nil ,@args))

(defmacro tf (&rest args) `(allf t ,@args))

(defmacro toggle (&rest args)
  `(progn
     ,@(mapcar #'(lambda (a) `(toggle2 ,a))
               args)))
(define-modify-macro toggle2 () not)



(nconc x y)
(setq x (nconc x y))

(define-modify-macro concf (obj) nconc)

;; like 'push'
(define-modify-macro conc1f (obj)
  (lambda (place obj)
    (nconc place (list obj))))

;; like 'pushnew'
(define-modify-macro concnew (obj &rest args)
  (lambda (place obj &rest args)
    (unless (apply #'member obj place args)
      (nconc place (list obj)))))


;; ------------------------------------------------------------
;; 4. 更に複雑なユーティリティ

(setf x (+ x y))
(incf x y)

(setf (obj-dx o) (* (obj-dx o) factor))
(_f * (obj-dx o) factor)

(defmacro _f (op place &rest args)  ; wrong
  `(setf ,place (,op ,place ,@args)))


;; get-setf-method
(incf (aref a (incf i)))

;;> (get-setf-method '(aref a (incf i)))
(#:G3220 #:G3221) ;
(A (INCF I)) ;
(#:G3222) ;
(SYSTEM::STORE #:G3220 #:G3221 #:G3222) ;
(AREF #:G3220 #:G3221)

(let* ((#:G3220 a)
       (#:G3221 (incf i))
       (#:G3222 (1+ (aref #:G3220 #:G3221))))
  (system:set-aref #:G3222 #:G3220 #:G3221))


(defmacro _f (op place &rest args)
  (multiple-value-bind (vars forms var set access)
      (get-setf-method place)
    `(let* (,@(mapcar #'list vars forms)
            (,(car var) (,op ,access ,@args)))
       ,set)))

(defmacro pull (obj place &rest args)
  (multiple-value-bind (vars forms var set access)
      (get-setf-method place)
    (let ((g (gensym)))
      `(let* ((,g ,obj)
              ,@(mapcar #'list vars forms)
              (,(car var) (delete ,g ,access ,@args)))
         ,set))))

(defmacro pull-if (test place &rest args)
  (multiple-value-bind (vars form var set access)
      (get-setf-method place)
    (let ((g (gensym)))
      `(let* ((,g ,test)
              ,@(mapcar #'list vars forms)
              (,(car var) (delete-if ,g ,access ,@args)))
         ,set))))

(defmacro popn (n place)
  (multiple-value-bind (vars form var set access)
      (get-setf-method place)
    (with-my-gensyms (gn glst)
      `(let* ((,gn ,n)
              ,@(mapcar #'list vars forms)
              (,glst ,access)
              (,(car var) (nthcdr ,gn ,glst)))
         (prog1 (subseq ,glst 0 ,gn)
           ,set)))))


;; memoize
(_f memoize (symbol-function 'foo))
;; conc1f
(defmacro conc1f (lst obj)
  `(_f nconc ,lst (list ,obj)))

(setq x '(a b c d e f))
(popn 3 x)
x


(if (> y x) (rotatef x y))

(setq x 1 y 2 z 3)  ; 3
(sortf > x y z)  ; 3
(list x y z)  ; (3 2 1)

(defmacro sortf (op &rest places)
  (let* ((meths (mapcar #'(lambda (p)
                            (multiple-value-list (get-setf-method p)))
                        places))
         (temps (apply #'append (mapcar #'third meths))))
    `(let* ,(mapcar #'list
                    (mapcan #'(lambda (m)
                                (append (first m) (third m)))
                            meths)
                    (mapcan #'(lambda (m)
                                (append (second m) (list (fifth m))))
                            meths))
       ,@(mapcon #'(lambda (rest)
                     (mapcar #'(lambda (arg)
                                 `(unless (,op ,(car rest) ,arg)
                                    (rotatef ,(car rest) ,arg)))
                             (cdr rest)))
                 temps)
       ,@(mapcar #'fourth meths))))






;; ------------------------------------------------------------
;; 5. インヴァージョンを定義する



;; ------------------------------------------------------------
;; end
