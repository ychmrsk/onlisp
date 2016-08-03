;; ============================================================
;; 09. 変数捕捉
;; ============================================================
;; 1. マクロ引数の捕捉

(defmacro for ((var start stop) &body body)
  `(do ((,var ,start (1+ ,var))
        (limit ,stop))
       ((> ,var limit))
     ,@body))

(for (x 1 5)
     (princ x))

(for (limit 1 5)
     (princ limit))

(macroexpand-1 '(for (limit 1 5) (princ limit)))
;; expand
(DO ((LIMIT 1 (1+ LIMIT))
     (LIMIT 5))
    ((> LIMIT LIMIT))
  (PRINC LIMIT))


(let ((limit 5))
  (for (i 1 10)
       (when (> i limit)
         (princ i))))
;; macroexpand
(DO ((I 1 (1+ I))
     (LIMIT 10))
    ((> I LIMIT))
  (WHEN (> I LIMIT)
    (PRINC I)))

;; ------------------------------------------------------------
;; 2. フリーシンボルの捕捉

(defvar w nil)

(defmacro gripe (warning)
  `(progn (setq w (nconc w (list ,warning)))
          nil))

(defun sample-ratio (v w)
  (let ((vn (length v)) (wn (length w)))
    (if (or (< vn 2) (< wn 2))
        (gripe "sample < 2")
        (/ vn wn))))

(let ((lst '(b)))
  (sample-ratio nil lst)
  lst)


(defun sample-ratio (v w)
  (let ((vn (length v)) (wn (length w)))
    (if (or (< vn 2) (< wn 2))
        (progn (setq w (nconc w (list "sample < 2")))
               nil)
        (/vn wn))))

;; ------------------------------------------------------------
;; 3. 捕捉はいつ起きるのか

;; フリー
(let ((x y) (z 10))
  (list w x z))

(let ((x x))
  x)

;; 骨格
(defmacro foo (x y)
  `(/ (+ ,x 1) ,y))

(foo (- 5 2) 6)
(/ (+ (- 5 2) 1) 6)

(/ (+         1)  )  ; bone

;; 捕捉可能
(defmacro cap1 ()
  '(+ x 1))

(defmacro cap2 (var)
  `(let ((x ...)
         (,var ...))
     ...))

(defmacro cap3 (var)
  `(let ((x ...))
     (let ((,var ...))
       ...)))

(defmacro cap4 (var)
  `(let ((,var ...))
     (let ((x ...))
       ...)))

(defmacro safe1 (var)
  `(progn (let ((x 1))
            (print x))
          (let ((,var 1))
            (print ,var))))

(defmacro cap5 (&body body)
  `(let ((x ...))
     ,@body))  ; <- eval point

(defmacro safe2 (expr)
  `(let ((x ,expr))  ; <- eval point
     (cons x 1)))

(defmacro safe3 (var &body body)
  `(let (,var ...)
     ,@body))



(defmacro for ((var start stop) &body body)  ; error
  `(do ((,var ,start (1+ ,var))
        (limit ,stop))
       ((> var limit))
     ,@body))

(for (limit 1 5)
     (princ limit))

(let ((limit 0))
  (for (x 1 10)
       (incf limit x))
  limit)


(defmacro pathological (&body body)  ; error
  (let* ((syms (remove-if (complement #'symbolp)
                          (flatten body)))
         (var (nth (random (length syms))
                   syms)))
    `(let ((,var 99))
       ,@body)))

;; ------------------------------------------------------------
;; 4. 適切な名前によって捕捉を避ける

;; global variables  : *package*


;; ------------------------------------------------------------
;; 5. 優先評価によって捕捉を避ける

(defmacro before (x y seq)
  `(let ((seq ,seq))
     (< (position ,x seq)
        (position ,y seq))))

(defmacro before (x y seq)
  `(let ((xval ,x) (yval ,y) (seq ,seq))
     (< (position xval seq)
        (position yval seq))))


(before (progn (setq seq '(b a)) 'a)
        'b
        '(a b))

(LET ((SEQ '(A B)))
  (< (POSITION (PROGN (SETQ SEQ '(B A)) 'A) SEQ)
     (POSITION 'B SEQ)))  ; nil

(LET ((XVAL (PROGN (SETQ SEQ '(B A)) 'A))
      (YVAL 'B)
      (SEQ '(A B)))
  (< (POSITION XVAL SEQ)
     (POSITION YVAL SEQ)))  ; T


(defmacro for ((var start stop) &body body)
  `(do ((,var ,start (1+ ,var))
        (limit ,stop))
       ((> ,var limit))
     ,@body))

(for (limit 1 5)
     (princ limit))

(DO ((LIMIT 1 (1+ LIMIT))
     (LIMIT 5))
    ((> LIMIT LIMIT))
  (PRINC LIMIT))


(let ((limit 0))
  (for (x 1 10)
       (incf limit x))
  limit)

(DO ((X 1 (1+ X))
     (LIMIT 10))
    ((> X LIMIT))
  (INCF LIMIT X))


(defmacro for ((var start stop) &body body)
  `(do ((b #'(lambda (,var) ,@body))
        (count ,start (1+ count))
        (limit ,stop))
       ((> count limit))
     (funcall b count)))

(for (limit 1 5)
     (princ limit))

(DO ((B #'(LAMBDA (LIMIT) (PRINC LIMIT)))
     (COUNT 1 (1+ COUNT))
     (LIMIT 5))
    ((> COUNT LIMIT))
 (FUNCALL B COUNT))


(let ((limit 0))
  (for (x 1 10)
       (incf limit x))
  limit)

(DO ((B #'(LAMBDA (X) (INCF LIMIT X)))
     (COUNT 1 (1+ COUNT))
     (LIMIT 10))
    ((> COUNT LIMIT))
 (FUNCALL B COUNT))



;; ------------------------------------------------------------
;; 6. Gynsymによって捕捉を避ける

(defmacro for ((var start stop) &body body)
  `(do ((,var ,start (1+ ,var))
        (xsf2jsh ,stop))
       ((> ,var xsf2jsh))
     ,@body))

(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))


;; ------------------------------------------------------------
;; 7. パッケージによって捕捉を避ける

(defmacro for ((var start stop) &body body)
  `(do ((,var ,start (1+ ,var))
        (limit ,stop))
       ((> ,var limit))
     ,@body))

;; in package "mycode"
;;   limit as 1st arg (var) => mycode::limit 

;; in for macro
;;   limit => macros::limit

;; ------------------------------------------------------------
;; 8. 異なる名前空間での捕捉

(defun fn (x) (+ x 1))

(defmacro mac (x) `(fn ,x))

(mac 10)  ; 11

(labels ((fn (y) (- y 1)))
  (mac 10))  ; 9


(block nil
  (list 'a
        (do ((x 1 (1+ x)))
            (nil)
          (if (> x 5)
              (return-from nil x)
              (princ x)))))

;; ------------------------------------------------------------
;; 9. 変数捕捉にこだわる理由



;; ------------------------------------------------------------
;; end
