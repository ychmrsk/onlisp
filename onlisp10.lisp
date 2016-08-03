;; ============================================================
;; 10. マクロのその他の落し穴
;; ============================================================
;; 1. 評価の回数

;; 適切
(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))

;; 複数回の評価
(defmacro for ((var start stop) &body body)
  `(do ((,var ,start (1+ ,var)))
       ((> ,var ,stop))
     ,@body))

;; 評価の順番
(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,gstop ,stop)
          (,var ,start (1+ ,var)))
         ((> ,var ,gstop))
       ,@body)))


(let ((x 2))
  (for (i 1 (incf x))
       (princ i)))

;; ------------------------------------------------------------
;; 2. 評価の順番

(setq x 10)  ; 10
(+ (setq x 3) x)  ; 6

(let ((x 1))
  (for (i x (setq x 13))
       (princ i)))

;; ------------------------------------------------------------
;; 3. 関数によらないマクロ展開

(defmacro nil! (x)
  (incf *nil!s*)
  `(setf ,x nil))

(defmacro string-call (opstring &rest args)  ; error
  `(,(intern opstring) ,@args))

(defun our+ (x y) (+ x y))

(string-call "OUR+" 2 3)


(defun et-al (&rest args)
  (nconc args (list 'et 'al)))

(et-al 'smith 'jones)

(setq greats '(leonardo michelangelo))
(apply #'et-al greats)
greats


(defmacro echo (&rest args)
  `',(nconc args (list 'amen)))

(defun foo () (echo x))
(foo)
(foo)

(defmacro echo (&rest args)
  `'(,@args amen))  ; ,@ == append


(defmacro crazy (expr) (nconc expr (list t)))

(defun foo () (crazy (list)))
(foo)


;; ------------------------------------------------------------
;; 4. 再帰

(defun our-length (x)
  (if (null x)
      0
      (1+ (our-length (cdr x)))))

(defun our-length (x)
  (do ((len 0 (1+ len))
       (y x (cdr y)))
      ((null y) len)))

;; ok
(defun ntha (n lst)
  (if (= n 0)
      (car lst)
      (ntha (- n 1) (cdr lst))))

;; ng
(defmacro nthb (n lst)
  `(if (= ,n 0)
       (car ,lst)
       (nthb (- ,n 1) (cdr ,lst))))

(nthb x y)

(if (= x 0)
    (car y)
    (nthb (- x 1) (cdr y)))

(if (= x 0)
    (car y)
    (if (= (- x 1) 0)
        (car (cdr y))
        (nthb (- (- x 1) 1) (cdr (cdr y)))))

(defmacro nthc (n lst)
  `(do ((n2 ,n (1- n2))
        (lst2 ,lst (cdr lst2)))
       ((= n2 0) (car lst2))))



(defmacro nthd (n lst)
  `(nth-fn ,n ,lst))

(defun nth-fn (n lst)
  (if (= n 0)
      (car lst)
      (nth-fn (- n 1) (cdr lst))))

(defmacro nthe (n lst)
  `(labels ((nth-fn (n lst)
              (if (= n 0)
                  (car lst)
                  (nth-fn (- n 1) (cdr lst)))))
     (nth-fn ,n ,lst)))




(defmacro ora (&rest args)
  (or-expand args))

(defun or-expand (args)
  (if (null args)
      nil
      (let ((sym (gensym)))
        `(let ((,sym ,(car args)))
           (if ,sym
               ,sym
               ,(or-expand (cdr args)))))))


(defmacro orb (&rest args)
  (if (null args)
      nil
      (let ((sym (gensym)))
        `(let ((,sym ,(car args)))
           (if ,sym
               ,sym
               (orb ,@(cdr args)))))))

(orb x y)

(let ((g2 x))
  (if g2
      g2
      (let ((g3 y))
        (if g3
            g3
            nil))))


;; ------------------------------------------------------------
;; end
