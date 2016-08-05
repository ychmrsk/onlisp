;; ============================================================
;; コンパイル時の計算処理
;; ============================================================
;; 1. 新しいユーティリティ

(defun avg (&rest args)
  (/ (apply #'+ args) (length args)))

(defmacro avg (&rest args)
  `(/ (+ ,@args) ,(length args)))


(defun most-of (&rest args)
  (let ((all 0)
        (hits 0))
    (dolist (a args)
      (incf all)
      (if a (incf hits)))
    (> hits (/ all 2))))

(defmacro most-of (&rest args)
  (let ((need (floor (/ (length args) 2)))
        (hits (gensym)))
    `(let ((,hits 0))
       (or ,@(mapcar #'(lambda (a)
                         `(and ,a (> (incf ,hits) ,need)))
                     args)))))

;; (macroexpand-1 '(most-of (a) (b) (c)))
(LET ((#:G3210 0))
  (OR (AND (A) (> (INCF #:G3210) 1))
      (AND (B) (> (INCF #:G3210) 1))
      (AND (C) (> (INCF #:G3210) 1))))



(defun map0-n (fn n)
  (mapa-b fn 0 n))
(defun mapa-b (fn a b &optional (step 1))
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (reverse result))
    (push (funcall fn i) result)))

(defmacro with-my-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))



(defun nthmost (n lst)
  (nth n (sort (copy-list lst) #'>)))

(defmacro nthmost (n lst)
  (if (and (integerp n) (< n 20))
      (with-my-gensyms (glst gi)
        (let ((syms (map0-n #'(lambda (x) (gensym)) n)))
          `(let ((,glst ,lst))
             (unless (< (length ,glst) ,(1+ n))
               ,@(gen-start glst syms)
               (dolist (,gi ,glst)
                 ,(nthmost-gen gi syms t))
               ,(car (last syms))))))
      `(nth ,n (sort (copy-list) #'>))))

(defun gen-start (glst syms)
  (reverse
   (maplist #'(lambda (syms)
                (let ((var (gensym)))
                  `(let ((,var (pop ,glst)))
                     ,(nthmost-gen var (reverse syms)))))
            (reverse syms))))

(defun nthmost-gen (var vars &optional long?)
  (if (null vars)
      nil
      (let ((else (nthmost-gen var (cdr vars) long?)))
        (if (and (not long?) (null else))
            `(setq ,(car vars) ,var)
            `(if (> ,var ,(car vars))
                 (setq ,@(mapcan #'list
                                 (reverse vars)
                                 (cdr (reverse vars)))
                       ,(car vars) ,var)
                 ,else)))))

(nthmost 2 '(2 6 1 5 3 4))

;; (macroexpand-1 '(nthmost 2 num))
(LET ((#:G3257 NUM))
  (UNLESS (< (LENGTH #:G3257) 3)
    (LET ((#:G3264 (POP #:G3257)))
      (SETQ #:G3259 #:G3264))
    (LET ((#:G3263 (POP #:G3257)))
      (IF (> #:G3263 #:G3259)
          (SETQ #:G3260 #:G3259 #:G3259 #:G3263)
          (SETQ #:G3260 #:G3263)))
    (LET ((#:G3262 (POP #:G3257)))
      (IF (> #:G3262 #:G3259)
          (SETQ #:G3261 #:G3260 #:G3260 #:G3259 #:G3259 #:G3262)
          (IF (> #:G3262 #:G3260)
              (SETQ #:G3261 #:G3260 #:G3260 #:G3262)
              (SETQ #:G3261 #:G3262))))
    (DOLIST (#:G3258 #:G3257)
      (IF (> #:G3258 #:G3259)
          (SETQ #:G3261 #:G3260 #:G3260 #:G3259 #:G3259 #:G3258)
          (IF (> #:G3258 #:G3260)
              (SETQ #:G3261 #:G3260 #:G3260 #:G3258)
              (IF (> #:G3258 #:G3261)
                  (SETQ #:G3261 #:G3258)
                  NIL))))
    #:G3261)) ;

(LET ((glst NUM))
  (UNLESS (< (LENGTH glst) 3)
    (LET ((var-1 (POP glst)))  ; ,@(gen-start glst syms) :syms=(most-0)
      (SETQ most-0 var-1))
    (LET ((var-2 (POP glst)))  ; ,@(gen-start glst syms) :syms=(most-1 most-0)
      (IF (> var-2 most-0)
          (SETQ most-1 most-0 most-0 var-2)
          (SETQ most-1 var-2)))
    (LET ((var-3 (POP glst)))  ; ,@(gen-start glst syms) :syms=(most-2 most-1 most-0)
      (IF (> var-3 most-0)
          (SETQ most-2 most-1 most-1 most-0 most-0 var-3)
          (IF (> var-3 most-1)
              (SETQ most-2 most-1 most-1 var-3)
              (SETQ most-2 var-3))))
    (DOLIST (gi glst)
      (IF (> gi most-0)  ; ,(nthmost-gen gi syms t)
          (SETQ most-2 most-1 most-1 most-0 most-0 gi)
          (IF (> gi most-1)
              (SETQ most-2 most-1 most-1 gi)
              (IF (> gi most-2)
                  (SETQ most-2 gi)
                  NIL))))
    most-2)) ;

;; ------------------------------------------------------------
;; 2. 例：Bezier曲線

;; x = (x3-3x2+3x1-x0)*u^3 + (3x2-6x1+3x0)*u^2 + (3x1-3x0)*u + x0
;; y = (y3-3y2+3y1-y0)*u^3 + (3y2-6y1+3y0)*u^2 + (3y1-3y0)*u + y0

(defconstant *segs* 20)
(defconstant *du* (/ 1- 0 *segs*))
(defconstant *pts* (make-array (list (1+ *segs*) 2)))

(defmacro genbez (x0 y0 x1 y1 x2 y2 x3 y3)
  (with-my-gensyms (gx0 gx1 gy0 gy1 gx3 gy3)
    `(let ((,gx0 ,x0) (,gy0 ,y0)
           (,gx1 ,x1) (,gy1 ,y1)
           (,gx3 ,x3) (,gy3 ,y3))
       (let ((cx (* (- ,gx1 ,gx0) 3))
             (cy (* (- ,gy1 ,gy0) 3))
             (px (* (- ,x2 ,gx1) 3))
             (py (* (- ,y2 ,gy1) 3)))
         (let ((bx (- px cx))
               (by (- py cy))
               (ax (- ,gx3 px ,gx0))
               (ay (- ,gy3 py ,gy0)))
           (setf (aref *pts* 0 0) ,gx0
                 (aref *pts* 0 1) ,gy0)
           ,@(map1-n #'(lambda (n)
                         (let* ((u (* n *du*))
                                (u^2 (* u u))
                                (u^3 (expt u 3)))
                           `(setf (aref *pts* ,n 0) (+ (* ax ,u^3)
                                                       (* bx ,u^2)
                                                       (* cx ,u)
                                                       ,gx0)
                                  (aref *pts* ,n 1) (+ (* ay ,u^3)
                                                       (* by ,u^2)
                                                       (* cy ,u)
                                                       ,gy0))))
                     (1- *segs*))
           (setf (aref *pts* *segs* 0) ,gx3
                 (aref *pts* *segs* 1) ,gy3))))))





;; ------------------------------------------------------------
;; 3. 応用




;; ------------------------------------------------------------
;; end
