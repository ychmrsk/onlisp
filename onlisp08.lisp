;; ============================================================
;; 8. いつマクロを使うべきか
;; ============================================================
;; 8.1 他に何も関係しないとき

(defun 1+ (x) (+ 1 x))
(defmacro 1+ (x) `(+ 1 ,x))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

;; マクロにできて関数にできないこと
;; 1. 引数の評価を制御（抑制）する
;; 2. 呼び出し側のソースコードそのものへと展開される


;; ------------------------------------------------------------
;; 8.2 マクロと関数どちらがよい？

(defun avg (&rest args)
  (/ (apply #'+ args) (length args)))

(defmacro avg (&rest args)
  `(/ (+ ,@args) ,(length args)))



(funcall #'(lambda (x y) (avg x y)) 1 3)

;; ------------------------------------------------------------
;; 8.3 マクロの応用例

(defmacro nil! (x)
  `(setf ,x nil))

(do ()
    ((not <condition>))
  <body>)


(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(while <condition>
  <body>)


(defun foo (x) (* x 2))
(setf (symbol-function 'foo)
      #'(lambda (x) (* x 2)))

(defmacro our-defun (name parms &body body)
  `(progn
     (setf (symbol-function ',name)
           #'(lambda ,parms (block ,name ,@body)))
     ',name))



(defun move-objs (objs dx dy)
  (multiple-value-bind (x0 y0 x1 y1) (bounds objs)
    (dolist (o objs)
      (incf (obj-x o) dx)
      (incf (obj-y o) dy))
    (multiple-value-bind (xa ya xb yb) (bounds objs)
      (redraw (min x0 xa) (min y0 ya)
              (max x1 xb) (max y1 yb)))))

(defun scale-objs (objs factor)
  (multiple-value-bind (x0 y0 x1 y1) (bounds objs)
    (dolist (o objs)
      (setf (obj-dx o) (* (obj-dx o) factor)
            (obj-dy o) (* (obj-dy o) factor)))
    (multiple-value-bind (xa ya xb yb) (bounds objs)
      (redraw (min x0 xa) (min y0 ya)
              (max x1 xb) (max y1 yb)))))


(defmacro with-redraw ((var obj) &body body)
  (let ((gob (gensym))
        (x0 (gensym)) (y0 (gensym))
        (x1 (gensym)) (y1 (gensym)))
    `(let ((,gob ,objs))
       (multiple-value-bind (,x0 ,y0 ,x1 ,y1) (bounds ,gob)
         (dolist (,var ,gob) ,@body)
         (multiple-value-bind (xa ya xb yb) (bounds ,gob)
           (redraw (min ,x0 xa) (min ,y0 ya)
                   (max ,x1 xb) (max ,y1 yb)))))))

(defun move-objs (objs dx dy)
  (with-redraw (o objs)
    (incf (obj-x o) dx)
    (incf (obj-y o) dy)))

(defun scale-objs (objs factor)
  (with-redraw (o objs)
    (setf (obj-dx o) (* (obj-dx o) factor)
          (obj-dy o) (* (obj-dy o) factor))))











;; ------------------------------------------------------------
;; end
