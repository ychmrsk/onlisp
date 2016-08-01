;; function as return value

(remove-if-not #'pred lst)

(remove-if #'(lambda (x) (not (pred x))) lst)

(remove-if (complement #'pred) lst)


(defun joiner (obj)
  (typecase obj
    (cons #'append)
    (number #'+)))

(defun join (&rest args)
  (apply (joiner (car args)) args))


(defun make-adder (n)
  #'(lambda (x) (+ x n)))

(setq add3 (make-adder 3))
(funcall add3 2)


(defun complement (fn)
  #'(lambda (&rest args) (not (apply fn args))))


;; orthogonal

;; complement & pred
;; setf & get
;; remove-if vs delete-if
;; reverse vs nreverse
;; append vs nconc

(defvar *!equivs* (make-hash-table))

(defun ! (fn)
  (or (gethash fn *!equivs*) fn))

(defun def! (fn fn!)
  (setf (gethash fn *!equivs*) fn!))


(def! #'remove-if #'delete-if)

(delete-if #'oddp lst)
(funcall (! #'remove-if) #'oddp lst)


;; memoize

(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
        (multiple-value-bind (val win) (gethash args cache)
          (if win
              val
              (setf (gethash args cache)
                    (apply fn args)))))))


(setq slowid (memoize #'(lambda (x) (sleep 5) x)))

(time (funcall slowid 1))
;; Real time: 5.005093 sec.
;; Run time: 0.0 sec.
;; Space: 16 Bytes

(time (funcall slowid 1))
;; Real time: 8.0E-6 sec.
;; Run time: 0.0 sec.
;; Space: 16 Bytes



;; synthesize function
(defun compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
        #'(lambda (&rest args)
            (reduce #'funcall fns
                    :from-end t
                    :initial-value (apply fn1 args))))
      #'identity))

(compose #'list #'1+)
#'(lambda (x) (list (1+ x)))

(funcall (compose #'1+ #'find-if) #'oddp '(2 3 4))

((lambda (x) (1+ (find-if x))) #'oddp '(2 3 4))
(1+ (find-if #'oddp '(2 3 4)))


(defun complement (pred)
  (compose #'not pred))



(defun fif (if then &optional else)
  #'(lambda (x)
      (if (funcall if x)
          (funcall then x)
          (if else (funcall else x)))))

;; functional intersection
(defun fint (fn &rest fns)
  (if (null fns)
      fn
      (let ((chain (apply #'fint fns)))
        #'(lambda (x)
            (and (funcall fn x) (funcall chain x))))))

(defun fun (fn &rest fns)
  (if (null fns)
      fn
      (let ((chain (apply #'fun fns)))
        #'(lambda (x)
            (or (funcall fn x) (funcall chain x))))))


;; recursion

;; cdr
(defun our-length (lst)
  (if (null lst)
      0
      (1+ (our-length (cdr lst)))))

(defun our-every (fn lst)
  (if (null lst)
      t
      (and (funcall fn (car lst))
           (our-every fn (cdr lst)))))


(defun lrec (rec &optional base)
  (labels ((self (lst)
             (if (null lst)
                 (if (functionp base)
                     (funcall base)
                     base)
                 (funcall rec (car lst)
                          #'(lambda ()
                              (self (cdr lst)))))))
    #'self))

(lrec #'(lambda (x f) (1+ (funcall f))) 0)

(lrec #'(lambda (x f) (and (oddp x) (funcall f))) t)

;; copy-list
(lrec #'(lambda (x f) (cons x (funcall f))))

;; remove-duplicates
(lrec #'(lambda (x f) (adjoin x (funcall f))))

;; find-if, for some function fn
(lrec #'(lambda (x f) (if (fn x) x (funcall f))))

;; some, for some function fn
(lrec #'(lambda (x f) (or (fn x) (funcall f))))



(a b c) = (a . (b . (c . nil)))
(a b (c d)) = (a . (b . ((c . (d . nil)) . nil)))


(setq x '(a b)
      listx (list x 1))
(eq x (car (copy-list listx)))
(eq x (car (copy-tree listx)))



(defun our-copy-tree (tree)
  (if (atom tree)
      tree
      (cons (our-copy-tree (car tree))
            (if (cdr tree) (our-copy-tree (cdr tree))))))

(defun count-leaves (tree)
  (if (atom tree)
      1
      (+ (count-leaves (car tree))
         (or (if (cdr tree) (count-leaves (cdr tree)))
             1))))


(defun flatten (tree)
  (if (atom tree)
      (mklist tree)
      (nconc (flatten (car tree))
             (if (cdr tree) (flatten (cdr tree))))))


(defun rfind-if (fn tree)
  (if (atom tree)
      (and (funcall fn tree) tree)
      (or (rfind-if fn (car tree))
          (if (cdr tree) (rfind-if fn (cdr tree))))))


(defun ttrav (rec &optional (base #'identity))
  (labels ((self (tree)
             (if (atom tree)
                 (if (functionp base)
                     (funcall base tree)
                     base)
                 (funcall rec (self (car tree))
                          (if (cdr tree)
                              (self (cdr tree)))))))
    #'self))

;; our-copy-tree
(ttrav #'cons #'identity)
;; count-leaves
(ttrav #'(lambda (l r) (+ l (or r 1))) 1)
;; flatten
(ttrav #'nconc #'mklist)

(defun trec (rec &optional (base #'identity))
  (labels ((self (tree)
             (if (atom tree)
                 (if (functionp base)
                     (funcall base tree)
                     base)
                 (funcall rec tree
                          #'(lambda ()
                              (self (car tree)))
                          #'(lambda ()
                              (if (cdr tree)
                                  (self (cdr tree))))))))
    #'self))

;; flatten
(trec #'(lambda (o l r) (nconc (funcall l) (funcall r)))
      #'mklist)

;; rfind-if
(trec #'(lambda (o l r) (or (funcall l) (funcall r)))
      #'(lambda (tree) (and (oddp tree) tree)))
