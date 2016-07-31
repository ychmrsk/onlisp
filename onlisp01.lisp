(do* ((x 1 (1+ x))
      (result (list x) (push x result)))
     ((= x 10) (nreverse result)))
;; (1 2 3 4 5 6 7 8 9 10)

(mapcar fn
        (do* ((x 1 (1+ x))
              (result (list x) (push x result)))
             ((= x 10) (nreverse result))))

(map1-n fn 10)  ;; p54


(do ((x a (+ 1 x)))
    ((> x b))
  (print x))

(for (x a b)  ;; p154
     (print x))


