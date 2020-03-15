;;; retunrs a^2
(defun sqr(a) (* a a)) 

;;; i redefine function min there
(defun minimum(a b c) 
    (if (> a b)
        (if (> b c) c b)
        (if (> a c) c a)))

;;; result func
(defun square-sum-two-of-three(a b c) 
    (+ (sqr a) (sqr b) (sqr c) (- (sqr (minimum a b c)))))
