
;;;; array-dimensions returns list (first second)

;;; func retuns first size of matrix
(defun first_size (A)
    (car (array-dimensions A)))

;;; func return secoond size of matrix
(defun second_size (A)
    (car (cdr (array-dimensions A))))

;;; if matrix size with 0 => return any value(0 for example) 
;;; else search min, start value - A[0][0]
(defun search_min (A)
    (if (or (< (first_size A) 1) (< (second_size A) 1)) 
        0
        (let ((ans (aref A 0 0)))
            (dotimes (i (first_size A)) 
                (dotimes (j (second_size A))
                    (if (< (aref A i j) ans) 
                        (setf ans (aref A i j)))))
            ans)))

;;; fill ans by requirement, m - minimum in A, r - new value
(defun fill_matrix (ans A m r) 
    (dotimes (i (first_size A)) 
        (dotimes (j (second_size A))
            (if (> (aref A i j) m) 
                (setf (aref ans i j) (aref A i j))
                (setf (aref ans i j) r))))
    ans)

;;; create matrix, search min, fill this mtrix and return
(defun change_min_matrix_to_num (A r)
    (let ((ans (make-array (array-dimensions A))))
        (fill_matrix ans A (search_min A) r)))