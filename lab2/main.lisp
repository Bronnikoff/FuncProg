;;; define function of concatination 2 list in 1 
(defun concat (set1 set2) 
    (if (null set1) set2 (cons (car set1) (concat (cdr set1) set2))))

;;; function append element in head of all list in list
(defun attach (elem set)
    (if (null set) nil
    (cons (cons elem (car set)) (attach elem (cdr set)))))

;;; main function
(defun subsets (set)
    ;; if set is empty => return (())
    (if (null set) '(nil)
    ;; else concat
    (concat 
    ;; subset of tail
    (subsets (cdr set))
    ;; and subset of tail with head
    (attach (car set) (subsets (cdr set))))))
