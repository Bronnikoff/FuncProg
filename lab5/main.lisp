;;;; Функции и объявления классов, которые я взял с сайта ystok
;;;; слово var заменено на vars для предотвращения ошибки компиляции

(defun order (term) (first term))   ; степень
(defun coeff (term) (second term)) ; коэффицент

;; инкапсулированное создание терма
(defun make-term (&key order coeff)
  (list order coeff))

;;; полином - список термов, где терм - список из коэффицента и степени
(defclass polynom ()
 ((vars :initarg :vars :reader vars)
  ;; Разреженный список термов в порядке убывания степени
  (terms :initarg :terms :reader terms)))

;;; Обобщенная функция сложения 
(defgeneric add2 (arg1 arg2)
 (:method ((n1 number) (n2 number))
  (+ n1 n2)))

;;; проверка на 0
(defgeneric zerop1 (arg)
 (:method ((n number))   ; (= n 0)
  (zerop n)))

;;; проверка на отрицательность
(defgeneric minusp1 (arg)
 (:method ((n number))   ; (< n 0)
  (minusp n)))

;;; печать многочлена
(defmethod print-object ((p polynom) stream)
  (format stream "[Pnm (~s) ~:{~:[~:[+~;-~]~d~[~2*~;~s~*~:;~s^~d~]~;~]~}]"
          (vars p)
          (mapcar (lambda (term)
                    (list (zerop1 (coeff term))
                          (minusp1 (coeff term))
                          (if (minusp1 (coeff term))
                              (abs (coeff term))
                              (coeff term))
                          (order term)
                          (vars p)
                          (order term)))
                  (terms p))))



;;;; остальные методы многочлена с сайта ystok я добавлять не стал 
;;;; при этом тк термы должны быть отсортированны по порядку степеней,
;;;; добавим сортировку при инициализации объекта класса:

;;; функция сортировки Хоара:
(defun quicksort (terms) (
    if (null terms) nil
        (let* 
            ((x (car terms)) 
                (r (cdr terms)) 
                ;; сортируем по убыванию
                (fn (lambda (a) (> (order a) (order x)))))
            (append 
                (quicksort (remove-if-not fn r)) 
                (list x)
                (quicksort (remove-if fn r))))))

;;; сортируем переданне термы объекта при инициализации
(defmethod initialize-instance :after ((p polynom) &key)
    (setf (slot-value p 'terms) (quicksort (slot-value p 'terms))))



;;;; Функция по заданию:

;;; функция сложения числа к терму (0 smthng), если он существует
;;; или создания нового, если он не существует(список термов отсорчен)
(defun add-num (terms num)
    (if (= num 0)
        ;; если число - 0, ничего делать не надо
        terms
        ;; иначе ищем:
        (if terms
            ;; выполняем поиск пока coeff > 0
            (if (> (order (first terms)) 0)
                (cons 
                    (first terms) 
                    (add-num (rest terms) num))
                ;; если мы дошли до значения >= 0, дальше поиск бессмысленен
                (if (= (order (first terms)) 0)
                    (cons 
                        (make-term :order 0 :coeff (+ num (coeff (first terms))))
                        (rest terms))
                    (cons
                        (make-term :order 0 :coeff num)
                        terms)))
            ;; если значение не нашли, добавляем в конец новый терм
            (list (make-term :order 0 :coeff num)))))

;;; функция принимает полином и число, после чего создает полином
;;; в котором изменен список nермов функцией add-num
(defmethod add2 ((p polynom) (n number))
    (make-instance 'polynom
        :vars (vars p)
        :terms (add-num (terms p) n)))

;;; сложение коммутативно
(defmethod add2 ((n number) (p polynom)) (add2 p n))




