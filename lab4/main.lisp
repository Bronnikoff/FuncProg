;;; Символы, по которым будет проводитя разбиение слов
(defun split-char-p (char)
  (member char '(#\Space #\Tab #\Newline 
                #\, #\. #\- #\; #\? #\!
                #\: #\' #\")))

(defun word-list-string (string)
  ;; Разбить строки на слова, разделённые знаками whitespace
  ;; A la (split-seq-if #'whitespace-char-p string)
  (loop with len = (length string)
        for left = 0 then (1+ right)
        for right = (or (position-if #'split-char-p string
                                     :start left)
                        len)
        unless (= right left)	; исключить пустые слова
          collect (subseq string left right)
        while (< right len)))

;;; возвращает список уникальных строк-элементов двух списков
(defun unique-concat (set1 set2) 
    (if (null set1) 
        set2 
        (if (member (car set1) set2 :test #'string=)
            (unique-concat (cdr set1) set2)
            (unique-concat (cdr set1) (cons (car set1) set2)))))

;;; разбивает текст на слова и возвращает список уникальных
(defun word-list-text (text)
    (if text
        (unique-concat 
            (word-list-string (car text))
            (word-list-text (cdr text)))))


;;; функция поиска максимальной длины слова и списка слов
(defun max-len-search (wset waslen)
    (if wset
        (if (> (length (car wset)) waslen)
            (max-len-search (cdr wset) (length (car wset)))
            (max-len-search (cdr wset) waslen))
        waslen))

;;; обертка для функции, вызывающая основную с параметром 0
(defun max-len (wset) (max-len-search wset 0))

;;; функция поиска слов заданной длины в списке слов
(defun words-by-len (wset len)
    (if wset
        (if (= (length (car wset)) len)
            (cons (car wset) 
                (words-by-len (cdr wset) len))
            (words-by-len (cdr wset) len))))


;;; Основная функция, требуемая по заданию
;;; Берет список уникальных слов текста и возвращает слова,
;;; длина которых равна максиальной длине слова из списка
(defun text-longest-words (text)
    (let ((wset (word-list-text text)))
        (words-by-len wset (max-len wset))))