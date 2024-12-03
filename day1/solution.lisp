;; Day 1 Part 1
(ql:quickload "cl-ppcre")

(defun collect-lines (file-name)
  (with-open-file (stream file-name :direction :input)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun transpose (matrix)
  (apply #'mapcar #'list matrix))

(defun split-list (list)
  (cl-ppcre:split "\\s+" list))

(defun sort-lists (lists)
  (let ((first-list (first lists))
        (second-list (second lists)))
    (list (sort first-list #'string<)
          (sort second-list #'string<))))

(defun pair-diff (pair)
  (let ((x (parse-integer (first pair)))
        (y (parse-integer (second pair))))
    (abs (- x y))))

(defun solution ()
  (let* ((list-items (collect-lines "input.txt"))
         (pairs (transpose (sort-lists (transpose (mapcar #'split-list list-items))))))
    (apply #'+ (mapcar #'pair-diff pairs))))

;; Day 1 Part 2
(defun solution-2 ()
  (let* ((list-items (collect-lines "input.txt"))
         (two-lists (transpose (mapcar #'split-list list-items)))
         (left-list (first two-lists))
         (right-list (second two-lists))
         (list-counts (mapcar #'(lambda (x)
                                  (let ((item-count (count x right-list :test #'string=)))
                                    (* (parse-integer x) item-count)))
                        left-list)))
    (apply #'+ list-counts)))

