(in-package :advent-of-code.day8)

;; input
(defun read-file-as-string (file-name)
  (with-open-file (stream file-name :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun grid-to-2d-array (grid-string)
  (let* ((lines (split-sequence:split-sequence #\Newline grid-string))
         (rows (length lines))
         (cols (length (first lines)))
         (array (make-array (list rows cols) :initial-element nil)))
    (loop for i from 0 below rows
          for line in lines
          do (loop for j from 0 below cols
                   for char across line
                   do (setf (aref array i j) char)))
    array))

(defun parse-grid-to-array (file-name)
  (let ((grid-string (read-file-as-string file-name)))
    (grid-to-2d-array grid-string)))

;; grid
(defparameter *puzzle-input* #p"./day8/input.txt")
(defparameter *grid* (parse-grid-to-array *puzzle-input*))
(defparameter *blank* #\.)
(defparameter *rows* (array-dimension *grid* 0))
(defparameter *cols* (array-dimension *grid* 0))

(defun out-of-bounds (point)
  (let ((x (car point))
        (y (cdr point)))
    (or (< x 0) (< y 0) (>= x *rows*) (>= y *cols*))))

(defun get-unique-chars (grid)
  (let ((chars (make-hash-table)))
    (loop for i from 0 below (array-dimension grid 0)
          do (loop for j from 0 below (array-dimension grid 1)
                   do (setf (gethash (aref grid i j) chars) t)))
    (loop for char being the hash-keys of chars
            when (not (char= char *blank*))
          collect char)))

(defun get-antinodes (p1 p2)
  (let ((x1 (car p1))
        (y1 (cdr p1))
        (x2 (car p2))
        (y2 (cdr p2)))
    (let ((x-diff (- x2 x1))
          (y-diff (- y2 y1)))
      (remove-if #'out-of-bounds
          (list (cons (- x1 x-diff)
                      (- y1 y-diff))
                (cons (+ x2 x-diff)
                      (+ y2 y-diff)))))))

(defun get-char-positions (grid char)
  (loop for i from 0 below (array-dimension grid 0)
          append (loop for j from 0 below (array-dimension grid 1)
                         when (char= char (aref grid i j))
                       collect (cons i j))))

(defparameter *all-points*
              (mapcar (lambda (c)
                        (cons c (get-char-positions *grid* c)))
                  (get-unique-chars *grid*)))

(defun position-list (node)
  (rest node))

(defun get-all-pairs (points)
  (loop for i from 0 below (length points)
        for p1 = (nth i points)
          nconc (loop for j from (1+ i) below (length points)
                      for p2 = (nth j points)
                      collect (cons p1 (list p2)))))

(defun collect-antinodes (node-pairs)
  (mapcan #'(lambda (p)
              (mapcan #'(lambda (q) (get-antinodes (car q) (cadr q))) p))
    node-pairs))

(defun solution-1 ()
  (let* ((unique-points (make-hash-table :test #'equal))
         (all-coordinates (mapcar #'position-list *all-points*))
         (all-pairs (mapcar #'get-all-pairs all-coordinates))
         (all-antinodes (collect-antinodes all-pairs)))
    (loop for p in all-antinodes do (setf (gethash p unique-points) t))
    (format t "There are ~a antinodes~%" (hash-table-count unique-points))))
