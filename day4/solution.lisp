(in-package :advent-of-code.day4)

(ql:quickload "split-sequence")

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
                   do (setf (aref array i j) (intern (string char)))))
    array))

(defun parse-grid-to-array (file-name)
  (let ((grid-string (read-file-as-string file-name)))
    (grid-to-2d-array grid-string)))


(defparameter *puzzle-input* #p"./day4/input.txt")
(defparameter *sample-input* #p"./day4/sample.txt")

(defparameter *grid* (parse-grid-to-array *puzzle-input*))

(defparameter *rows* (array-dimension *grid* 0))
(defparameter *cols* (array-dimension *grid* 1))

;; PART 1

(defun valid-index (i j)
  (and (>= i 0) (>= j 0) (< i *rows*) (< j *cols*)))

(defmacro define-check-function (name delta-i delta-j)
  `(defun ,name (i j)
     (when (valid-index (+ i (* ,delta-i 3)) (+ j (* ,delta-j 3)))
           (let ((result (list
                           (aref *grid* i j)
                           (aref *grid* (+ i (* ,delta-i 1)) (+ j (* ,delta-j 1)))
                           (aref *grid* (+ i (* ,delta-i 2)) (+ j (* ,delta-j 2)))
                           (aref *grid* (+ i (* ,delta-i 3)) (+ j (* ,delta-j 3))))))
             (if (equal result '(X M A S)) 1 0)))))

(define-check-function check-right 0 1)
(define-check-function check-left 0 -1)
(define-check-function check-down 1 0)
(define-check-function check-up -1 0)
(define-check-function check-up-right -1 1)
(define-check-function check-up-left -1 -1)
(define-check-function check-down-right 1 1)
(define-check-function check-down-left 1 -1)

(defun solution-1 ()
  (let ((count 0))
    (destructuring-bind (n m) (array-dimensions *grid*)
      (loop for i from 0 below n do
        (loop for j from 0 below m do
          (when (equal (aref *grid* i j) 'X)
            (let ((result (+ (or (check-right i j) 0)
                             (or (check-left i j) 0)
                             (or (check-down i j) 0)
                             (or (check-up i j) 0)
                             (or (check-up-right i j) 0)
                             (or (check-up-left i j) 0)
                             (or (check-down-right i j) 0)
                             (or (check-down-left i j) 0))))
                  (setf count (+ count result)))))))
    count))

;; PART 2

(defmacro define-check-pair-function (name positions)
  `(defun ,name (i j char)
     (when (and
            ,@(mapcar (lambda (pos) (list 'valid-index
                                (list '+ 'i (car pos))
                                (list '+ 'j (cadr pos))))
                  positions)
            ,@(mapcar (lambda (pos) (list 'equal
                                (list 'aref '*grid*
                                (list '+ 'i (car pos))
                                (list '+ 'j (cadr pos)))
                              'char))
                  positions))
           1)))

(define-check-pair-function check-NW-NE ((-1 -1) (-1 1)))
(define-check-pair-function check-NE-SE ((-1 1) (1 1)))
(define-check-pair-function check-SE-SW ((1 1) (1 -1)))
(define-check-pair-function check-SW-NW ((1 -1) (-1 -1)))

(defun solution-2 ()
  (let ((count 0))
    (destructuring-bind (n m) (array-dimensions *grid*)
      (loop for i from 0 below n do
        (loop for j from 0 below m do
          (when (equal (aref *grid* i j) 'A)
                (let ((result (+ (if (and (check-NW-NE i j 'M) (check-SE-SW i j 'S)) 1 0)
                                 (if (and (check-NE-SE i j 'M) (check-SW-NW i j 'S)) 1 0)
                                 (if (and (check-SE-SW i j 'M) (check-NW-NE i j 'S)) 1 0)
                                 (if (and (check-SW-NW i j 'M) (check-NE-SE i j 'S)) 1 0))))
                  (setf count (+ count result)))))))
    count))
