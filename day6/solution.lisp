(in-package :advent-of-code.day6)

;; INPUT
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
                   do (setf (aref array i j) char)))
    array))

(defun parse-grid-to-array (file-name)
  (let ((grid-string (read-file-as-string file-name)))
    (grid-to-2d-array grid-string)))

;; model
(deftype direction ()
  '(member :north :south :east :west))

(defstruct guard
  (x 0 :type integer)
  (y 0 :type integer)
  (direction :north :type direction))

(defun pos->coord (guard)
  (cons (guard-x guard) (guard-y guard)))

(defun set-guard-pos (guard pos)
  (setf (guard-x guard) (car pos)
    (guard-y guard) (cdr pos)))

(defparameter *input* #p"./day6/input.txt")

(defparameter *open-space* #\.)
(defparameter *obstacle* #\#)
(defparameter *guard-start* #\^)

(defparameter *guard* (make-guard))
(defparameter *grid* (parse-grid-to-array *input*))

(defun find-guard ()
  (loop for i from 0 below (array-dimension *grid* 0)
        do (loop for j from 0 below (array-dimension *grid* 1)
                 do (when (equal (aref *grid* i j) *guard-start*)
                          (set-guard-pos *guard* (cons i j))
                          (print (cons i j))
                          (return)))))

(defun move-guard (guard)
  (case (guard-direction guard)
    (:north (decf (guard-x guard)))
    (:south (incf (guard-x guard)))
    (:east (incf (guard-y guard)))
    (:west (decf (guard-y guard)))))

(defun turn-guard (guard)
  (setf (guard-direction guard)
    (case (guard-direction guard)
      (:north :east)
      (:east :south)
      (:south :west)
      (:west :north)
      (otherwise (error "Invalid direction")))))

(defun look-ahead (grid current-position direction)
  (destructuring-bind (x . y) current-position
    (let ((nx (case direction
                (:north (1- x))
                (:south (1+ x))
                (:east x)
                (:west x)))
          (ny (case direction
                (:north y)
                (:south y)
                (:east (1+ y))
                (:west (1- y)))))
      (if (or (< nx 0) (< ny 0)
              (>= nx (array-dimension grid 0))
              (>= ny (array-dimension grid 1)))
          nil
          (aref grid nx ny)))))

(defun next-pos-open (grid current-position direction)
  (let ((ch (look-ahead grid current-position direction)))
    (or (not ch)
        (not (char= ch *obstacle*)))))

(defun guard-out-of-bounds (grid guard)
  (let ((x (guard-x guard))
        (y (guard-y guard)))
    (or (< x 0)
        (< y 0)
        (>= x (array-dimension grid 0))
        (>= y (array-dimension grid 1)))))

(defun compute-next-move (grid guard)
  (let ((pos (pos->coord guard))
        (dir (guard-direction guard)))
    (if (next-pos-open grid pos dir)
        (move-guard guard)
        (turn-guard guard))))

(defparameter *visited-locations* (make-hash-table :test #'equal))

(defun add-current-location (guard)
  (setf (gethash (pos->coord guard) *visited-locations*) t))

(defun count-visited-locations ()
  (hash-table-count *visited-locations*))

(defun solution-1 ()
  (find-guard)
  (loop while (not (guard-out-of-bounds *grid* *guard*))
        do (progn
            (add-current-location *guard*)
            (compute-next-move *grid* *guard*)))
  (format t "Visited locations: ~a" (count-visited-locations))
  (setf *visited-locations* nil))
