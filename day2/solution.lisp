(ql:quickload "cl-ppcre")

(defun collect-lines (file-name)
  (with-open-file (stream file-name :direction :input)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun split-list (list)
  (cl-ppcre:split "\\s+" list))

;; part 1
(defun parse-puzzle-input (file-name)
  (mapcar #'str-list->int-list (mapcar #'split-list (collect-lines file-name))))

(defun all-decreasing (list)
  (loop for i from 0 to (- (length list) 2)
          always (> (nth i list) (nth (1+ i) list))))

(defun all-increasing (list)
  (loop for i from 0 to (- (length list) 2)
          always (< (nth i list) (nth (1+ i) list))))

(defun within-range (list)
  (if (null (rest list))
      t
      (let* ((x (first list))
             (y (second list))
             (diff (abs (- x y))))
        (and (>= diff 1) (<= diff 3) (within-range (rest list))))))


(defun str-list->int-list (list)
  (mapcar #'parse-integer list))

(defun safe-row (row)
  (and (or (all-increasing row) (all-decreasing row))
       (within-range row)))

(defun solution-1 ()
  (let* ((rows (parse-puzzle-input "input.txt"))
         (safety-status (mapcar #'(lambda (row)
                                    (if (safe-row row)
                                        1
                                        0))
                          rows)))
    (apply #'+ safety-status)))

;; part 2
(defun list-remove-1 (list)
  (loop for i from 0 to (1- (length list))
        collect (append (subseq list 0 i) (subseq list (1+ i)))))

(defun dampener-success-p (rows)
  (some #'safe-row rows))

(defun solution-2 ()
  (let* ((rows (parse-puzzle-input "input.txt"))
         (safety-status (mapcar #'(lambda (row)
                                    (let ((dampened-rows (list-remove-1 row)))
                                      (if (dampener-success-p dampened-rows)
                                          1
                                          0)))
                          rows)))
    (apply #'+ safety-status)))

