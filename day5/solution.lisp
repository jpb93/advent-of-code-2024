(in-package :advent-of-code.day5)

(ql:quickload "split-sequence")

(defparameter *page-rules-input* #p"./day5/input1.txt")
(defparameter *page-input* #p"./day5/input2.txt")

(defun collect-lines (file-name)
  (with-open-file (stream file-name :direction :input)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun create-rule-table (lines)
  (let ((page-rules (make-hash-table)))
    (loop for line in lines
          for (page-num-str rule-str) = (split-sequence:split-sequence #\| line)
          for page-num = (parse-integer page-num-str)
          for rule = (parse-integer rule-str)
          do (push rule (gethash page-num page-rules)))
    page-rules))

(defun pages->list (line)
  (mapcar #'parse-integer (split-sequence:split-sequence #\, line)))

(defparameter *rules* (create-rule-table (collect-lines *page-rules-input*)))
(defparameter *pages* (mapcar #'pages->list (collect-lines *page-input*)))

(defun in-correct-position (page pages-before)
  (let ((page-rules (gethash page *rules*)))
    (null (intersection page-rules pages-before))))

(defun get-middle-element (list)
  (nth (floor (length list) 2) list))

(defun solution-1 ()
  (let ((correct-pages '()))
    (dolist (page-update *pages*)
      (let ((correct-position t)
            (pages-before '()))
        (dolist (current-page page-update)
          (unless (in-correct-position current-page pages-before)
            (setf correct-position nil)
            (return))
          (push current-page pages-before))
        (when correct-position
              (push page-update correct-pages))))
    (apply #'+ (mapcar #'get-middle-element correct-pages))))
