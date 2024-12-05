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

;; Helpers
(defun in-correct-position (page pages-before)
  (let ((page-rules (gethash page *rules*)))
    (null (intersection page-rules pages-before))))

(defun get-middle-element (list)
  (nth (floor (length list) 2) list))

(defun sum-middle-elements (list)
  (apply #'+ (mapcar #'get-middle-element list)))

(defun insert-at-idx (list element idx)
  (append (subseq list 0 idx)
    (list element)
    (subseq list idx)))

;; PART 1
(defun get-correct-pages ()
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
    correct-pages))

(defun solution-1 ()
  (let ((correct-pages (get-correct-pages)))
    (sum-middle-elements correct-pages)))

;; PART 2
(defun violates-rule (current-page before-page)
  (let ((page-rules (gethash current-page *rules*)))
    (and page-rules (member before-page page-rules))))

(defun correct-page (page)
  (let ((corrected-page '()))
    (dolist (current-page page)
      (let ((inserted nil))
        (loop for pos from 0 upto (length corrected-page)
              do (let ((pages-before (subseq corrected-page 0 pos))
                       (pages-after (subseq corrected-page pos)))
                   (if (and (every #'(lambda (before-page)
                                       (in-correct-position current-page (list before-page)))
                              pages-before)
                            (every #'(lambda (after-page)
                                       (in-correct-position after-page (list current-page)))
                              pages-after))
                       (progn
                        (setf corrected-page
                          (append pages-before (list current-page) pages-after))
                        (setf inserted t)
                        (return)))))
        (unless inserted
          (push current-page corrected-page))))
    (nreverse corrected-page)))

(defun solution-2 ()
  (let ((corrected-pages (mapcar #'correct-page (set-difference *pages* (get-correct-pages)))))
    (sum-middle-elements corrected-pages)))
