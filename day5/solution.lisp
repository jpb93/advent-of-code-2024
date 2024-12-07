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
  (let ((page-rules (make-hash-table :test #'equal)))
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
(defun all-nodes ()
  (loop for key being the hash-keys of *rules*
        collect key))

(defun compute-in-degrees ()
  (let* ((nodes (all-nodes))
         (flattened-neighbors (mapcan #'(lambda (x) (gethash x *rules*)) nodes))
         (in-degree (make-hash-table :test #'equal)))
      (dolist (node nodes)
        (setf (gethash node in-degree) (count node flattened-neighbors)))
        in-degree))

(defun print-hash-table (hash-table)
  (maphash (lambda (k v)
             (format t "~a -> ~a~%" k v))
           hash-table)
           
           'done)

(let ((keys (list 66 14 95 31 32 13 52 21 34 48 75 59 57 87 73 89 42 22 82 67 77 92 79 84
                  64 19 98 94 47 54 91 46 76 68 72 85 25 37 36 78 74 63 93 81 17 38 29 49 44))
      (values-acc (make-hash-table :test #'equal)))
  (dolist (k keys)
    (dolist (v (gethash k *rules*))
      (setf (gethash v values-acc) t)))
  (dolist (k keys)
    ; (print k)
    (unless (gethash k values-acc)
      (format t "Key ~A never appears as a value.~%" k))))