(in-package :advent-of-code.day7)

(defparameter *puzzle-input* #p"./day7/input.txt")

(defun parse-line (line)
  (let* ((parts (split-sequence:split-sequence #\: line))
         (total-str (string-trim " " (first parts)))
         (terms-str (string-trim " " (second parts)))
         (total (parse-integer total-str :junk-allowed t))
         (terms (mapcar (lambda (s)
                          (parse-integer s :junk-allowed t))
                    (split-sequence:split-sequence #\Space terms-str))))
    (cons total (list terms))))

(defun parse-puzzle-input (lines)
  (mapcar #'parse-line lines))

(defun read-lines-from-file (filename)
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun parse-input-file (filename)
  (let ((lines (read-lines-from-file filename)))
    (parse-puzzle-input lines)))

(defun result-evals-p (total terms &optional (current nil) (started nil))
  (cond ((null terms)
          (if started
              (equal current total)
              (equal total 0)))
        ((not started)
          (result-evals-p total (rest terms) (first terms) t))
        (t
          (or (result-evals-p total (rest terms) (+ current (first terms)) t)
              ;; PART 2
              (result-evals-p total (rest terms) (concat-integers current (first terms)) t)
              ;;
              (and (not (zerop (first terms)))
                   (result-evals-p total (rest terms) (* current (first terms)) t))))))

(defun solution ()
  (let* ((parsed-input (parse-input-file *puzzle-input*))
         (safe-lines (remove-if-not #'(lambda (line) (result-evals-p (first line) (second line))) parsed-input)))
    (apply #'+ (mapcar #'car safe-lines))))


;; PART 2
(defun concat-integers (&rest integers)
  (parse-integer (apply #'concatenate 'string (mapcar #'write-to-string integers))))

