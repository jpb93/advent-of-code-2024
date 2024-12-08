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

(defun has-solution-p (current remaining total)
  (if (null remaining)
      (equal current total)
      (progn
       (let ((next-term (first remaining))
             (rest-terms (rest remaining)))
         (or
          (has-solution-p (+ current next-term) rest-terms total)
          (has-solution-p (* current next-term) rest-terms total)
          (has-solution-p (concat-integers current next-term) rest-terms total))))))

(defun check-line (line)
  (let ((terms (second line))
        (total (first line)))
    (has-solution-p (first terms) (rest terms) total)))

(defun solution ()
  (let* ((parsed-input (parse-input-file *puzzle-input*))
         (safe-lines (remove-if-not #'check-line parsed-input)))
    (apply #'+ (mapcar #'car safe-lines))))

;; PART 2
(defun concat-integers (&rest integers)
  (parse-integer (apply #'concatenate 'string (mapcar #'write-to-string integers))))
