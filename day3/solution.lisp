(in-package :advent-of-code.day3)

(defparameter *puzzle-input* #p"./day3/input.txt")

(defun read-file-as-string (file-name)
  (with-open-file (stream file-name :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

;; solution 1
(defun find-mul-expressions (string)
  (cl-ppcre:all-matches-as-strings "mul\\(\\d+,\\d+\\)" string))

(defun eval-mul-expression (expr)
  (let* ((lParenIdx (search "(" expr))
         (rParenIdx (search ")" expr))
         (commaIdx (search "," expr))
         (num1 (parse-integer (subseq expr (1+ lparenidx) commaIdx)))
         (num2 (parse-integer (subseq expr (1+ commaIdx) rParenIdx))))
    (* num1 num2)))

(defun solution-1 ()
  (let ((input (find-mul-expressions (read-file-as-string *puzzle-input*))))
    (apply #'+ (mapcar #'eval-mul-expression input))))

;; solution 2
(defun filter-disabled-blocks (string)
  (cl-ppcre:regex-replace-all "(?s)don't\\(\\)(.*?)(do\\(\\)|$)" string ""))

(defun solution-2 ()
  (let* ((input-string (read-file-as-string *puzzle-input*))
         (valid-blocks (filter-disabled-blocks input-string))
         (mul-expressions (find-mul-expressions valid-blocks))
         (results (mapcar #'eval-mul-expression mul-expressions)))
    (apply #'+ results)))
