#!/usr/local/bin/sbcl --script

(defun help ()
  (write-line
   "rot13.lisp [input string]

rot13 in common lisp.
For rot13 reference, see: https://en.wikipedia.org/wiki/ROT13
")
  (quit))


(defun rot13 (string)
  (let ((code (flet ((alist (start end)
                       (let* ((seq (loop for i from start to end
                                         collect (code-char i)))
                              (sub1 (subseq seq 0 13))
                              (sub2 (subseq seq 13)))
                         (append (pairlis sub1 sub2) (pairlis sub2 sub1)))))
                (append (alist (char-code #\A) (char-code #\Z))
                        (alist (char-code #\a) (char-code #\z))))))
    (map 'string
         (lambda (x)
           (let ((ret (cdr (assoc x code :test #'char=))))
             (if (null ret) x ret)))
         string)))


(defun main (args)
  (unless (= 2 (length args))
    (help))
  (let ((str (cadr args)))
    (write-line (rot13 str))
    (quit)))


(main *posix-argv*)
