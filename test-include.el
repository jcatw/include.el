(defvar *test-dependencies*)
(setf *test-dependencies* nil)
(nuke-includes)
(include 'bob :depends 'sally 'mary)
(include 'sally)
(include 'mary)
(include 'bill :depends 'mary)
(include 'hola :depends 'mary 'sally)

(if-included 'bob (setf *test-dependencies* (cons 'bob *test-dependencies*)))
(if-included 'sally (setf *test-dependencies* (cons 'sally *test-dependencies*)))
(if-included 'mary (setf *test-dependencies* (cons 'mary *test-dependencies*)))
(if-included 'bill (setf *test-dependencies* (cons 'bill *test-dependencies*)))
(if-included 'hola (setf *test-dependencies* (cons 'hola *test-dependencies*)))

(eval-included)

(defun list-eq-p (l1 l2)
  (cond
   ((null l1) (if (null l2) t nil))
   ((null l2) nil)
   ((eq (car l1) (car l2)) (list-eq-p (cdr l1) (cdr l2)))
   (t nil)))

(list-eq-p '(1 2 3) '(4 5 6))
(list-eq-p '(1 2 3) '(1 2 3))

*test-dependencies*

(if (list-eq-p *included* (nreverse *test-dependencies*))
    (print "include.el tests passed")
  (print "include.el tests failed"))
(nuke-includes)
  
  
