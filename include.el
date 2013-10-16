;;Copyright 2013 James Atwood
;;
;;Licensed under the Apache License, Version 2.0 (the "License");
;;you may not use this file except in compliance with the License.
;;You may obtain a copy of the License at
;;
;;    http://www.apache.org/licenses/LICENSE-2.0
;;
;;Unless required by applicable law or agreed to in writing, software
;;distributed under the License is distributed on an "AS IS" BASIS,
;;WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;See the License for the specific language governing permissions and
;;limitations under the License.
;;
;;
;; include.el adds a simple dependency management for chunks of elisp,
;; which I use to manage my emacs config file.  Chunks of elisp are
;; included via (include 'name), which activates any code in an
;; (if-included 'name body) block.  include.el includes dependency
;; management between chunks via a keyword argument to include:
;; (include 'name :depends 'dependence-name).  (eval-included)
;; computes dependencies then evalutates the elisp chunks in the
;; proper order.
;; 

(require 'cl)

(defvar *included* nil)
(defvar *included-dependencies* (make-hash-table :test 'eq))
(defvar *included-elisp* (make-hash-table :test 'eq))

(defun nuke-includes ()
  (setf *included* nil)
  (setf *included-dependencies* (make-hash-table :test 'eq))
  (setf *included-elisp* (make-hash-table :test 'eq)))

(defun include (name &rest dependencies)
  (if (not (null dependencies)) (pop dependencies))  ;remove :depends keyword
  (setf (gethash name *included-dependencies*) dependencies)
  (push name *included*))

(defun list-contains (list thing)
  (cond
   ((null list) nil)
   ((listp list) (if (eq thing (car list)) t (list-contains (cdr list) thing)))
   (t nil)))

(defun included-p (name)
  (list-contains *included* name))

;;(defmacro if-included (name &rest body)
;;  `(if (included-p ,name) (progn ,@body)))

(defmacro if-included (name &rest body)
  `(if (included-p ,name)
       (setf (gethash ,name *included-elisp*)
	     (append (gethash ,name *included-elisp*) (quote ,body)))))

(lexical-let ((time 0))
  (defun dfs-included ()
    (setf time 0)
    (dolist (incl *included*)
      (put incl 'color 'white)
      (put incl 'predecessor nil))
    (dolist (incl *included*)
      (if (eq (get incl 'color) 'white)
	  (dfs-visit-included incl))))

  (defun dfs-visit-included (incl)
    (incf time) 
    (put incl 'discovered time)
    (put incl 'color 'gray)
    (dolist (dep (gethash incl *included-dependencies*))
      (if (eq (get dep 'color) 'white)
	  (progn
	    (put dep 'predecessor incl)
	    (dfs-visit-included dep))))
    (put incl 'color 'black)
    (incf time)
    (put incl 'finished time)))

(defun order-includes ()
  (dfs-included)
  (setf *included*
	(sort *included*
	      (lambda (x y)
		(< (get x 'finished) (get y 'finished))))))

(defun eval-included ()
  (order-includes)
  (dolist (incl *included*)
    (eval `(progn ,@(gethash incl *included-elisp*)))))
(add-hook 'after-init-hook 'eval-included)


(defun view-include (incl)
  (interactive "SView include: ")
  (if (not (included-p incl))
      (message "%s not included"
	       incl)
    (save-excursion
      (beginning-of-buffer)
      (let* ((pt (point))
	     (form (read (current-buffer))))
	(while form
	  (if (not (and
		    (not (null form))
		    (listp form)
		    (or (eq (car form) 'include)
			(eq (car form) 'if-included))
		    (not (null (cdr form)))
		    (not (null (cdr (cadr form))))
		    (eq (cadr (cadr form)) incl)))
	      (let ((new-overlay (make-overlay pt (point))))
		(overlay-put new-overlay 'invisible t)))
	  (setq pt (point))
	  (if (null (save-excursion (search-forward "(" nil t)))
	      (setq form nil)
	    (setq form (read (current-buffer)))))))))
  

(defun unview-include ()
  (interactive)
  (remove-overlays))


(defun form-to-string (form)
  (prin1-to-string form))

(defun package-include (incl)
  (interactive "SPackage include: ")
  (if (not (included-p incl))
      (message "%s not included"
	       incl)
    (let ((package-buffer (create-file-buffer (concat (symbol-name incl) ".el"))))
      (save-excursion 
	(set-buffer package-buffer)
	(emacs-lisp-mode)
	(mapc (lambda (form)
		(insert (pp form))
		(newline))
	      (gethash incl *included-elisp*))
	(newline) 
	(insert (format "(provide '%S)" incl))
	(newline)
	(insert (format ";; %S.el ends here" incl)))
      (switch-to-buffer-other-window package-buffer))))
		  
;;(global-set-key (kbd "C-c m i") 'view-include)
;;(global-set-key (kbd "C-c m u") 'unview-include)


(provide 'include)
;;; include.el ends here
