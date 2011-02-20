;;; ob-lilypond.el --- org-babel functions for lilypond evaluation

;; Copyright (C) Shelagh Manton

;; Author: Shelagh Manton
;; Keywords: literate programming, weaving markup
;; Homepage: http://orgmode.org
;; Version: 0.01

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;
;;

;;
;; If you are planning on adding a language to org-babel we would ask
;; that if possible you fill out the FSF copyright assignment form
;; available at http://orgmode.org/request-assign-future.txt as this
;; will make it possible to include your language support in the core
;; of Org-mode, otherwise unassigned language support files can still
;; be included in the contrib/ directory of the Org-mode repository.

;;; Requirements:

;; You need to have a copy of LilyPond

;;; Code:
(require 'ob)
(require 'ob-eval)
(require 'lilypond-mode)
;; possibly require modes required for your language

;; optionally define a file extension for this language
(add-to-list 'org-babel-tangle-lang-exts '("lilypond" . "ly"))

;; optionally declare default header arguments for this language
(defvar org-babel-default-header-args:lilypond 
   '((:results . "file") (:exports . "results"))
  "Default arguments to use when evaluating a dot source block.")

;; This function expands the body of a source code block by doing
;; things like prepending argument definitions to the body, it should
;; be called by the `org-babel-execute:lilypond' function below.

(defun org-babel-expand-body:lilypond (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
    (let ((vars (nth 1 (or processed-params
			 (org-babel-process-params params)))))
    (mapc
     (lambda (pair)
       (let ((name (symbol-name (car pair)))
	     (value (cdr pair)))
	 (setq body
	       (replace-regexp-in-string
		(concat "\$" (regexp-quote name))
		(if (stringp value) value (format "%S" value))
		body))))
     vars)
    body))

;; This is the main function which is called to evaluate a code
;; block.
;;
;; This function will evaluate the body of the source code and
;; return the results as emacs-lisp depending on the value of the
;; :results header argument
;; - output means that the output to STDOUT will be captured and
;;   returned
;; - value means that the value of the last statement in the
;;   source code block will be returned
;;
;; The most common first step in this function is the expansion of the
;; PARAMS argument using `org-babel-process-params'.
;;
;; Please feel free to not implement options which aren't appropriate
;; for your language (e.g. not all languages support interactive
;; "session" evaluation).  Also you are free to define any new header
;; arguments which you feel may be useful -- all header arguments
;; specified by the user will be available in the PARAMS variable.

(defun org-babel-execute:lilypond (body params)
  "Execute a block of Lilypond code with org-babel.  This function is
called by `org-babel-execute-src-block'"
 ;; (message "executing Lilypond source code block")
  (let* ((processed-params (org-babel-process-params params))
         (result-params (split-string (or (cdr (assoc :results params)) "")))
	 (out-file (cdr (assoc :file params)))
	 (cmdline (or (cdr (assoc :cmdline params))
		     (format " -f%s" (file-name-extension out-file))))
    	 (cmd (or (cdr (assoc :cmd params)) "lilypond"))
    	 (in-file (org-babel-temp-file "lp-")))
    (with-temp-file in-file
      (insert (org-babel-expand-body:lilypond body params processed-params)))
    (org-babel-eval
     (concat cmd
	      " " cmdline
    	     " " (org-babel-process-file-name in-file)
	     " " (org-babel-process-file-name out-file)) "")
    out-file))

;; This is what we want it to look like
;; lilypond -fpng --preview $infile

;; This function should be used to assign any variables in params in
;; the context of the session environment.
(defun org-babel-prep-session:lilypond (session params)
  "Return an error because LilyPond does not support sessions."
  (error "LilyPond does not support sessions"))


(provide 'ob-lilypond)

;;; ob-lilypond.el ends here
