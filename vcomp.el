;;; vcomp.el --- compare version strings

;; Copyright (C) 2008, 2009  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20081202
;; Updated: 20090417
;; Version: 0.0.3
;; Homepage: https://github.com/tarsius/vcomp
;; Keywords: versions

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Compare version strings.

;; This supports version strings like for example "0.11a_rc3-r1".

;; This is in part based on code in library `package.el' which is:
;; Copyright (C) 2007, 2008 Tom Tromey <tromey@redhat.com>

;; Note: You shouldn't use this library yet - it has to be polished first.
;; Note: I have just learned that such a library already existed before I
;;       created this: `versions.el'.  Haven't checked it out yet.

;; TODO: Properly define what kinds of version string are supported.
;; TODO: Support chaining alpha etc.  Which combinations make sense?
;; TODO: Do not require "_" before "alpha".  Good idea?

;;; Code:

(defconst vcomp--regexp
  "^\\(^[0-9]+\\(\\.[0-9]+\\)*\\)\\([a-z]\\)?\
\\(_\\(alpha\\|beta\\|pre\\|rc\\|p\\)\\([0-9]+\\)?\\)?\\(-r\\([0-9]+\\)\\)?$")

(defun vcomp-version-p (version)
  "Return t if VERSION is a valid version string."
  (when (string-match-p vcomp--regexp version) t))

(defun vcomp--intern (version)
  "Convert version string VERSION to a list of integers."
  ;; Don't use vcomp-version-p here as it doesn't change match data.
  (if (string-match vcomp--regexp version)
      (let ((num (mapcar #'string-to-int
			 (split-string (match-string 1 version) "\\.")))
	    (alp (match-string 3 version))
	    (tag (match-string 5 version))
	    (tnm (string-to-number (or (match-string 6 version) "0")))
	    (rev (string-to-number (or (match-string 8 version) "0"))))
	(list num (nconc (cond ((equal tag "alpha")
				(list  100 tnm))
			       ((equal tag "beta")
				(list  101 tnm))
			       ((equal tag "pre")
				(list  102 tnm))
			       ((equal tag "rc")
				(list  103 tnm))
			       ((equal tag nil)
				(list  104 tnm))
			       ((equal tag "p")
				(list  105 tnm)))
			 (list (if alp (string-to-char alp) 96))
			 (list rev))))
    (error "%S isn't a valid version string" version)))

(defun vcomp-compare (v1 v2 pred)
  "Compare version strings V1 and V2 using PRED."
  (setq v1 (vcomp--intern v1))
  (setq v2 (vcomp--intern v2))
  (let ((l1 (length (car v1)))
	(l2 (length (car v2))))
    (cond ((> l1 l2)
	   (nconc (car v2) (make-list (- l1 l2) -1)))
	  ((> l2 l1)
	   (nconc (car v1) (make-list (- l2 l1) -1)))))
  (setq v1 (nconc (car v1) (cadr v1))
	v2 (nconc (car v2) (cadr v2)))
  (while (and v1 v2 (= (car v1) (car v2)))
    (setq v1 (cdr v1)
	  v2 (cdr v2)))
  (if v1
      (if v2
	  (funcall pred (car v1) (car v2))
	(funcall pred v1 -1))
    (if v2
	(funcall pred -1 v2)
      (funcall pred 0 0))))

(defun vcomp-max (version &rest versions)
  "Return largest of all the arguments (which must be version strings)."
  (dolist (elt versions)
    (when (vcomp-compare elt version '>)
      (setq version elt)))
  version)

(defun vcomp-min (version &rest versions)
  "Return smallest of all the arguments (which must be version strings)."
  (dolist (elt versions)
    (when (vcomp-compare elt version '<)
      (setq version elt)))
  version)

(defun vcomp< (v1 v2)
  "Return t if first version string smaller than second."
  (vcomp-compare v1 v2 '<))

(provide 'vcomp)
;;; vcomp.el ends here
