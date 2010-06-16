;;; vcomp.el --- compare version strings

;; Copyright (C) 2008, 2009, 2010  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20081202
;; Updated: 20100616
;; Version: 0.0.9+
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

;; Have a look at the regular expression `vcomp--regexp' to determine
;; what version string schemes are supported.  Note that the \"-rN\"
;; suffix should not be used by authors of libraries but rather is for
;; maintainers of packages (like ELPA or the Emacsmirror).

;; Some related (or alternative) libraries and functions:
;;
;; - `version<' and related functions defined in `subr.el' distributed
;;   with Emacs, or stand-alone as `versions.el'.
;; - `inversion' which is part of Cedet and therefor included in recent
;;   versions of Emacs.

;; This package is in part based on code from `package.el' which is:
;; Copyright (C) 2007, 2008 Tom Tromey <tromey@redhat.com>

;;; Code:

(require 'cl)

(defconst vcomp--regexp
  (concat "^\\("
	  "\\([0-9]+\\(?:[-_.][0-9]+\\)*\\)"
	  "\\([a-z]\\)?"
	  "\\(?:_?\\(alpha\\|beta\\|pre\\|rc\\|p\\)\\([0-9]+\\)?\\)?"
	  "\\(?:-r\\([0-9]+\\)\\)?"
	  "\\)$")
  "The regular expression used to compare version strings.")

(defun vcomp-version-p (version)
  "Return t if VERSION is a valid version string."
  (when (string-match-p vcomp--regexp version) t))

(defun vcomp--intern (version)
  "Convert version string VERSION to a list of integers."
  ;; Don't use vcomp-version-p here as it doesn't change match data.
  (if (string-match vcomp--regexp version)
      (let ((num (mapcar #'string-to-int
			 (save-match-data
			   (split-string (match-string 2 version) "[-_.]"))))
	    (alp (match-string 3 version))
	    (tag (match-string 4 version))
	    (tnm (string-to-number (or (match-string 5 version) "0")))
	    (rev (string-to-number (or (match-string 6 version) "0"))))
	(list num (nconc (list (if (not alp)
				   96
				 (setq alp (string-to-char alp))
				 (if (< alp 97)
				     (+ alp 32)
				   alp)))
			 (cond ((equal tag "alpha")
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
  "Return t if first version string is smaller than second."
  (vcomp-compare v1 v2 '<))

(defun vcomp-normalize (version)
  "Normalize VERSION which has to be a valid version string."
  (if (string-match vcomp--regexp version)
      (let ((num (match-string 2 version))
	    (alp (match-string 3 version))
	    (tag (match-string 4 version))
	    (tnm (match-string 5 version))
	    (rev (match-string 6 version)))
	(concat (save-match-data
		  (replace-regexp-in-string "[-_]" "." num))
		(when alp
		  (downcase alp))
		(when tag
		  (concat "_" (downcase tag)))
		(match-string 5 version)
		(when rev
		  (concat "-r" rev))))
    (error "%S isn't a valid version string" version)))

(defun vcomp-max-link (page pattern)
  "Return largest link matching PATTERN from the webpage PAGE.

PAGE should be a webpage containing links to versioned files matching
PATTERN.  If PATTERN contains \"%v\" then this is replaced with the value
of `vcomp--regexp' (sans the leading ^ and trailing $).  The result is
then used as part of a regular expression to find matching urls.  The
first sub-expression of _PATTERN_ has to match the version string which is
used for comparison.  The returned value is always a complete url even if
PATTERN is relativ to PAGE (which is necessary when urls on PAGE are
relative)."
  (setq pattern
	(replace-regexp-in-string "%v" (substring vcomp--regexp 1 -1)
				  pattern nil t))
  (let ((buffer (url-retrieve-synchronously page))
	links url)
    (with-current-buffer buffer
      (goto-char (point-min))
      (while (re-search-forward
	      (format "<a.+[^>]*?href=\[\"']?\\(%s\\)[\"']?[^>]*?>" pattern)
	      nil t)
	(push (cons (match-string 1) (match-string 2)) links)))
    (kill-buffer buffer)
    (setq url (caar (last (sort* links 'vcomp< :key 'cdr))))
    (when (stringp url)
      (if (string-match ".+://" url)
	  url
	(concat (replace-regexp-in-string "[^/]+$" "" page) url)))))

(provide 'vcomp)
;;; vcomp.el ends here
