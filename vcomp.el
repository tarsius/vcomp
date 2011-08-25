;;; vcomp.el --- compare version strings

;; Copyright (C) 2008, 2009, 2010, 2011  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20081202
;; Version: 0.1.1-git
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

;;; Regular Versions.

(defconst vcomp--regexp
  (concat "^\\("
	  "\\([0-9]+\\(?:[-_.][0-9]+\\)*\\)"
	  "\\([a-z]\\)?"
	  "\\(?:_?\\(alpha\\|beta\\|pre\\|rc\\|p\\)\\([0-9]+\\)?\\)?"
	  "\\(?:-r\\([0-9]+\\)\\)?"
	  "\\)$")
  "The regular expression used to compare version strings.")

(defun vcomp-version-p (string)
  "Return t if STRING is a valid version string."
  (when (string-match-p vcomp--regexp string) t))

(defun vcomp--intern (version &optional prefix noerror)
  "Convert version string VERSION to a list of two lists of integers.

If optional PREFIX is non-nil it is a partial regular expression which
matches a prefix VERSION may (but does not need to) begin with, like e.g.
a package name.  PREFIX must not begin with ^ (unless you want to
literally match it) or contain any non-shy grouping constructs.

If VERSION cannot be converted an error is raised unless optional NOERROR
is non-nil in which case nil is returned."
  (if (string-match (if prefix
			(concat "^" prefix (substring vcomp--regexp 1))
		      vcomp--regexp)
		    version)
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
    (unless noerror
      (error "%S isn't a valid version string" version))))

(defun vcomp-compare (v1 v2 pred)
  "Compare version strings V1 and V2 using PRED."
  (vcomp--compare-interned (vcomp--intern v1)
			   (vcomp--intern v2)
			   pred))

(defun vcomp--compare-interned (v1 v2 pred)
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

(defun vcomp> (v1 v2)
  "Return t if first version string is greater than second."
  (vcomp-compare v1 v2 '>))

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

;;; Prefixed Versions.

(defun vcomp--prefix-regexp (&optional name)
  (concat "^\\(?:\\(?:"
	  (when name
	    (format "%s\\|" name))
	  "v\\(?:ersion\\)?\\|r\\(?:elease\\)"
	  "?\\)[-_]?\\)?"))

(defun vcomp-prefixed-version-p (string &optional prefix)
  "Return non-nil if STRING is a valid but possibly prefixed version string.

The returned value is the normalized part of STRING which is a valid
version string.

If optional PREFIX is non-nil it has to be a string.  If it begin with
\"^\" it is considered a partial regexp.  It must not end with \"$\" and may
only contain *shy* groups.  In this case STRING is matched against:

  (concat PREFIX (substring vcomp--regexp 1))

Otherwise if PREFIX is nil or does not begin with \"^\" the function
`vcomp--prefix-regexp' is used to create the prefix regexp.  In this
case STRING is matched against:

  (concat (vcomp--prefix-regexp PREFIX) (substring vcomp--regexp 1))"
  (when (string-match (concat (if (and prefix (string-match-p "^^" prefix))
				  prefix
				(vcomp--prefix-regexp prefix))
			      (substring vcomp--regexp 1))
		      string)
    (vcomp-normalize (match-string 1 string))))

(defun vcomp--reverse-regexp (version &optional prefix)
  (let* ((val (vcomp--intern version))
	 (num (nth 0 val))
	 (rst (nth 1 val))
	 (alp (nth 0 rst))
	 (tag (nth 1 rst))
	 (tnm (nth 2 rst))
	 (rev (nth 3 rst)))
    (concat "^"
	    (when prefix
	      (vcomp--prefix-regexp prefix))
	    (mapconcat (lambda (elt)
			 (concat "0*" (int-to-string elt)))
		       num "[-_.]")
	    (when (> alp 96)
	      (concat "["
		      (char-to-string alp)
		      (char-to-string (- alp 32))
		      "]"))
	    (case tag
	      (100 "_?alpha")
	      (101 "_?beta")
	      (102 "_?pre")
	      (103 "_?rc")
	      (104 nil)
	      (105 "_?p"))
	    (cond ((= tag 104) nil)
		  ((= tnm 0) "0?")
		  (t (int-to-string tnm)))
	    (when (> rev 0)
	      (concat "-r"))
	    "$")))

(defun vcomp-reverse-match (version strings &optional prefix)
  (setq version (vcomp--reverse-regexp version prefix))
  (car (member* version strings
		:test (lambda (version elt)
			(string-match version elt)))))

;;; Version Links.

(defun vcomp-max-link (page pattern &optional prefix)
  "Return the url to the file with the greatest version linked from PAGE.

Download PAGE and search it for links to versioned files matching a
generated regexp and return the full url of the link whose version part
is the greatest.  If PAGE can not be retrieved or no matching href can
be found return nil.

PATTERN is used as the first subexpression of that regexp and has to
match the value of the href attributes.  PATTERN itself has to contain
one subexpression which has to match a valid version string.  If PATTERN
contains %v this is the case as %v is replaced by the value of
`vcomp--regexp' sans the leading ^ and trailing $.

Among all matches the one whose match for the version-subexpression is
the greatest is determined and a complete url for that match is returned.

If the href-subexpression is a complete url that is returned otherwise
a complete url is created by prepending the href-expression with one of
the following strings:

* If optional PREFIX is non-nil use that,
* or if PAGE does not end with / use it sans the last part,
* else use the complete PAGE."
  (let ((regexp (format
		 "<a[^>]+href=[\"']?\\(%s\\)[\"']?[^>]*>"
		 (replace-regexp-in-string
		  "%v" (substring vcomp--regexp 1 -1) pattern nil t)))
	(buffer (condition-case nil
		    (url-retrieve-synchronously page)
		  (error nil)))
	matches)
    (when buffer
      (with-current-buffer buffer
	(goto-char (point-min))
	(while (re-search-forward regexp nil t)
	  (push (cons (match-string 1) (match-string 2)) matches)))
      (kill-buffer buffer)
      (setq href (caar (last (sort* matches 'vcomp< :key 'cdr))))
      (cond ((not href)
	     nil)
	    ((string-match ".+://" href)
	     href)
	    (prefix
	     (concat prefix href))
	    (t
	     (concat (replace-regexp-in-string "[^/]+$" "" page) href))))))

(provide 'vcomp)
;;; vcomp.el ends here
