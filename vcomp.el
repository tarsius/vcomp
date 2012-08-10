;;; vcomp.el --- compare version strings

;; Copyright (C) 2008-2012  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20081202
;; Version: 0.2.1
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

;; Compare version strings.  What is a valid version string is defined
;; jointly by the regular expression stored in variable `vcomp--regexp'
;; and the function `vcomp--intern'.

;; Even crazy version strings like "0.11a_rc3-r1" are supported.  But
;; note that you shouldn't go crazy just because you can and that the
;; \"-rN\" suffix while valid really is reserved for package maintainers
;; like ELPA or the Emacsmirror and should not be used by library authors.

;; Some related (or alternative) libraries and functions:
;;
;; - `version<' and related functions defined in `subr.el' distributed
;;   with Emacs, or stand-alone as `versions.el'.
;; - `inversion' which is part of Cedet and therefor included in recent
;;   versions of Emacs.
;; - `package.el' also contains some version handling code which this
;;   package was originally based on.

;; **************************** WARNING ****************************
;; *                                                               *
;; *  This package still functions properly and ISN'T OBSOLETE     *
;; *  but it is NO LONGER USED by the author.                      *
;; *                                                               *
;; *  It was written for the Emacsmirror because there are many    *
;; *  packages that use version strings to crazy for the tools     *
;; *  mentioned above.  I have since decided that it's not worth   *
;; *  trying to parse every single imaginable way some confused    *
;; *  mind might define a version string.  Therefor I now use the  *
;; *  builtin tools instead.                                       *
;; *                                                               *
;; *  If you are interested feel free to adopt it - orphans lead   *
;; *  a sad live.                                                  *
;; *                                                               *
;; *****************************************************************

;;; Code:

(require 'cl)

;;; Regular Versions.

(defconst vcomp--regexp
  (concat "^\\("
          "\\([0-9]+\\(?:[-_.][0-9]+\\)*\\)"
          "\\([a-z]\\)?"
          "\\(?:[-_]?\\(alpha\\|beta\\|pre\\|rc\\|p\\)\\([0-9]+\\)?\\)?"
          "\\(?:-r\\([0-9]+\\)\\)?"
          "\\)$")
  "The regular expression used to compare version strings.")

(defvar vcomp--fill-number -1
  "Integer used for missing positions in numeric part of versions.
Either -1 or 0.  See the library header of `vcomp.el' for more
information.")

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
           (nconc (car v2) (make-list (- l1 l2) vcomp--fill-number)))
          ((> l2 l1)
           (nconc (car v1) (make-list (- l2 l1) vcomp--fill-number)))))
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

(provide 'vcomp)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; vcomp.el ends here
