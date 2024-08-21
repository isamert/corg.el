;;; corg.el --- Header completion for org-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Isa Mert Gurbuz

;; Author: Isa Mert Gurbuz <isamertgurbuz@gmail.com>
;; URL: https://github.com/isamert/corg.el
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1") (s "1.13.1") (dash "2.19.1"))
;; Keywords: abbrev convenience completion matching

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; *corg.el* (as in *C*omplete *ORG*) is an Emacs package that
;; provides completion-at-point for Org-mode source block and dynamic
;; block headers.  /corg/ auto-completes programming language names,
;; their parameters, and possible parameter values for source blocks.
;; For dynamic blocks, it completes block names, their parameters, and
;; possible parameter values.

;;; Code:

(require 's)
(require 'dash)
(require 'ob-core)

;;;; Main

(defun corg-completion-at-point ()
  "Provide a list of completion candidates with documentation.
See `corg' for detailed documentation."
  (-let ((bounds (bounds-of-thing-at-point 'filename))
         (candidates (corg)))
    (if candidates
      (list
       (or (car bounds) (point))
       (or (cdr bounds) (point))
       (mapcar #'car candidates)
       :annotation-function
       (lambda (candidate)
	 (concat " " (plist-get (alist-get candidate candidates nil nil #'equal) :ann)))
       :company-doc-buffer
       (lambda (candidate)
	 (with-current-buffer (get-buffer-create " *corg*")
           (erase-buffer)
           (let ((doc (plist-get (alist-get candidate candidates nil nil #'equal) :doc)))
             (insert (if (functionp doc)
			 (funcall doc)
                       doc)))
           (current-buffer)))))))

(defun corg ()
  "Return a list of possible completion candidates and their types.
A single completion is in the form of

  (completion . \\='(:doc \"documentation for the completion\"
                  :ann \"annotation for the completion\"
                  :type \"type of the completion\"))

where `:doc' can be function that returns a string.

This function is used by `corg-completion-at-point'.

Generally speaking, returned completions are annotated with one of these:

- doc: Meaning that this completion is extracted from the parsing
       of documentation of executer function of the block.  There
       might be some false-positives but it's generally a trusted
       source.

- source: Meaning that this completion is extracted from the
          parsing of executer function's source code of the
          block. There might be some false-positives but it's
          generally a trusted source.

- common: Meaning that this completion is extracted from the
          `org-babel-common-header-args-w-values'.  All of the
          common variables are not implemented by all source
          blocks.  The user should be aware of that.

- native: Meaning that this completion is extracted from a
          variable similar to the common variable but it is
          specifically defined for the block which means this is
          a definitely implemented, trusted completion."
  (-let* ((line (thing-at-point 'line t))
          ((_ type what _params)
           (s-match "^#\\+begin\\(:\\|_[a-zA-Z0-9]+\\) *\\([A-Za-z0-9_-]+\\)?* *\\(.*\\)?$" line))
          (block-type (pcase (s-chop-prefix "_" type)
                        (":" 'dblock)
                        ((or "src" "SRC") 'src))))
    (cond
     ((or (not line) (s-blank? type)) '())
     ((or (s-blank? what) (looking-back (format " %s" what) (line-beginning-position)))
      (corg--block-types block-type))
     ((looking-back ":[a-zA-Z0-9_-]+ +\"?" (line-beginning-position))
      (-let* (((start end) (match-data))
              (parameter (s-trim
                          (s-chop-suffix
                           "\""
                           (s-trim (buffer-substring-no-properties start end))))))
        (corg--parameter-types what block-type parameter)))
     ((not (s-blank? what))
      (corg--parameters what block-type))
     (t '()))))

;;;###autoload
(defun corg-setup ()
  "Enable use of `corg' in current buffer.
This function is meant to be used as following:

  (add-hook \\='org-mode-hook \\='corg-setup)

So that `corg' is automatically enabled in org mode."
  (interactive nil org-mode)
  (add-hook
   'completion-at-point-functions
   #'corg-completion-at-point nil 'local))

(defun corg--parameter-types (what block-type param)
  "Get types of PARAM of the BLOCK-TYPE.
WHAT is block name (see `corg--parameters' for more details).  PARAM is
the parameter name, something like `:tangle', `:eval' etc.

Return type is same as described in `corg'."
  (let* ((param-info
          (alist-get param (corg--parameters what block-type) nil nil #'equal))
         (param-type (plist-get param-info :type)))
    (--map
     (cons it (list :ann (concat "∈ " param)
                    :doc (corg--doc-fn what block-type param-type)
                    :type (plist-get param-info :type)))
     (corg--candify-type param-type))))

(defun corg--parameters (what block-type)
  "Get parameters of the BLOCK-TYPE.
WHAT is the name of the block, for source blocks it's something
like `emacs-lisp', `python' etc. and for dynamic blocks it's
something like `clocktable', `org-ql' etc.

Return type is same as described in `corg'."
  (let* ((var-candidates (corg--parameters-from-var what block-type))
         (source-candidates (unless var-candidates
                              (corg--parameters-from-source-code what block-type)))
         (doc-candidates (unless source-candidates
                           (corg--get-parameter-completions-from-doc what block-type)))
         (common-candidates (when (equal 'src block-type)
                              (corg--parameters-from-common-src-args what block-type))))
    (append
     var-candidates
     source-candidates
     doc-candidates
     common-candidates)))

(defun corg--block-types (block-type)
  "Get list of all possible names for BLOCK-TYPE.
For source blocks it's something like `emacs-lisp', `python'
etc. and for dynamic blocks it's something like `clocktable',
`org-ql' etc.

Return type is same as described in `corg'."
  (--map
   (let ((lang (s-chop-prefixes '("org-dblock-write:" "org-babel-execute:") (symbol-name it))))
     (cons
      lang
      (list :ann (concat (symbol-name block-type) " type")
            :doc (corg--doc-fn lang block-type))))
   (corg--get-functions-starting-with
    (pcase block-type
      ('dblock "org-dblock-write:")
      ('src "org-babel-execute:")))))

;;;; Parameter completion

(defun corg--parameters-from-common-src-args (what block-type)
  "Extract all common parameters from `org-babel-common-header-args-w-values'.

WHAT is block name (see `corg--parameters' for more details).
BLOCK-TYPE is either \\='src or \\='dblock.

These completions are annotated as \"common\"."
  (corg--type-completions-for-parameter
   what block-type "common"
   org-babel-common-header-args-w-values))

(defun corg--parameters-from-source-code (what block-type)
  "Parse the source code of the execution function and retrieve parameters.
For source blocks `org-babel-execute:<LANG>', for dynamic blocks
`org-dblock-write:<BLOCK>' functions source codes are parsed and
the used parameter names are extracted.  Of course, for this to
work the code should've been written in certain style where it
makes calls to `alist-get', `assq' or `plist-get' with parameter
name and the PARAMS object that is supplied to the function.  Any
indirection will disrupt the parsing logic of this function but
generally speaking, write and execute functions are written in
this way.

WHAT is block name (see `corg--parameters' for more details).
BLOCK-TYPE is either \\='src or \\='dblock.

These completions are annotated as \"source\"."
  (-let* ((fn-name (corg--build-fn-name what block-type))
          (fn (-as->
               fn-name %
               (intern %)
               (corg--get-function-source %)
               (s-lines %)
               (s-join " " %)))
          ((_ fntype) (s-match "( *\\(defun\\|cl-defun\\|lambda\\)" fn))
          (args (pcase fntype
                  ("lambda" (s-match "(lambda *(\\([a-zA-Z0-9]+\\) *\\([a-zA-Z0-9]+\\)?)" fn))
                  (fn-type (s-match (format "(%s %s *(\\([a-zA-Z0-9]+\\) *\\([a-zA-Z0-9]+\\)?)" fn-type fn-name) fn))))
          (params-name (pcase block-type
                         ('src (nth 2 args))
                         ('dblock (nth 1 args)))))
    (--map
     (cons
      it
      (list
       :ann (concat what " parameter (source)")
       :doc (corg--doc-fn what block-type "not available. This parameter found in the documentation.")
       :type nil))
     (corg--extract-parameters-from-source fn params-name))))

(defun corg--get-parameter-completions-from-doc (what block-type)
  "Get parameter candidates from function documentation.
For source blocks `org-babel-execute:<LANG>', for dynamic blocks
`org-dblock-write:<BLOCK>' functions documentations are parsed
and the mentioned parameter names are extracted.  Of course, for
this to work the documentation should've been written in certain
style where it makes mentions parameters as `:param-name'
explicitly.  Word boundaries are mostly handled properly so that
you don't get many false positives.

WHAT is block name (see `corg--parameters' for more details).
BLOCK-TYPE is either \\='src or \\='dblock.

These completions are annotated as \"doc\"."
  (-let* ((fn-name (corg--build-fn-name what block-type))
          (doc (ignore-errors
                 (documentation (intern fn-name)))))
    (--map
     (cons
      it
      (list
       :ann (concat what " parameter (doc)")
       :doc (corg--doc-fn what block-type "not available. This parameter found in source code. See below.")
       :type nil))
     ;; I don't use an extensive regexp to reduce the amount of
     ;; false-positives. Parameter names are mostly lower-kebab-case
     ;; and that's what we need.
     (-uniq (mapcar #'cadr (s-match-strings-all "[  \t\n\"`'‘’“”]+\\(:[a-z]+\\)[  \t\n\"`'‘’“”]+" doc))))))

(defun corg--parameters-from-var (what block-type)
  "Get parameter candidates from a special variable, if available.
For SRC blocks, this variable is `org-babel-header-args:<LANG>',
for dynamic blocks no such variable exist, but in the same vein
we try to get them from `org-dblock-header-args:<BLOCK>' if it's
provided.  This probably does not exist but user can supply their
own completions this way.

WHAT is block name (see `corg--parameters' for more details).
BLOCK-TYPE is either \\='src or \\='dblock.

These completions are annotated as \"native\"."
  (corg--type-completions-for-parameter
   what block-type "native"
   (ignore-errors
     (symbol-value
      (intern (concat
               (pcase block-type
                 ('dblock "org-dblock-header-args")
                 ('src "org-babel-header-args"))
               ":" what))))))

(defun corg--extract-parameters-from-source (source params-name)
  (-uniq
   (mapcar
    #'cadr
    (append
     ;; Well, this fails if the sexp spans for more than one line
     (s-match-strings-all (format "(plist-get +%s +\\([a-zA-Z0-9:_-]+\\))" params-name) source)
     (s-match-strings-all (format "(alist-get +\\([a-zA-Z0-9:_-]+\\) +%s)" params-name) source)
     (s-match-strings-all (format "(assq +\\([a-zA-Z0-9:_-]+\\) +%s)" params-name) source)))))

(defun corg--type-completions-for-parameter (what block-type from parameter)
  (--map
   (cons
    (concat ":" (symbol-name (car it)))
    (list
     :ann (concat what " parameter (" from ")")
     :doc (corg--doc-fn what block-type (cdr it))
     :type (cdr it)))
   parameter))

;;;; Utils

(defun corg--get-function-source (function)
  "Return FUNCTION source in string form."
  (-if-let ((buffer . pos) (ignore-error error
                             (delay-mode-hooks
                               (find-definition-noselect function nil))))
      (save-current-buffer
        (set-buffer buffer)
        (buffer-substring-no-properties
         pos
         (progn (end-of-defun) (point))))
    (let ((print-level nil)
          (print-length nil))
      (format "%S" (indirect-function function)))))

(defun corg--get-functions-starting-with (prefix)
  "Get all function names starting with PREFIX."
  (let ((result '()))
    (mapatoms
     (lambda (symbol)
       (when (and (fboundp symbol)
                  (string-prefix-p prefix (symbol-name symbol)))
         (push symbol result))))
    result))

(defun corg--get-package-commentary (package)
  "Get commentary of given PACKAGE."
  (when-let ((pkg-file (s-chop-suffix "c" (locate-library package))))
    (with-temp-buffer
      (insert-file-contents pkg-file)
      (goto-char (point-min))
      (when (search-forward ";;; Commentary:" nil t)
        (let* ((start (point))
               (commentary (if (search-forward ";;;" nil t)
                               (buffer-substring start (match-beginning 0))
                             "")))
          (->>
           commentary
           s-trim
           s-lines
           (--map (s-trim (s-chop-prefix ";;" it)))
           (s-join "\n")))))))

(defun corg--build-fn-name (what block-type)
  (concat
   (pcase block-type
     ('dblock "org-dblock-write")
     ('src "org-babel-execute"))
   ":" what))

(defun corg--candify-type (type)
  (cond
   ((listp type) (-non-nil (mapcar #'corg--candify-type-1 (car type))))
   (t (when-let ((result (corg--candify-type-1 type)))
        result))))

(defun corg--candify-type-1 (type)
  (cond
   ((equal type :any) nil)
   ((symbolp type) (symbol-name type))
   (t (format "%S" (or type "")))))

(defun corg--stringify-type (type)
  (cond
   ((listp type) (s-join "|" (mapcar #'corg--stringify-type (car type))))
   ((symbolp type) (symbol-name type))
   (t (format "%s" type))))

(defun corg--doc-fn (lang block-type &optional type)
  (lambda ()
    (let ((fn-name (concat
                    ;; TODO extract to function?
                    (pcase block-type
                      ('dblock "org-dblock-write")
                      ('src "org-babel-execute"))
                    ":" lang)))
      (concat
       (when type
         (concat "Type is " (corg--stringify-type type) "\n\n---\n\n"))
       (when-let ((commentary (corg--get-package-commentary (concat "ob-" lang))))
         (concat commentary "\n\n---\n\n"))
       "⇒ "
       fn-name
       "\n"
       (ignore-errors
         (documentation (intern fn-name)))))))

;;;; Footer

(provide 'corg)

;;; corg.el ends here
