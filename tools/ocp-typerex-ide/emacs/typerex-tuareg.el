;;; ocp.el --- Caml mode for (X)Emacs.

;;        Copyright (C) 1997-2006 Albert Cohen, all rights reserved.
;;        Copyright (C) 2009-2010 Jane Street Holding, LLC.
;;        Copyright (C) 2011 OCamlPro SAS

;;        Licensed under the GNU General Public License.

;;    This program is free software; you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation; either version 2 of the License, or
;;    (at your option) any later version.

;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;    GNU General Public License for more details.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))
(require 'easymenu)

(defconst typerex-mode-version
  (concat "TypeRex Version " typerex-version " ("
          (eval-when-compile
            (let ((file (or (and (boundp 'byte-compile-current-file)
                                 byte-compile-current-file)
                            load-file-name)))
              (when file
                (setq file (expand-file-name "version"
                                             (file-name-directory file))))
              (with-temp-buffer
                (if (and file (file-exists-p file))
                    (insert-file-contents-literally file)
                    (let ((default-directory
                           (if file
                               (file-name-directory file)
                               default-directory)))
                      (cond ((file-directory-p ".hg")
                             (call-process "hg" nil t nil "id" "-i" "--debug"))
                            ((file-directory-p ".svn")
                             (shell-command "svn info | grep Revision: | sed 's/Revision: //'" t))
			    ((file-directory-p ".bzr")
                             (shell-command "bzr log -l -1 | grep revno:" t))
                            (t (insert "unknown\n")))))
                (buffer-substring-no-properties
                 (point-min) (1- (point-max))))))
          ")")
  "         Copyright (C) 1997-2006 Albert Cohen, all rights reserved.
         Copyright (C) 2009-2010 Jane Street Holding, LLC.
         Copyright (C) 2011 OCamlPro SAS
         Copying is covered by the GNU General Public License.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        Emacs versions support

(defconst typerex-with-xemacs (featurep 'xemacs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      Compatibility functions

(defun typerex-editing-ls3 ()
  "Tells whether we are editing Lucid Synchrone syntax."
  (string-match "\\.ls" (buffer-name)))

(defun typerex-editing-camllex ()
  "Tells whether we are editing CamlLex syntax."
  (string-match "\\.mll" (buffer-name)))

(defalias 'typerex-match-string
  (if (fboundp 'match-string-no-properties)
      'match-string-no-properties
    'match-string))

(or (fboundp 'read-shell-command)
    (defun read-shell-command  (prompt &optional initial-input history)
      "Read a string from the minibuffer, using `shell-command-history'."
      (read-from-minibuffer prompt initial-input nil nil
                            (or history 'shell-command-history))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Import types and help features

(defvar typerex-with-caml-mode-p
  (and (require 'caml-types nil t) (require 'caml-help nil t)))
(eval-when-compile
  (autoload 'caml-complete "caml-help"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       User customizable variables

;; Use the standard `customize' interface or `typerex-mode-hook' to
;; Configure these variables

(require 'custom)

(defgroup ocp nil
  "Support for the OCaml language."
  :group 'languages)

;; Comments

(defcustom typerex-indent-leading-comments t
  "*If true, indent leading comment lines (starting with `(*') like others."
  :group 'ocp :type 'boolean)

(defcustom typerex-indent-comments t
  "*If true, automatically align multi-line comments."
  :group 'ocp :type 'boolean)

(defcustom typerex-comment-end-extra-indent 0
  "*How many spaces to indent a leading comment end `*)'.
If you expect comments to be indented like
        (*
          ...
         *)
even without leading `*', use `typerex-comment-end-extra-indent' = 1."
  :group 'ocp
  :type '(radio :extra-offset 8
                :format "%{Comment End Extra Indent%}:
   Comment alignment:\n%v"
                (const :tag "align with `(' in comment opening" 0)
                (const :tag "align with `*' in comment opening" 1)
                (integer :tag "custom alignment" 0)))

(defcustom typerex-support-leading-star-comments t
  "*Enable automatic intentation of comments of the form
        (*
         * ...
         *)
Documentation comments (** *) are not concerned by this variable
unless `typerex-leading-star-in-doc' is also set.

If you do not set this variable and still expect comments to be
indented like
        (*
          ...
         *)
\(without leading `*'), set `typerex-comment-end-extra-indent' to 1."
  :group 'ocp :type 'boolean)

(defcustom typerex-leading-star-in-doc nil
  "*Enable automatic intentation of documentation comments of the form
        (**
         * ...
         *)"
  :group 'ocp :type 'boolean)

;; Indentation defaults

(defcustom typerex-default-indent 2
  "*Default indentation.

Global indentation variable (large values may lead to indentation overflows).
When no governing keyword is found, this value is used to indent the line
if it has to."
  :group 'ocp :type 'integer)

(defcustom typerex-support-camllight nil
  "*If true, handle Caml Light character syntax (incompatible with labels)."
  :group 'ocp :type 'boolean
  :set '(lambda (var val)
          (setq typerex-support-camllight val)
          (when (boundp 'typerex-mode-syntax-table)
            (modify-syntax-entry ?` (if val "\"" ".")
                                 typerex-mode-syntax-table))))

(defcustom typerex-support-metaocaml nil
  "*If true, handle MetaOCaml syntax."
  :group 'ocp :type 'boolean
  :set '(lambda (var val)
          (setq typerex-support-metaocaml val)
          (when (boundp 'typerex-font-lock-keywords)
            (typerex-make-indentation-regexps)
            (typerex-install-font-lock))))

(defcustom typerex-let-always-indent t
  "*If true, enforce indentation is at least `typerex-let-indent' after a `let'.

As an example, set it to false when you have `typerex-with-indent' set to 0,
and you want `let x = match ... with' and `match ... with' indent the
same way."
  :group 'ocp :type 'boolean)

(defcustom typerex-pipe-extra-unindent typerex-default-indent
  "*Extra backward indent for Caml lines starting with the `|' operator.

It is NOT the variable controlling the indentation of the `|' itself:
this value is automatically added to `function', `with', `parse' and
some cases of `type' keywords to leave enough space for `|' backward
indentation.

For example, setting this variable to 0 leads to the following indentation:
  match ... with
    X -> ...
    | Y -> ...
    | Z -> ...

To modify the indentation of lines lead by `|' you need to modify the
indentation variables for `with', `function' and `parse', and possibly
for `type' as well. For example, setting them to 0 (and leaving
`typerex-pipe-extra-unindent' to its default value) yields:
  match ... with
    X -> ...
  | Y -> ...
  | Z -> ..."
  :group 'ocp :type 'integer)

(defcustom typerex-class-indent typerex-default-indent
  "*How many spaces to indent from a `class' keyword."
  :group 'ocp :type 'integer)

(defcustom typerex-sig-struct-align t
  "*Align `sig' and `struct' keywords with `module'."
  :group 'ocp :type 'boolean)

(defcustom typerex-sig-struct-indent typerex-default-indent
  "*How many spaces to indent from a `sig' or `struct' keyword."
  :group 'ocp :type 'integer)

(defcustom typerex-method-indent typerex-default-indent
  "*How many spaces to indent from a `method' keyword."
  :group 'ocp :type 'integer)

(defcustom typerex-begin-indent typerex-default-indent
  "*How many spaces to indent from a `begin' keyword."
  :group 'ocp :type 'integer)

(defcustom typerex-for-while-indent typerex-default-indent
  "*How many spaces to indent from a `for' or `while' keyword."
  :group 'ocp :type 'integer)

(defcustom typerex-do-indent typerex-default-indent
  "*How many spaces to indent from a `do' keyword."
  :group 'ocp :type 'integer)

(defcustom typerex-fun-indent typerex-default-indent
  "*How many spaces to indent from a `fun' keyword."
  :group 'ocp :type 'integer)

(defcustom typerex-function-indent typerex-default-indent
  "*How many spaces to indent from a `function' keyword."
  :group 'ocp :type 'integer)

(defcustom typerex-if-then-else-indent typerex-default-indent
  "*How many spaces to indent from an `if', `then' or `else' keyword."
  :group 'ocp :type 'integer)

(defcustom typerex-let-indent typerex-default-indent
  "*How many spaces to indent from a `let' keyword."
  :group 'ocp :type 'integer)

(defcustom typerex-in-indent 0 ; typerex-default-indent
  "*How many spaces to indent from a `in' keyword.
Upstream <http://caml.inria.fr/resources/doc/guides/guidelines.en.html>
recommends 0, and this is what we default to since 2.0.1
instead of the historical `typerex-default-indent'."
  :group 'ocp :type 'integer)

(defcustom typerex-match-indent typerex-default-indent
  "*How many spaces to indent from a `match' keyword."
  :group 'ocp :type 'integer)

(defcustom typerex-try-indent typerex-default-indent
  "*How many spaces to indent from a `try' keyword."
  :group 'ocp :type 'integer)

(defcustom typerex-with-indent typerex-default-indent
  "*How many spaces to indent from a `with' keyword."
  :group 'ocp :type 'integer)

(defcustom typerex-rule-indent typerex-default-indent
  "*How many spaces to indent from a `rule' keyword."
  :group 'ocp :type 'integer)

(defcustom typerex-type-indent typerex-default-indent
  "*How many spaces to indent from a `type' keyword."
  :group 'ocp :type 'integer)

(defcustom typerex-val-indent typerex-default-indent
  "*How many spaces to indent from a `val' keyword."
  :group 'ocp :type 'integer)

;; Automatic indentation
;; Using abbrev-mode and electric keys

(defcustom typerex-use-abbrev-mode t
  "*Non-nil means electrically indent lines starting with leading keywords.
Leading keywords are such as `end', `done', `else' etc.
It makes use of `abbrev-mode'.

Many people find eletric keywords irritating, so you can disable them by
setting this variable to nil."
  :group 'ocp :type 'boolean
  :set '(lambda (var val)
          (setq typerex-use-abbrev-mode val)
          (abbrev-mode val)))

(defcustom typerex-electric-indent t
  "*Non-nil means electrically indent lines starting with `|', `)', `]' or `}'.

Many people find eletric keys irritating, so you can disable them in
setting this variable to nil."
  :group 'ocp :type 'boolean)

(defcustom typerex-electric-close-vector t
  "*Non-nil means electrically insert `|' before a vector-closing `]' or
`>' before an object-closing `}'.

Many people find eletric keys irritating, so you can disable them in
setting this variable to nil. You should probably have this on,
though, if you also have `typerex-electric-indent' on."
  :group 'ocp :type 'boolean)

;; TypeRex-Interactive
;; Configure via `typerex-mode-hook'

(defcustom typerex-interactive-scroll-to-bottom-on-output nil
  "*Controls when to scroll to the bottom of the interactive buffer
upon evaluating an expression.

See `comint-scroll-to-bottom-on-output' for details."
  :group 'ocp :type 'boolean
  :set '(lambda (var val)
          (setq typerex-interactive-scroll-to-bottom-on-output val)
          (when (boundp 'comint-scroll-to-bottom-on-output)
            (setq comint-scroll-to-bottom-on-output val))))

(defcustom typerex-skip-after-eval-phrase t
  "*Non-nil means skip to the end of the phrase after evaluation in the
Caml toplevel."
  :group 'ocp :type 'boolean)

(defcustom typerex-interactive-read-only-input nil
  "*Non-nil means input sent to the Caml toplevel is read-only."
  :group 'ocp :type 'boolean)

(defcustom typerex-interactive-echo-phrase t
  "*Non-nil means echo phrases in the toplevel buffer when sending
them to the Caml toplevel."
  :group 'ocp :type 'boolean)

(defcustom typerex-interactive-input-font-lock t
  "*Non nil means Font-Lock for toplevel input phrases."
  :group 'ocp :type 'boolean)

(defcustom typerex-interactive-output-font-lock t
  "*Non nil means Font-Lock for toplevel output messages."
  :group 'ocp :type 'boolean)

(defcustom typerex-interactive-error-font-lock t
  "*Non nil means Font-Lock for toplevel error messages."
  :group 'ocp :type 'boolean)

(defcustom typerex-display-buffer-on-eval t
  "*Non nil means pop up the Caml toplevel when evaluating code."
  :group 'ocp :type 'boolean)

(defcustom typerex-manual-url "http://pauillac.inria.fr/ocaml/htmlman/index.html"
  "*URL to the Caml reference manual."
  :group 'ocp :type 'string)

(defcustom typerex-browser 'browse-url
  "*Name of function that displays the Caml reference manual.
Valid names are `browse-url', `browse-url-firefox', etc."
  :group 'ocp)

(defcustom typerex-library-path "/usr/local/lib/ocaml/"
  "*Path to the Caml library."
  :group 'ocp :type 'string)

(defcustom typerex-definitions-max-items 30
  "*Maximum number of items a definitions menu can contain."
  :group 'ocp :type 'integer)

(defvar typerex-options-list
  '(("Automatic indentation of leading keywords" . 'typerex-use-abbrev-mode)
    ("Automatic indentation of ), ] and }" . 'typerex-electric-indent)
    ("Automatic matching of [| and {<" . 'typerex-electric-close-vector)
    "---"
    ("Indent body of comments" . 'typerex-indent-comments)
    ("Indent first line of comments" . 'typerex-indent-leading-comments)
    ("Leading-`*' comment style" . 'typerex-support-leading-star-comments))
  "*List of menu-configurable TypeRex options.")

(defvar typerex-interactive-options-list
  '(("Skip phrase after evaluation" . 'typerex-skip-after-eval-phrase)
    ("Echo phrase in interactive buffer" . 'typerex-interactive-echo-phrase)
    "---"
    ("Font-lock interactive input" . 'typerex-interactive-input-font-lock)
    ("Font-lock interactive output" . 'typerex-interactive-output-font-lock)
    ("Font-lock interactive error" . 'typerex-interactive-error-font-lock)
    "---"
    ("Read only input" . 'typerex-interactive-read-only-input))
  "*List of menu-configurable TypeRex options.")

(defvar typerex-interactive-program "ocaml"
  "*Default program name for invoking a Caml toplevel from Emacs.")
;; Could be interesting to have this variable buffer-local
;;   (e.g., ocaml vs. metaocaml buffers)
;; (make-variable-buffer-local 'typerex-interactive-program)

(eval-and-compile
  (defconst typerex-use-syntax-ppss (fboundp 'syntax-ppss)
    "*If nil, use our own parsing and caching."))

(defgroup typerex-faces nil
  "Special faces for the TypeRex mode."
  :group 'ocp)

(defconst typerex-faces-inherit-p
  (and (boundp 'face-attribute-name-alist)
       (assq :inherit face-attribute-name-alist)))

(defface typerex-font-lock-governing-face
  '((((background light)) (:foreground "blue" :bold t))
    (t (:foreground "orange" :bold t)))
  "Face description for governing/leading keywords."
  :group 'typerex-faces)
(defvar typerex-font-lock-governing-face
  'typerex-font-lock-governing-face)

(defface typerex-font-lock-multistage-face
  '((((background light))
     (:foreground "darkblue" :background "lightgray" :bold t))
    (t (:foreground "steelblue" :background "darkgray" :bold t)))
  "Face description for MetaOCaml staging operators."
  :group 'typerex-faces)
(defvar typerex-font-lock-multistage-face
  'typerex-font-lock-multistage-face)

(defface typerex-font-lock-operator-face
  '((((background light)) (:foreground "brown"))
    (t (:foreground "khaki")))
  "Face description for all operators."
  :group 'typerex-faces)
(defvar typerex-font-lock-operator-face
  'typerex-font-lock-operator-face)

(defface typerex-font-lock-error-face
  '((t (:foreground "yellow" :background "red" :bold t)))
  "Face description for all errors reported to the source."
  :group 'typerex-faces)
(defvar typerex-font-lock-error-face
  'typerex-font-lock-error-face)

(defface typerex-font-lock-interactive-output-face
  '((((background light))
     (:foreground "blue4"))
    (t (:foreground "cyan")))
  "Face description for all toplevel outputs."
  :group 'typerex-faces)
(defvar typerex-font-lock-interactive-output-face
  'typerex-font-lock-interactive-output-face)

(defface typerex-font-lock-interactive-error-face
  (if typerex-faces-inherit-p
      '((t :inherit font-lock-warning-face))
    '((((background light)) (:foreground "red3"))
      (t (:foreground "red2"))))
  "Face description for all toplevel errors."
  :group 'typerex-faces)
(defvar typerex-font-lock-interactive-error-face
  'typerex-font-lock-interactive-error-face)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            Support definitions

(defun typerex-leading-star-p ()
  (and typerex-support-leading-star-comments
       (save-excursion ; this function does not make sense outside of a comment
         (typerex-beginning-of-literal-or-comment)
         (and (or typerex-leading-star-in-doc
                  (not (looking-at "(\\*[Tt][Ee][Xx]\\|(\\*\\*")))
              (progn
                (forward-line 1)
                (back-to-indentation)
                (looking-at "\\*[^)]"))))))

(defun typerex-auto-fill-insert-leading-star (&optional leading-star)
  (let ((point-leading-comment (looking-at "(\\*")) (return-leading nil))
    (save-excursion
      (back-to-indentation)
      (when typerex-electric-indent
        (when (and (typerex-in-comment-p)
                   (or leading-star
                       (typerex-leading-star-p)))
          (unless (looking-at "(?\\*")
            (insert-before-markers "* "))
          (setq return-leading t))
        (unless point-leading-comment
          ;; Use optional argument to break recursion
          (typerex-indent-command t))))
    return-leading))

(defun typerex-auto-fill-function ()
  (unless (typerex-in-literal-p)
    (let ((leading-star
           (and (not (char-equal ?\n last-command-event))
                (typerex-auto-fill-insert-leading-star))))
      (do-auto-fill)
      (unless (char-equal ?\n last-command-event)
        (typerex-auto-fill-insert-leading-star leading-star)))))

;; these two functions are different from the standard
;; in that they do NOT signal errors beginning-of-buffer and end-of-buffer
(defun typerex-forward-char (&optional step)
  (if step (goto-char (+ (point) step))
    (goto-char (1+ (point)))))

(defun typerex-backward-char (&optional step)
  (if step (goto-char (- (point) step))
    (goto-char (1- (point)))))

(defun typerex-in-indentation-p ()
  "Return non-nil if all chars between beginning of line and point are blanks."
  (save-excursion
    (skip-chars-backward " \t")
    (bolp)))

(defvar typerex-cache-stop (point-min))
(make-variable-buffer-local 'typerex-cache-stop)
(defvar typerex-cache nil)
(make-variable-buffer-local 'typerex-cache)
(defvar typerex-cache-local nil)
(make-variable-buffer-local 'typerex-cache-local)
(defvar typerex-cache-last-local nil)
(make-variable-buffer-local 'typerex-cache-last-local)
(defvar typerex-last-loc (cons nil nil))

;; PPSS definitions
(defun typerex-ppss-in-literal-or-comment () (error "ocp uses PPSS"))
(defun typerex-ppss-fontify (beg end) (error "ocp uses PPSS"))
(defun typerex-ppss-in-literal-p ()
  "Returns non-nil if point is inside a Caml literal."
  (nth 3 (syntax-ppss)))
(defun typerex-ppss-in-comment-p ()
  "Returns non-nil if point is inside or right before a Caml comment."
  (or (nth 4 (syntax-ppss))
      (looking-at "[ \t]*(\\*")))
(defun typerex-ppss-in-literal-or-comment-p ()
  "Returns non-nil if point is inside a Caml literal or comment."
  (nth 8 (syntax-ppss)))
(defun typerex-ppss-beginning-of-literal-or-comment ()
  "Skips to the beginning of the current literal or comment (or buffer)."
  (interactive)
  (goto-char (or (nth 8 (syntax-ppss)) (point))))
(defun typerex-ppss-beginning-of-literal-or-comment-fast ()
  (goto-char (or (nth 8 (syntax-ppss)) (point-min))))
;; FIXME: not clear if moving out of a string/comment counts as 1 or no.
(defalias 'typerex-backward-up-list 'backward-up-list)

;; non-PPSS definitions
(defun typerex-!ppss-in-literal-p ()
  "Return non-nil if point is inside a Caml literal."
  (car (typerex-in-literal-or-comment)))
(defun typerex-!ppss-in-comment-p ()
  "Return non-nil if point is inside a Caml comment."
  (cdr (typerex-in-literal-or-comment)))
(defun typerex-!ppss-in-literal-or-comment-p ()
  "Return non-nil if point is inside a Caml literal or comment."
  (typerex-in-literal-or-comment)
  (or (car typerex-last-loc) (cdr typerex-last-loc)))
(defun typerex-!ppss-in-literal-or-comment ()
  "Return the pair `((typerex-in-literal-p) . (typerex-in-comment-p))'."
  (if (and (<= (point) typerex-cache-stop) typerex-cache)
      (progn
        (if (or (not typerex-cache-local) (not typerex-cache-last-local)
                (and (>= (point) (caar typerex-cache-last-local))))
            (setq typerex-cache-local typerex-cache))
        (while (and typerex-cache-local (< (point) (caar typerex-cache-local)))
          (setq typerex-cache-last-local typerex-cache-local
                typerex-cache-local (cdr typerex-cache-local)))
        (setq typerex-last-loc
              (if typerex-cache-local
                  (cons (eq (cadar typerex-cache-local) 'b)
                        (> (cddar typerex-cache-local) 0))
                  (cons nil nil))))
    (let ((flag t) (op (point)) (mp (min (point) (1- (point-max))))
          (balance 0) (end-of-comment nil))
      (while (and typerex-cache (<= typerex-cache-stop (caar typerex-cache)))
        (setq typerex-cache (cdr typerex-cache)))
      (if typerex-cache
          (if (eq (cadar typerex-cache) 'b)
              (progn
                (setq typerex-cache-stop (1- (caar typerex-cache)))
                (goto-char typerex-cache-stop)
                (setq balance (cddar typerex-cache))
                (setq typerex-cache (cdr typerex-cache)))
            (setq balance (cddar typerex-cache))
            (setq typerex-cache-stop (caar typerex-cache))
            (goto-char typerex-cache-stop)
            (skip-chars-forward "("))
          (goto-char (point-min)))
      (skip-chars-backward "\\\\*")
      (while flag
        (if end-of-comment (setq balance 0 end-of-comment nil))
        (skip-chars-forward "^\\\\'`\"(\\*")
        (cond
          ((looking-at "\\\\")
           (typerex-forward-char 2))
          ((looking-at "'\\([^\n\\']\\|\\\\[^ \t\n][^ \t\n]?[^ \t\n]?\\)'")
           (setq typerex-cache (cons (cons (1+ (point)) (cons 'b balance))
                                    typerex-cache))
           (goto-char (match-end 0))
           (setq typerex-cache (cons (cons (point) (cons 'e balance))
                                    typerex-cache)))
          ((and
            typerex-support-camllight
            (looking-at "`\\([^\n\\']\\|\\\\[^ \t\n][^ \t\n]?[^ \t\n]?\\)`"))
           (setq typerex-cache (cons (cons (1+ (point)) (cons 'b balance))
                                    typerex-cache))
           (goto-char (match-end 0))
           (setq typerex-cache (cons (cons (point) (cons 'e balance))
                                    typerex-cache)))
          ((looking-at "\"")
           (typerex-forward-char)
           (setq typerex-cache (cons (cons (point) (cons 'b balance))
                                    typerex-cache))
           (skip-chars-forward "^\\\\\"")
           (while (looking-at "\\\\")
             (typerex-forward-char 2) (skip-chars-forward "^\\\\\""))
           (typerex-forward-char)
           (setq typerex-cache (cons (cons (point) (cons 'e balance))
                                    typerex-cache)))
          ((looking-at "(\\*")
           (setq balance (1+ balance))
           (setq typerex-cache (cons (cons (point) (cons nil balance))
                                    typerex-cache))
           (typerex-forward-char 2))
          ((looking-at "\\*)")
           (typerex-forward-char 2)
           (if (> balance 1)
               (progn
                 (setq balance (1- balance))
                 (setq typerex-cache (cons (cons (point) (cons nil balance))
                                          typerex-cache)))
               (setq end-of-comment t)
               (setq typerex-cache (cons (cons (point) (cons nil 0))
                                        typerex-cache))))
          (t (typerex-forward-char)))
        (setq flag (<= (point) mp)))
      (setq typerex-cache-local typerex-cache
            typerex-cache-stop (point))
      (goto-char op)
      (if typerex-cache (typerex-in-literal-or-comment)
          (setq typerex-last-loc (cons nil nil))
          typerex-last-loc))))
(defun typerex-!ppss-beginning-of-literal-or-comment ()
  "Skips to the beginning of the current literal or comment (or buffer)."
  (interactive)
  (when (typerex-in-literal-or-comment-p)
    (typerex-beginning-of-literal-or-comment-fast)))

(defun typerex-!ppss-beginning-of-literal-or-comment-fast ()
  (while (and typerex-cache-local
              (or (eq 'b (cadar typerex-cache-local))
                  (> (cddar typerex-cache-local) 0)))
    (setq typerex-cache-last-local typerex-cache-local
          typerex-cache-local (cdr typerex-cache-local)))
  (if typerex-cache-last-local
      (goto-char (caar typerex-cache-last-local))
    (goto-char (point-min)))
  (when (eq 'b (cadar typerex-cache-last-local)) (typerex-backward-char)))

(defun typerex-!ppss-backward-up-list ()
  "Safe up-list regarding comments, literals and errors."
  (let ((balance 1) (op (point)) (oc nil))
    (typerex-in-literal-or-comment)
    (while (and (> (point) (point-min)) (> balance 0))
      (setq oc (if typerex-cache-local (caar typerex-cache-local) (point-min)))
      (condition-case nil (up-list -1) (error (goto-char (point-min))))
      (if (>= (point) oc) (setq balance (1- balance))
        (goto-char op)
        (skip-chars-backward "^[]{}()") (typerex-backward-char)
        (cond ((typerex-in-literal-or-comment-p)
               (typerex-beginning-of-literal-or-comment-fast))
              ((looking-at "[[{(]")
               (setq balance (1- balance)))
              ((looking-at "[]})]")
               (setq balance (1+ balance)))))
      (setq op (point)))))

(defalias 'typerex-in-literal-or-comment
    (eval-and-compile (if typerex-use-syntax-ppss
                          'typerex-ppss-in-literal-or-comment
                          'typerex-!ppss-in-literal-or-comment)))
(defalias 'typerex-fontify
    (eval-and-compile (if typerex-use-syntax-ppss
                          'typerex-ppss-fontify
                          'typerex-!ppss-fontify)))
(defalias 'typerex-in-literal-p
    (eval-and-compile (if typerex-use-syntax-ppss
                          'typerex-ppss-in-literal-p
                          'typerex-!ppss-in-literal-p)))
(defalias 'typerex-in-comment-p
    (eval-and-compile (if typerex-use-syntax-ppss
                          'typerex-ppss-in-comment-p
                          'typerex-!ppss-in-comment-p)))
(defalias 'typerex-in-literal-or-comment-p
    (eval-and-compile (if typerex-use-syntax-ppss
                          'typerex-ppss-in-literal-or-comment-p
                          'typerex-!ppss-in-literal-or-comment-p)))
(defalias 'typerex-beginning-of-literal-or-comment
    (eval-and-compile (if typerex-use-syntax-ppss
                          'typerex-ppss-beginning-of-literal-or-comment
                          'typerex-!ppss-beginning-of-literal-or-comment)))
(defalias 'typerex-beginning-of-literal-or-comment-fast
    (eval-and-compile (if typerex-use-syntax-ppss
                          'typerex-ppss-beginning-of-literal-or-comment-fast
                          'typerex-!ppss-beginning-of-literal-or-comment-fast)))
(defalias 'typerex-backward-up-list
    ;; FIXME: not clear if moving out of a string/comment counts as 1 or no.
    (eval-and-compile (if typerex-use-syntax-ppss
                          'backward-up-list
                          'typerex-!ppss-backward-up-list)))

(defun typerex-false-=-p ()
  "Is the underlying `=' the first/second letter of an operator?"
  (or (memq (preceding-char) '(?: ?> ?< ?=))
      (char-equal ?= (char-after (1+ (point))))))

(defun typerex-at-phrase-break-p ()
  "Is the underlying `;' a phrase break?"
  (and (char-equal ?\; (following-char))
       (or (and (not (eobp))
                (char-equal ?\; (char-after (1+ (point)))))
           (char-equal ?\; (preceding-char)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           Font-lock in Emacs

;; Originally by Stefan Monnier

(defcustom typerex-font-lock-symbols nil
  "*Display fun and -> and such using symbols in fonts.
This may sound like a neat trick, but note that it can change the
alignment and can thus lead to surprises."
  :group 'ocp :type 'boolean)

(defvar typerex-font-lock-symbols-alist
  (cond ((and (fboundp 'make-char) (fboundp 'charsetp) (charsetp 'symbol))
         `(("fun" . ,(make-char 'symbol 108))
           ("sqrt" . ,(make-char 'symbol 214))
           ("not" . ,(make-char 'symbol 216))
           ("&&" . ,(make-char 'symbol 217))
           ("or" . ,(make-char 'symbol 218))
           ("||" . ,(make-char 'symbol 218))
           ("*." . ,(make-char 'symbol 183))
           ("/." . ,(make-char 'symbol 184))
           ("<=" . ,(make-char 'symbol 163))
           ("<-" . ,(make-char 'symbol 172))
           ("->" . ,(make-char 'symbol 174))
           (">=" . ,(make-char 'symbol 179))
           ("<>" . ,(make-char 'symbol 185))
           ("==" . ,(make-char 'symbol 186))
           ("<=>" . ,(make-char 'symbol 219))
           (":=" . ,(make-char 'symbol 220))
           ("=>" . ,(make-char 'symbol 222))
           ("infinity" . ,(make-char 'symbol 165))
           ;; Some greek letters for type parameters.
           ("'a" . ,(make-char 'symbol 97))
           ("'b" . ,(make-char 'symbol 98))
           ("'c" . ,(make-char 'symbol 103)) ; sic! 99 is chi, 103 is gamma
           ("'d" . ,(make-char 'symbol 100))
           ("'e" . ,(make-char 'symbol 101))
           ("'f" . ,(make-char 'symbol 102))
           ("'i" . ,(make-char 'symbol 105))
           ("'k" . ,(make-char 'symbol 107))
           ("'m" . ,(make-char 'symbol 109))
           ("'n" . ,(make-char 'symbol 110))
           ("'o" . ,(make-char 'symbol 111))
           ("'p" . ,(make-char 'symbol 112))
           ("'r" . ,(make-char 'symbol 114))
           ("'s" . ,(make-char 'symbol 115))
           ("'t" . ,(make-char 'symbol 116))
           ("'x" . ,(make-char 'symbol 120))))
        ((fboundp 'decode-char) ;; or a unicode font.
         `(("fun" . ,(decode-char 'ucs 955))
           ("sqrt" . ,(decode-char 'ucs 8730))
           ("not" . ,(decode-char 'ucs 172))
           ("&&" . ,(decode-char 'ucs 8896))
           ("or" . ,(decode-char 'ucs 8897))
           ("||" . ,(decode-char 'ucs 8897))
           ("*." . ,(decode-char 'ucs 215))
           ("/." . ,(decode-char 'ucs 247))
           ("->" . ,(decode-char 'ucs 8594))
           ("<-" . ,(decode-char 'ucs 8592))
           ("<=" . ,(decode-char 'ucs 8804))
           (">=" . ,(decode-char 'ucs 8805))
           ("<>" . ,(decode-char 'ucs 8800))
           ("==" . ,(decode-char 'ucs 8801))
           ("<=>" . ,(decode-char 'ucs 8660))
           (":=" . ,(decode-char 'ucs 8656))
           ("infinity" . ,(decode-char 'ucs 8734))
           ;; Some greek letters for type parameters.
           ("'a" . ,(decode-char 'ucs 945))
           ("'b" . ,(decode-char 'ucs 946))
           ("'c" . ,(decode-char 'ucs 947))
           ("'d" . ,(decode-char 'ucs 948))
           ("'e" . ,(decode-char 'ucs 949))
           ("'f" . ,(decode-char 'ucs 966))
           ("'i" . ,(decode-char 'ucs 953))
           ("'k" . ,(decode-char 'ucs 954))
           ("'m" . ,(decode-char 'ucs 956))
           ("'n" . ,(decode-char 'ucs 957))
           ("'o" . ,(decode-char 'ucs 969))
           ("'p" . ,(decode-char 'ucs 960))
           ("'r" . ,(decode-char 'ucs 961))
           ("'s" . ,(decode-char 'ucs 963))
           ("'t" . ,(decode-char 'ucs 964))
           ("'x" . ,(decode-char 'ucs 958))))))

(defun typerex-font-lock-compose-symbol (alist)
  "Compose a sequence of ascii chars into a symbol.
Regexp match data 0 points to the chars."
  ;; Check that the chars should really be composed into a symbol.
  (let* ((mbegin (match-beginning 0))
         (mend (match-end 0))
         (syntax (char-syntax (char-after mbegin))))
    (if (or (eq (char-syntax (or (char-before mbegin) ?\ )) syntax)
            (eq (char-syntax (or (char-after mend) ?\ )) syntax)
            (memq (get-text-property mbegin 'face)
                  '(typerex-doc-face font-lock-string-face
                    font-lock-comment-face)))
        ;; No composition for you. Let's actually remove any composition
        ;;   we may have added earlier and which is now incorrect.
        (remove-text-properties mbegin mend '(composition))
      ;; That's a symbol alright, so add the composition.
      (compose-region mbegin mend (cdr (assoc (match-string 0) alist)))))
  ;; Return nil because we're not adding any face property.
  nil)

(defun typerex-font-lock-symbols-keywords ()
  (when (fboundp 'compose-region)
    (let ((alist nil))
      (dolist (x typerex-font-lock-symbols-alist)
        (when (and (if (fboundp 'char-displayable-p)
                       (char-displayable-p (cdr x))
                     t)
                   (not (assoc (car x) alist))) ; not yet in alist.
          (push x alist)))
      (when alist
        `((,(regexp-opt (mapcar 'car alist) t)
           (0 (typerex-font-lock-compose-symbol ',alist))))))))

(defvar typerex-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "_" st)
    (modify-syntax-entry ?? ". p" st)
    (modify-syntax-entry ?~ ". p" st)
    (modify-syntax-entry ?: "." st)
    (modify-syntax-entry ?' "w" st) ; ' is part of words (for primes).
    (modify-syntax-entry
     ;; ` is punctuation or character delimiter (Caml Light compatibility).
     ?` (if typerex-support-camllight "\"" ".") st)
    (modify-syntax-entry ?\" "\"" st) ; " is a string delimiter
    (modify-syntax-entry ?\\ "\\" st)
    (modify-syntax-entry ?*  ". 23" st)
    (condition-case nil
        (progn
          (modify-syntax-entry ?\( "()1n" st)
          (modify-syntax-entry ?\) ")(4n" st))
      (error               ;XEmacs signals an error instead of ignoring `n'.
       (modify-syntax-entry ?\( "()1" st)
       (modify-syntax-entry ?\) ")(4" st)))
    st)
  "Syntax table in use in TypeRex mode buffers.")

(defmacro typerex-with-internal-syntax (&rest body)
  `(progn
     ;; Switch to a modified internal syntax.
     (modify-syntax-entry ?. "w" typerex-mode-syntax-table)
     (modify-syntax-entry ?_ "w" typerex-mode-syntax-table)
     (unwind-protect (progn ,@body)
       ;; Switch back to the interactive syntax.
       (modify-syntax-entry ?. "." typerex-mode-syntax-table)
       (modify-syntax-entry ?_ "_" typerex-mode-syntax-table))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  Font-Lock

;; XEmacs and Emacs have different documentation faces...
(defvar typerex-doc-face
  (if (facep 'font-lock-doc-face)
      'font-lock-doc-face 'font-lock-doc-string-face))

(unless typerex-use-syntax-ppss

  (defun typerex-fontify-buffer ()
    (font-lock-default-fontify-buffer)
    (typerex-fontify (point-min) (point-max)))

  (defun typerex-fontify-region (begin end &optional verbose)
    (font-lock-default-fontify-region begin end verbose)
    (typerex-fontify begin end))

  (defun typerex-fontify (begin end)
    (when (eq major-mode 'typerex-mode)
      (save-excursion
       (typerex-with-internal-syntax

        (let ((case-fold-search nil)
              (modified (buffer-modified-p))) ; Emacs hack (see below)
          (goto-char begin)
          (setq begin (line-beginning-position))
          (goto-char (1- end))
          (end-of-line)
          ;; Dirty hack to trick `font-lock-default-unfontify-region'
          (unless typerex-with-xemacs (forward-line 2))
          (setq end (point))

          (while (> end begin)
            (goto-char (1- end))
            (typerex-in-literal-or-comment)
            (cond
              ((cdr typerex-last-loc)
               (typerex-beginning-of-literal-or-comment)
               (put-text-property (max begin (point)) end 'face
                                  (if (looking-at
                                       "(\\*[Tt][Ee][Xx]\\|(\\*\\*[^*]")
                                      typerex-doc-face
                                      'font-lock-comment-face))
               (setq end (1- (point))))
              ((car typerex-last-loc)
               (typerex-beginning-of-literal-or-comment)
               (put-text-property (max begin (point)) end 'face
                                  'font-lock-string-face)
               (setq end (point)))
              (t (while (and typerex-cache-local
                             (or (> (caar typerex-cache-local) end)
                                 (eq 'b (cadar typerex-cache-local))))
                   (setq typerex-cache-local (cdr typerex-cache-local)))
                 (setq end (if typerex-cache-local
                               (caar typerex-cache-local) begin)))))
          (unless (or typerex-with-xemacs modified) ; properties taken
            (set-buffer-modified-p nil)))          ; too seriously...
        ))))
  ) ;; End of (unless typerex-use-syntax-ppss

(defconst typerex-font-lock-syntactic-keywords
  ;; Char constants start with ' but ' can also appear in identifiers.
  ;; Beware not to match things like '*)hel' or '"hel' since the first '
  ;; might be inside a string or comment.
  '(("\\<\\('\\)\\([^'\\\n]\\|\\\\.[^\\'\n \")]*\\)\\('\\)"
     (1 '(7)) (3 '(7)))))

(defun typerex-font-lock-syntactic-face-function (state)
  (if (nth 3 state) font-lock-string-face
    (let ((start (nth 8 state)))
      (if (and (> (point-max) (+ start 2))
               (eq (char-after (+ start 2)) ?*)
               (not (eq (char-after (+ start 3)) ?*)))
          ;; This is a documentation comment
          typerex-doc-face
        font-lock-comment-face))))

;; Initially empty, set in `typerex-install-font-lock'
(defvar typerex-font-lock-keywords ()
  "Font-Lock patterns for TypeRex mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    Keymap

(defvar typerex-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "|" 'typerex-electric-pipe)
    (define-key map ")" 'typerex-electric-rp)
    (define-key map "}" 'typerex-electric-rc)
    (define-key map "]" 'typerex-electric-rb)
    (define-key map "\M-q" 'typerex-indent-phrase)
    (define-key map "\C-c\C-q" 'typerex-indent-phrase)
    (define-key map "\M-\C-\\" 'indent-region)
    (define-key map "\C-c\C-a" 'typerex-find-alternate-file)
    (define-key map "\C-c\C-c" 'compile)
    (define-key map "\C-xnd" 'typerex-narrow-to-phrase)
    (define-key map "\M-\C-x" 'typerex-eval-phrase)
    (define-key map "\C-x\C-e" 'typerex-eval-phrase)
    (define-key map "\C-c\C-e" 'typerex-eval-phrase)
    (define-key map "\C-c\C-r" 'typerex-eval-region)
    (define-key map "\C-c\C-b" 'typerex-eval-buffer)
    (define-key map "\C-c\C-s" 'typerex-run-caml)
    (define-key map "\C-c\C-i" 'typerex-interrupt-caml)
    (define-key map "\C-c\C-k" 'typerex-kill-caml)
    (define-key map "\C-c\C-n" 'typerex-next-phrase)
    (define-key map "\C-c\C-p" 'typerex-previous-phrase)
    (define-key map [(backspace)] 'backward-delete-char-untabify)
    (define-key map [(control c) (home)] 'typerex-move-inside-module-or-class-opening)
    (define-key map [(control c) (control down)] 'typerex-next-phrase)
    (define-key map [(control c) (control up)] 'typerex-previous-phrase)
    (define-key map [(meta control down)]  'typerex-next-phrase)
    (define-key map [(meta control up)] 'typerex-previous-phrase)
    (define-key map [(meta control n)]  'typerex-next-phrase)
    (define-key map [(meta control p)] 'typerex-previous-phrase)
    (define-key map [(meta control h)] 'typerex-mark-phrase)
    (define-key map "\C-c`" 'typerex-interactive-next-error-source)
    (define-key map "\C-c?" 'typerex-interactive-next-error-source)
    (define-key map "\C-c.c" 'typerex-insert-class-form)
    (define-key map "\C-c.b" 'typerex-insert-begin-form)
    (define-key map "\C-c.f" 'typerex-insert-for-form)
    (define-key map "\C-c.w" 'typerex-insert-while-form)
    (define-key map "\C-c.i" 'typerex-insert-if-form)
    (define-key map "\C-c.l" 'typerex-insert-let-form)
    (define-key map "\C-c.m" 'typerex-insert-match-form)
    (define-key map "\C-c.t" 'typerex-insert-try-form)
    (when typerex-with-caml-mode-p
      ;; Trigger caml-types
      (define-key map [?\C-c ?\C-t] 'caml-types-show-type)  ; "type"
      (define-key map [?\C-c ?\C-f] 'caml-types-show-call)  ; "function"
      (define-key map [?\C-c ?\C-l] 'caml-types-show-ident) ; "let"
      ;; To prevent misbehavior in case of error during exploration.
      (define-key map [?\C-c mouse-1] 'caml-types-mouse-ignore)
      (define-key map [?\C-c down-mouse-1] 'caml-types-explore)
      ;; Trigger caml-help
      (define-key map [?\C-c ?i] 'ocaml-add-path)
      (define-key map [?\C-c ?\[] 'ocaml-open-module)
      (define-key map [?\C-c ?\]] 'ocaml-close-module)
      (define-key map [?\C-c ?h] 'caml-help)
      (define-key map [?\C-c ?\t] 'typerex-complete))
    map)
  "Keymap used in TypeRex mode.")

(defconst typerex-font-lock-syntax
  `((?_ . "w") (?` . ".")
    ,@(unless typerex-use-syntax-ppss
        '((?\" . ".") (?\( . ".") (?\) . ".") (?* . "."))))
  "Syntax changes for Font-Lock.")

(defvar typerex-mode-abbrev-table ()
  "Abbrev table used for TypeRex mode buffers.")
(defun typerex-define-abbrev (keyword)
  (define-abbrev typerex-mode-abbrev-table keyword keyword 'typerex-abbrev-hook))
(if typerex-mode-abbrev-table ()
    (setq typerex-mode-abbrev-table (make-abbrev-table))
  (mapc 'typerex-define-abbrev
        '("module" "class" "functor" "object" "type" "val" "inherit"
          "include" "virtual" "constraint" "exception" "external" "open"
          "method" "and" "initializer" "to" "downto" "do" "done" "else"
          "begin" "end" "let" "in" "then" "with"))
  (setq abbrevs-changed nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              The major mode

;;;###autoload (add-to-list 'auto-mode-alist '("\\.ml\\w?\\'" . typerex-mode))

;;;###autoload
(defun typerex-mode ()
  "Major mode for editing Caml code.

Dedicated to Emacs and XEmacs, version 21 and higher. Provides
- automatic indentation and compilation interface (Tuareg implementation)
- font/color highlighting (new implementation)
- automatic identifier completion (experimental)
- identifier browsing
- refactoring

Report bugs, remarks and questions to OCamlPro.


For customization purposes, you should use `typerex-mode-hook'
\(run for every file) or `typerex-load-hook' (run once) and not patch
the mode itself. You should add to your configuration file something like:
  (add-hook 'typerex-mode-hook
            (lambda ()
               ... ; your customization code
            ))
For example you can change the indentation of some keywords, the
`electric' flags, Font-Lock colors... Every customizable variable is
documented, use `C-h-v' or look at the mode's source code.

`custom-ocp.el' is a sample customization file for standard changes.
You can append it to your `.emacs' or use it as a tutorial.

`M-x camldebug' FILE starts the Caml debugger camldebug on the executable
FILE, with input and output in an Emacs buffer named *camldebug-FILE*.

A TypeRex Interactive Mode to evaluate expressions in a toplevel is included.
Type `M-x typerex-run-caml' or see special-keys below.

For the best indentation experience, some elementary rules must be followed.
  - Because the `function' keyword has a special indentation (to handle
    case matches) use the `fun' keyword when no case match is performed.
  - In OCaml, `;;' is no longer necessary for correct indentation,
    except before top level phrases not introduced by `type', `val', `let'
    etc. (i.e., phrases used for their side-effects or to be executed
    in a top level.)
  - Long sequences of `and's may slow down indentation slightly, since
    some computations (few) require to go back to the beginning of the
    sequence. Some very long nested blocks may also lead to slow
    processing of `end's, `else's, `done's...
  - Multiline strings are handled properly, but you may prefer string
    concatenation `^' to break long strings (the C-j keystroke can help).
  - Comment indentation is often a matter of taste and context, yet
    sophisticated heuristics provide reasonable indentation in most cases.
    When inserting a comment right before the code it refers to, it is
    generally expected that this comment will be aligned with the folowing
    code; to enforce this, leave a blank line before the comment.

Known bugs:
  - When writting a line with mixed code and comments, avoid putting
    comments at the beginning or middle of the text. More precisely,
    writing comments immediately after `=' or parentheses then writing
    some more code on the line leads to indentation errors. You may write
    `let x (* blah *) = blah' but should avoid `let x = (* blah *) blah'.

Short cuts for the TypeRex mode:
\\{typerex-mode-map}

Short cuts for interactions with the toplevel:
\\{typerex-interactive-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'typerex-mode)
  (setq mode-name "TypeRex")
  (use-local-map typerex-mode-map)
  (set-syntax-table typerex-mode-syntax-table)
  (setq local-abbrev-table typerex-mode-abbrev-table)

  ;; Initialize the TypeRex menu
  (typerex-build-menu)

  ;; Initialize indentation regexps
  (typerex-make-indentation-regexps)

  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^[ \t]*$\\|\\*)$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "(* ")
  (make-local-variable 'comment-end)
  (setq comment-end " *)")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "(\\*+[ \t]*")
  (make-local-variable 'comment-multi-line)
  (setq comment-multi-line t)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments nil)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'typerex-indent-command)
  (unless typerex-use-syntax-ppss
    (add-hook 'before-change-functions 'typerex-before-change-function nil t))
  (make-local-variable 'normal-auto-fill-function)
  (setq normal-auto-fill-function 'typerex-auto-fill-function)

  (when (featurep 'imenu)
    (setq imenu-prev-index-position-function 'typerex-imenu-prev-index-position
          imenu-extract-index-name-function 'typerex-imenu-extract-index-name))

  ;; Hooks for typerex-mode, use them for typerex-mode configuration
  (typerex-install-font-lock)
  (run-hooks 'typerex-mode-hook)
  (when typerex-use-abbrev-mode (abbrev-mode 1))
  (message nil))

(defun typerex-install-font-lock ()
  (setq
   typerex-font-lock-keywords
   `(,@(and (typerex-editing-ls3)
            '(("\\<\\(let[ \t\n]+\\(clock\\|node\\|static\\)\\|present\\|automaton\\|where\\|match\\|with\\|do\\|done\\|unless\\|until\\|reset\\|every\\)\\>"
               0 typerex-font-lock-governing-face nil nil)))
     ("\\<\\(external\\|open\\|include\\|rule\\|s\\(ig\\|truct\\)\\|module\\|functor\\|with[ \t\n]+\\(type\\|module\\)\\|val\\|type\\|method\\|virtual\\|constraint\\|class\\|in\\|inherit\\|initializer\\|let\\|rec\\|object\\|and\\|begin\\|end\\)\\>"
      0 typerex-font-lock-governing-face nil nil)
     ,@(and typerex-support-metaocaml
            '(("\\.<\\|>\\.\\|\\.~\\|\\.!"
               0 typerex-font-lock-multistage-face nil nil)))
     ("\\<\\(false\\|true\\)\\>" 0 font-lock-constant-face nil nil)
     ("\\<\\(as\\|do\\(ne\\|wnto\\)?\\|else\\|for\\|if\\|mutable\\|new\\|p\\(arser\\|rivate\\)\\|t\\(hen\\|o\\|ry\\)\\|wh\\(en\\|ile\\)\\|match\\|with\\|lazy\\|exception\\|raise\\|failwith[f]?\\|exit\\|assert\\|fun\\(ction\\)?\\)\\>"
      0 font-lock-keyword-face nil nil)
     ,@(if (typerex-editing-ls3)
           '(("\\<\\(merge\\|when\\|emit\\|period\\)\\>"
              0 font-lock-keyword-face nil nil)
             ("[][;,()|{}]\\|[@^!:*=<>&/%+~?#---]\\.?\\|\\.\\.\\.*\\|\\<\\(asr\\|asl\\|lsr\\|lsl\\|l?or\\|l?and\\|lxor\\|l?not\\|mod\\|of\\|ref\\|fby\\|pre\\|last\\|at\\)\\>"
              0 typerex-font-lock-operator-face nil nil)
             ("\\<\\(\\(method\\([ \t\n]+\\(private\\|virtual\\)\\)?\\)\\([ \t\n]+virtual\\)?\\|val\\([ \t\n]+mutable\\)?\\|external\\|and\\|class\\|let\\([ \t\n]+\\(rec\\|clock\\|node\\|static\\)\\)?\\)\\>[ \t\n]*\\(['_[:lower:]]\\(\\w\\|[._]\\)*\\)\\>[ \t\n]*\\(\\(\\w\\|[()_?~.'*:--->]\\)+\\|=[ \t\n]*fun\\(ction\\)?\\>\\)"
              9 font-lock-function-name-face keep nil))
           '(("[][;,()|{}]\\|[@^!:*=<>&/%+~?#---]\\.?\\|\\.\\.\\.*\\|\\<\\(asr\\|asl\\|lsr\\|lsl\\|l?or\\|l?and\\|lxor\\|l?not\\|mod\\|of\\|ref\\)\\>"
              0 typerex-font-lock-operator-face nil nil)
             ("\\<\\(\\(method\\([ \t\n]+\\(private\\|virtual\\)\\)?\\)\\([ \t\n]+virtual\\)?\\|val\\([ \t\n]+mutable\\)?\\|external\\|and\\|class\\|let\\([ \t\n]+rec\\)?\\)\\>[ \t\n]*\\(['_[:lower:]]\\(\\w\\|[._]\\)*\\)\\>[ \t\n]*\\(\\(\\w\\|[()_?~.'*:--->]\\)+\\|=[ \t\n]*fun\\(ction\\)?\\>\\)"
              8 font-lock-function-name-face keep nil)))
     ("\\<method\\([ \t\n]+\\(private\\|virtual\\)\\)?\\>[ \t\n]*\\(\\(\\w\\|[_,?~.]\\)*\\)"
      3 font-lock-function-name-face keep nil)
     ("\\<\\(fun\\(ction\\)?\\)\\>[ \t\n]*\\(\\(\\w\\|[_ \t()*,]\\)+\\)"
      3 font-lock-variable-name-face keep nil)
     ,@(if (typerex-editing-ls3)
           '(("\\<\\(reset\\|do\\|val\\([ \t\n]+mutable\\)?\\|external\\|and\\|class\\|let\\([ \t\n]+rec\\)?\\)\\>[ \t\n]*\\(\\(\\w\\|[_,?~.]\\)*\\)"
              4 font-lock-variable-name-face keep nil)
             ("\\<\\(reset\\|do\\|val\\([ \t\n]+mutable\\)?\\|external\\|method\\|and\\|class\\|let\\([ \t\n]+\\(rec\\|clock\\|node\\|static\\)\\)?\\)\\>[ \t\n]*\\(\\(\\w\\|[_,?~.]\\)*\\)\\>\\(\\(\\w\\|[->_ \t,?~.]\\|(\\(\\w\\|[--->_ \t,?~.=]\\)*)\\)*\\)"
              7 font-lock-variable-name-face keep nil))
           '(("\\<\\(val\\([ \t\n]+mutable\\)?\\|external\\|and\\|class\\|let\\([ \t\n]+rec\\)?\\)\\>[ \t\n]*\\(\\(\\w\\|[_,?~.]\\)*\\)"
              4 font-lock-variable-name-face keep nil)
             ("\\<\\(val\\([ \t\n]+mutable\\)?\\|external\\|method\\|and\\|class\\|let\\([ \t\n]+rec\\)?\\)\\>[ \t\n]*\\(\\(\\w\\|[_,?~.]\\)*\\)\\>\\(\\(\\w\\|[->_ \t,?~.]\\|(\\(\\w\\|[--->_ \t,?~.=]\\)*)\\)*\\)"
              6 font-lock-variable-name-face keep nil)))
     ( "\\<\\(open\\|\\(class\\([ \t\n]+type\\)?\\)\\([ \t\n]+virtual\\)?\\|inherit\\|include\\|module\\([ \t\n]+\\(type\\|rec\\)\\)?\\|type\\)\\>[ \t\n]*\\(['~?]*\\([_--->.* \t]\\|\\w\\|(['~?]*\\([_--->.,* \t]\\|\\w\\)*)\\)*\\)"
           7 font-lock-type-face keep nil)
     ,@(and (typerex-editing-ls3)
            '(("\\<val\\>[ \t\n]*\\w*[ \t\n]*::[ \t\n]*\\(['~?]*\\([_--->.* \t]\\|\\w\\|(['~?]*\\([_--->.,* \t]\\|\\w\\)*)\\)*\\)"
               1 font-lock-type-face keep nil)))
     ("[^:>=]:[ \t\n]*\\(['~?]*\\([_--->.* \t]\\|\\w\\|(['~?]*\\([_--->.,* \t]\\|\\w\\)*)\\)*\\)"
      1 font-lock-type-face keep nil)
     ("\\<\\([A-Z]\\w*\\>\\)[ \t]*\\." 1 font-lock-type-face keep nil)
     ("\\<\\([?~]?[_[:alpha:]]\\w*\\)[ \t\n]*:[^:>=]"
      1 font-lock-variable-name-face keep nil)
     ("\\<exception\\>[ \t\n]*\\(\\<[_[:alpha:]]\\w*\\>\\)"
      1 font-lock-variable-name-face keep nil)
     ("^#\\w+\\>" 0 font-lock-preprocessor-face t nil)
     ,@(and typerex-font-lock-symbols
            (typerex-font-lock-symbols-keywords))))
  (setq font-lock-defaults
        (list*
         'typerex-font-lock-keywords (not typerex-use-syntax-ppss) nil
         typerex-font-lock-syntax nil
         '(font-lock-syntactic-keywords
           . typerex-font-lock-syntactic-keywords)
         '(parse-sexp-lookup-properties
           . t)
         '(font-lock-syntactic-face-function
           . typerex-font-lock-syntactic-face-function)
         (unless typerex-use-syntax-ppss
           '((font-lock-fontify-region-function
              . typerex-fontify-region)))))
  (when (and (boundp 'font-lock-fontify-region-function)
             (not typerex-use-syntax-ppss))
    (make-local-variable 'font-lock-fontify-region-function)
    (setq font-lock-fontify-region-function 'typerex-fontify-region)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Error processing

(require 'compile)

;; In some versions of Emacs, the regexps in
;; compilation-error-regexp-alist do not match the error messages when
;; the language is not English. Hence we add a regexp.

(defconst typerex-error-regexp
  "^[^\0-@]+ \"\\([^\"\n]+\\)\", [^\0-@]+ \\([0-9]+\\)[-,:]"
  "Regular expression matching the error messages produced by (o)camlc.")

(when (boundp 'compilation-error-regexp-alist)
  (or (assoc typerex-error-regexp
             compilation-error-regexp-alist)
      (setq compilation-error-regexp-alist
            (cons (list typerex-error-regexp 1 2)
                  compilation-error-regexp-alist))))

;; A regexp to extract the range info.

(defconst typerex-error-chars-regexp
  ".*, .*, [^\0-@]+ \\([0-9]+\\)-\\([0-9]+\\):"
  "Regexp matching the char numbers in an error message produced by (o)camlc.")

;; Wrapper around next-error.

;; itz 04-21-96 instead of defining a new function, use defadvice
;; that way we get our effect even when we do \C-x` in compilation buffer

(defadvice next-error (after typerex-next-error activate)
 "Read the extra positional information provided by the Caml compiler.

Puts the point and the mark exactly around the erroneous program
fragment. The erroneous fragment is also temporarily highlighted if
possible."
 (when (eq major-mode 'typerex-mode)
   (let ((beg nil) (end nil))
     (with-current-buffer compilation-last-buffer
       (save-excursion
         (goto-char (window-point (get-buffer-window (current-buffer) t)))
         (when (looking-at typerex-error-chars-regexp)
           (setq beg (string-to-number (typerex-match-string 1))
                 end (string-to-number (typerex-match-string 2))))))
     (beginning-of-line)
     (when beg
       (setq beg (+ (point) beg) end (+ (point) end))
       (goto-char beg) (push-mark end t t)))))

(defvar typerex-interactive-error-regexp
  (concat "\\(\\("
          "Toplevel input:"
          "\\|Entr.e interactive:"
          "\\|Characters [0-9-]*:"
          "\\|The global value [^ ]* is referenced before being defined."
          "\\|La valeur globale [^ ]* est utilis.e avant d'.tre d.finie."
          "\\|Reference to undefined global"
          "\\|The C primitive \"[^\"]*\" is not available."
          "\\|La primitive C \"[^\"]*\" est inconnue."
          "\\|Cannot find \\(the compiled interface \\)?file"
          "\\|L'interface compil.e [^ ]* est introuvable."
          "\\|Le fichier [^ ]* est introuvable."
          "\\|Exception non rattrap.e:"
          "\\|Uncaught exception:"
          "\\)[^#]*\\)" )
  "Regular expression matching the error messages produced by Caml.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Indentation stuff

(eval-and-compile
  (defconst typerex-no-more-code-this-line-regexp "[ \t]*\\((\\*\\|$\\)"
    "Regexp matching lines which have no more code:
 blanks + (maybe) comment start."))

(defmacro typerex-no-code-after (rex)
  `(eval-when-compile (concat ,rex typerex-no-more-code-this-line-regexp)))

(defconst typerex-no-code-this-line-regexp
  (concat "^" typerex-no-more-code-this-line-regexp))

(defun typerex-ro (&rest words) (concat "\\<" (regexp-opt words t) "\\>"))

(defconst typerex-extra-unindent-regexp
  (concat "\\(" (typerex-ro "with" "fun" "function" "parse" "parser")
          "\\|\\[" typerex-no-more-code-this-line-regexp "\\)")
  "Regexp for keywords needing extra indentation to compensate for case matches.")

(defconst typerex-ls3-extras (concat "\\|" (typerex-ro "automaton" "present")))

(defconst typerex-extra-unindent-regexp-ls3
  (concat typerex-extra-unindent-regexp typerex-ls3-extras)
  "Regexp for keywords needing extra indentation to compensate for case matches.")

(defun typerex-give-extra-unindent-regexp ()
  (if (typerex-editing-ls3)
      typerex-extra-unindent-regexp-ls3
    typerex-extra-unindent-regexp))

(defconst typerex-keyword-regexp
  (concat (typerex-ro "object" "initializer" "and" "constraint" "class"
                     "match" "module" "method" "mutable" "sig" "struct" "begin"
                     "else" "exception" "external" "to" "then" "try" "type"
                     "virtual" "val" "while" "when" "with" "if" "in" "inherit"
                     "for" "fun" "functor" "function" "let" "do" "downto"
                     "parse" "parser" "rule" "of")
          "\\|->\\|[;,|]")
  "Regexp for all recognized keywords.")

(defconst typerex-keyword-regexp-ls3
  (concat typerex-keyword-regexp "\\|"
          (typerex-ro "where" "automaton" "present" "fby" "pre" "last" "merge"
                     "when" "reset" "every" "emit" "until" "unless" "period"
                     "at"))
  "Regexp for all recognized keywords.
For synchronous programming.")

(defun typerex-give-keyword-regexp ()
  (if (typerex-editing-ls3)
      typerex-keyword-regexp-ls3
    typerex-keyword-regexp))

(defconst typerex-match-pipe-kwop-regexp
  (concat (typerex-ro "and" "function" "type" "with" "parse" "parser")
          "\\|[[({=]\\||[^!]")
  "Regexp for keywords supporting case match.")

(defconst typerex-match-pipe-kwop-regexp-ls3
  (concat typerex-match-pipe-kwop-regexp typerex-ls3-extras)
  "Regexp for keywords supporting case match.
For synchronous programming.")

(defun typerex-give-match-pipe-kwop-regexp ()
  (if (typerex-editing-ls3)
      typerex-match-pipe-kwop-regexp-ls3
    typerex-match-pipe-kwop-regexp))

(defconst typerex-operator-regexp "[---+*/=<>@^&|]\\|:>\\|::\\|\\<\\(or\\|l\\(and\\|x?or\\|s[lr]\\)\\|as[lr]\\|mod\\)\\>"
  "Regexp for all operators.")

(defconst typerex-matching-keyword-regexp
  (typerex-ro "and" "do" "done" "then" "else" "end" "in" "down" "downto")
  "Regexp matching Caml keywords which act as end block delimiters.")

(defconst typerex-extra-ls3-keyword-regexp
  (typerex-ro "where" "unless" "until" "every")
  "Additional Lucid Synchrone keywords.")

(defconst typerex-matching-keyword-regexp-ls3
  (concat typerex-matching-keyword-regexp "\\|" typerex-extra-ls3-keyword-regexp)
  "Regexp matching Caml keywords which act as end block delimiters
For synchronous programming.")

(defun typerex-give-matching-keyword-regexp ()
  (let ((rxp (if (typerex-editing-ls3)
                 typerex-matching-keyword-regexp-ls3
               typerex-matching-keyword-regexp)))
    (if typerex-support-metaocaml
        (concat rxp "\\|>\\.")
      rxp)))

(defconst typerex-matching-kwop-regexp
  (concat typerex-matching-keyword-regexp
          "\\|\\<with\\>\\|[|>]?\\]\\|>?}\\|[|)]\\|;;")
  "Regexp matching Caml keywords or operators which act as end block delimiters.")

(defconst typerex-matching-kwop-regexp-ls3
  (concat typerex-matching-kwop-regexp "\\|" typerex-extra-ls3-keyword-regexp)
  "Regexp matching Caml keywords or operators which act as end block delimiters.
For synchronous programming.")

(defun typerex-give-matching-kwop-regexp ()
  (if (typerex-editing-ls3)
      typerex-matching-kwop-regexp-ls3
    typerex-matching-kwop-regexp))

(defconst typerex-block-regexp
  (concat (typerex-ro "for" "while" "do" "if" "begin" "sig" "struct" "object")
          "\\|[][(){}]\\|\\*)"))

(defconst typerex-find-kwop-regexp
  (concat typerex-matching-keyword-regexp "\\|" typerex-block-regexp))

(defconst typerex-find-kwop-regexp-ls3
  (concat typerex-find-kwop-regexp "\\|"
          (typerex-ro "where" "automaton" "present" "match")))

(defun typerex-give-find-kwop-regexp ()
  (if (typerex-editing-ls3)
      typerex-find-kwop-regexp-ls3
    typerex-find-kwop-regexp))

(defconst typerex-governing-phrase-regexp
  (typerex-ro "val" "type" "method" "module" "constraint" "class" "inherit"
             "initializer" "external" "exception" "open" "let" "object"
             "include")
  "Regexp matching ocp phrase delimitors.")

(defconst typerex-keyword-alist
  '(("module" . typerex-default-indent)
    ("class" . typerex-class-indent)
    ("sig" . typerex-sig-struct-indent)
    ("struct" . typerex-sig-struct-indent)
    ("method" . typerex-method-indent)
    ("object" . typerex-begin-indent)
    ("begin" . typerex-begin-indent)
    (".<" . typerex-begin-indent)
    ("for" . typerex-for-while-indent)
    ("while" . typerex-for-while-indent)
    ("do" . typerex-do-indent)
    ("val" . typerex-val-indent)
    ("fun" . typerex-fun-indent)
    ("if" . typerex-if-then-else-indent)
    ("then" . typerex-if-then-else-indent)
    ("else" . typerex-if-then-else-indent)
    ("let" . typerex-let-indent)
    ("match" . typerex-match-indent)
    ("try" . typerex-try-indent)
    ("rule" . typerex-rule-indent)

    ;; Case match keywords
    ("function" . typerex-function-indent)
    ("with" . typerex-with-indent)
    ("parse" . typerex-with-indent)
    ("parser" . typerex-with-indent)
    ("automaton" . typerex-with-indent)
    ("present" . typerex-with-indent)
    ("type" . typerex-type-indent) ; sometimes, `type' acts like a case match

    ;; Assume default indentation for other keywords and operators
    )
  "Association list of indentation values based on governing keywords.")

(defconst typerex-leading-kwop-alist
  '(("|" . typerex-find-pipe-match)
    ("}" . typerex-find-match)
    (">}" . typerex-find-match)
    (">." . typerex-find-match)
    (")" . typerex-find-match)
    ("]" . typerex-find-match)
    ("|]" . typerex-find-match)
    (">]" . typerex-find-match)
    ("end" . typerex-find-match)
    ("done" . typerex-find-done-match)
    ("unless" . typerex-find-done-match)
    ("until" . typerex-find-done-match)
    ("every" . typerex-find-done-match)
    ("in" . typerex-find-in-match)
    ("where" . typerex-find-in-match)
    ("with" . typerex-find-with-match)
    ("else" . typerex-find-else-match)
    ("then" . typerex-find-then-match)
    ("do" . typerex-find-do-match)
    ("to" . typerex-find-match)
    ("downto" . typerex-find-match)
    ("and" . typerex-find-and-match))
  "Association list used in TypeRex mode for skipping back over nested blocks.")

(defun typerex-find-leading-kwop-match (kwop)
  (funcall (cdr (assoc kwop typerex-leading-kwop-alist))))

(defconst typerex-binding-regexp "\\(\\<and\\>\\|(*\\<let\\>\\)")

(defun typerex-assoc-indent (kwop &optional look-for-let-or-and)
  "Return relative indentation of the keyword given in argument."
  (let ((ind (or (symbol-value (cdr (assoc kwop typerex-keyword-alist)))
                 typerex-default-indent))
        (looking-let-or-and (and look-for-let-or-and
                                 (looking-at typerex-binding-regexp))))
    (if (string-match (typerex-give-extra-unindent-regexp) kwop)
        (- (if (and typerex-let-always-indent
                    looking-let-or-and (< ind typerex-let-indent))
               typerex-let-indent ind)
           typerex-pipe-extra-unindent)
      ind)))

(defun typerex-in-monadic-op-p (&optional pos)
  (unless pos (setq pos (point)))
  (and (char-equal ?> (char-before pos))
       (char-equal ?> (char-before (1- pos)))))

(defconst typerex-meaningful-word-regexp
  "[^ \t\n_[:alnum:]]\\|\\<\\(\\w\\|_\\)+\\>\\|\\*)")
(defun typerex-find-meaningful-word ()
  "Look back for a word, skipping comments and blanks.
Returns the actual text of the word, if found."
  (let ((found nil) (kwop nil) (pt (point)))
    (while (and (not found)
                (re-search-backward typerex-meaningful-word-regexp
                                    (point-min) t))
      (setq kwop (typerex-match-string 0))
      (cond ((and (or (string= kwop "|") (string= kwop "=") (string= kwop ">"))
                  (typerex-in-monadic-op-p))
             (backward-char 2)
             (setq kwop (concat ">>" kwop)))
            ((and (string= kwop ">") (char-equal ?- (char-before)))
             (backward-char)
             (setq kwop "->")))
      (when (= pt (point))
        (error "typerex-find-meaningful-word: inf loop at %d, kwop=%s" pt kwop))
      (setq pt (point))
      (if kwop
          (if (typerex-in-comment-p)
              (typerex-beginning-of-literal-or-comment-fast)
            (setq found t))
        (setq found t)))
    (if found kwop (goto-char (point-min)) nil)))

(defun typerex-make-find-kwop-regexp (kwop-regexp)
  "Make a custom indentation regexp."
  (concat (typerex-give-find-kwop-regexp) "\\|" kwop-regexp))

;; Dynamic regexps (for language changes, see `typerex-editing-ls3')
(defvar typerex-find-comma-match-regexp nil)
(defvar typerex-find-with-match-regexp nil)
(defvar typerex-find-in-match-regexp nil)
(defvar typerex-find-else-match-regexp nil)
(defvar typerex-find-do-match-regexp nil)
(defvar typerex-find-=-match-regexp nil)
(defvar typerex-find-pipe-match-regexp nil)
(defvar typerex-find-arrow-match-regexp nil)
(defvar typerex-find-semicolon-match-regexp nil)
(defvar typerex-find-phrase-indentation-regexp nil)
(defvar typerex-find-phrase-indentation-break-regexp nil)
(defvar typerex-find-phrase-indentation-class-regexp nil)
(defvar typerex-compute-argument-indent-regexp nil)
(defvar typerex-compute-normal-indent-regexp nil)
(defvar typerex-find-module-regexp nil)
(defvar typerex-find-pipe-bang-match-regexp nil)
(defvar typerex-find-monadic-match-regexp nil)

;; Static regexps
(defconst typerex-find-and-match-regexp
  (concat (typerex-ro "do" "done" "else" "end" "in" "then" "down" "downto"
                     "for" "while" "do" "if" "begin" "sig" "struct" "class"
                     "rule" "exception" "let" "in" "type" "val" "module")
          "\\|[][(){}]\\|\\*)"))
(defconst typerex-find-phrase-beginning-regexp
  (concat (typerex-ro "end" "type" "module" "sig" "struct" "class"
                     "exception" "open" "let")
          "\\|^#[ \t]*[a-z][_a-z]*\\>\\|;;"))
(defconst typerex-find-phrase-beginning-and-regexp
  (concat "\\<\\(and\\)\\>\\|" typerex-find-phrase-beginning-regexp))
(defconst typerex-back-to-paren-or-indentation-regexp
  "[][(){}]\\|\\.<\\|>\\.\\|\\*)\\|^[ \t]*\\(.\\|\n\\)")

;; Specific regexps for module/class detection
(defconst typerex-inside-module-or-class-opening
  (typerex-ro "struct" "sig" "object"))
(defconst typerex-inside-module-or-class-opening-full
  (concat typerex-inside-module-or-class-opening "\\|"
          (typerex-ro "module" "class")))
(defconst typerex-inside-module-or-class-regexp
  (concat (typerex-give-matching-keyword-regexp) "\\|"
          typerex-inside-module-or-class-opening))

(defun typerex-make-indentation-regexps ()
  "Initialisation of specific indentation regexp.
Gathered here for memoization and dynamic reconfiguration purposes."
  (setq
   typerex-find-comma-match-regexp
    (typerex-make-find-kwop-regexp
     (concat (typerex-ro "and" "match" "begin" "else" "exception" "then" "try"
                        "with" "or" "fun" "function" "let" "do")
             "\\|->\\|[[{(]"))
   typerex-find-with-match-regexp
    (typerex-make-find-kwop-regexp
     (concat (typerex-ro "match" "try" "module" "begin" "with" "type")
             "\\|[[{(]"))
   typerex-find-in-match-regexp
    (typerex-make-find-kwop-regexp (typerex-ro "let" "open"))
   typerex-find-else-match-regexp
    (typerex-make-find-kwop-regexp ";")
   typerex-find-do-match-regexp
    (typerex-make-find-kwop-regexp "->")
   typerex-find-=-match-regexp
    (typerex-make-find-kwop-regexp
     (concat (typerex-ro "val" "let" "method" "module" "type" "class" "when"
                        "if" "in" "do")
             "\\|="))
   typerex-find-pipe-match-regexp
    (typerex-make-find-kwop-regexp (typerex-give-match-pipe-kwop-regexp))
   typerex-find-arrow-match-regexp
    (typerex-make-find-kwop-regexp
     (concat (typerex-ro "external" "type" "val" "method" "let" "with" "fun"
                        "function" "functor" "class" "parser")
             "\\|[|;]"))
   typerex-find-semicolon-match-regexp
    (typerex-make-find-kwop-regexp
     (concat ";" typerex-no-more-code-this-line-regexp "\\|->\\|"
             (typerex-ro "let" "method" "with" "try" "initializer")))
   typerex-find-phrase-indentation-regexp
    (typerex-make-find-kwop-regexp
     (concat typerex-governing-phrase-regexp "\\|" (typerex-ro "and" "every")))
   typerex-find-phrase-indentation-break-regexp
    (concat typerex-find-phrase-indentation-regexp "\\|;;")
   typerex-find-phrase-indentation-class-regexp
    (concat (typerex-give-matching-keyword-regexp) "\\|\\<class\\>")
   typerex-compute-argument-indent-regexp
    (typerex-make-find-kwop-regexp
     (concat (typerex-give-keyword-regexp) "\\|="))
   typerex-compute-normal-indent-regexp
    (concat typerex-compute-argument-indent-regexp "\\|^.[ \t]*")
   typerex-find-module-regexp
    (typerex-make-find-kwop-regexp "\\<module\\>")
   typerex-find-pipe-bang-match-regexp
    (concat typerex-find-comma-match-regexp "\\|=")
   typerex-find-monadic-match-regexp
    (concat typerex-block-regexp "\\|\\([;=]\\)\\|\\(->\\)\\|"
            (typerex-ro "val" "let" "method" "module" "type" "class" "when"
                       "if" "in" "do" "done" "end"))))

(defun typerex-strip-trailing-whitespace (string)
  (if (string-match "[ \t]*\\'" string)
      (substring string 0 (match-beginning 0))
    string))

(defun typerex-find-kwop-pos (kr do-not-skip-regexp may-terminate-early)
  "Look back for a keyword or operator matching KR (short for kwop regexp).
Skips blocks etc...

Ignore occurences inside literals and comments.
If found, return the actual text of the keyword or operator."
  (let ((found nil)
        (kwop nil) pos
        (kwop-regexp (if typerex-support-metaocaml
                         (concat kr "\\|\\.<\\|>\\.")
                       kr)))
    (while (and (not found)
                (setq pos (re-search-backward kwop-regexp (point-min) t))
                (setq kwop (typerex-strip-trailing-whitespace
                            ;; for trailing blanks after a semicolon
                            (typerex-match-string 0))))
      (cond
       ((typerex-in-literal-or-comment-p)
        (typerex-beginning-of-literal-or-comment-fast))
       ((looking-at "[]})]")
        (typerex-backward-up-list))
       ((typerex-at-phrase-break-p)
        (setq found t))
       ((and do-not-skip-regexp (looking-at do-not-skip-regexp))
        (if (and (string= kwop "|") (char-equal ?| (preceding-char)))
            (backward-char)
          (setq found t)))
       ((looking-at (typerex-give-matching-keyword-regexp))
        (let ((mkwop (typerex-find-leading-kwop-match (typerex-match-string 0))))
          (when (and may-terminate-early (string-match kwop-regexp mkwop))
            (setq found t))))
       (t
        (setq found t))))
    (if found (list kwop pos) (goto-char (point-min)) nil)))

(defun typerex-find-kwop (kr &optional do-not-skip-regexp)
  (car (typerex-find-kwop-pos kr do-not-skip-regexp nil)))

(defun typerex-find-match ()
  (let ((kwop (typerex-find-kwop (typerex-give-find-kwop-regexp))))
    (when (string= kwop "then")
      (typerex-find-then-match)
      (typerex-find-match))
    kwop))

(defun typerex-find-comma-match ()
  (car (typerex-find-kwop-pos typerex-find-comma-match-regexp nil t)))

(defun typerex-find-pipe-bang-match ()
  (destructuring-bind (kwop pos)
      (typerex-find-kwop-pos typerex-find-pipe-bang-match-regexp nil t)
    ;; when matched "if ... then", kwop is "then" but point is at "if"
    (goto-char pos)   ; go back to kwop for typerex-indent-to-code
    (if (looking-at "\\[|") "[|" kwop)))

(defun typerex-monadic-operator-p (word)
  (and (or (string= ">>=" word) (string= ">>|" word) (string= ">>>" word))
       word))

(defun typerex-ignorable-arrow-p ()
  (save-excursion
    (or (typerex-monadic-operator-p (typerex-find-arrow-match))
        (looking-at (typerex-give-extra-unindent-regexp)))))

(defun typerex-find-monadic-match ()
  (let (kwop)
    (while (or (null kwop)
               (and (string= kwop "=") (typerex-in-monadic-op-p)))
      (when kwop (typerex-backward-char 2))
      (setq kwop (typerex-find-kwop typerex-find-monadic-match-regexp))
      (when (and (string= kwop "->") (typerex-ignorable-arrow-p))
        (setq kwop nil)))
    kwop))

(defun typerex-find-with-match ()
  (typerex-find-kwop typerex-find-with-match-regexp))

(defun typerex-find-in-match ()
  (let ((kwop (typerex-find-kwop typerex-find-in-match-regexp "\\<and\\>")))
    (cond
     ((string= kwop "and")
      (typerex-find-in-match))
     (t
      kwop))))

(defconst typerex-find-arrow-match-regexp-ls3
  (concat typerex-find-arrow-match-regexp typerex-ls3-extras))
(defun typerex-give-find-arrow-match-regexp ()
  (if (typerex-editing-ls3)
      typerex-find-arrow-match-regexp-ls3
    typerex-find-arrow-match-regexp))

(defconst typerex-find-then-match-skip-regexp-ls3
  (regexp-opt '("->" "unless" "until") t))
(defconst typerex-find-then-match-regexp-ls3
  (typerex-make-find-kwop-regexp typerex-find-then-match-skip-regexp-ls3))
(defconst typerex-find-then-match-regexp
  (typerex-make-find-kwop-regexp "\\(->\\)"))
(defun typerex-find-then-kwop ()
  (let ((ls3 (typerex-editing-ls3)))
    (typerex-find-kwop
     (if ls3 typerex-find-then-match-regexp-ls3 typerex-find-then-match-regexp)
     (if ls3 typerex-find-then-match-regexp-ls3 "\\(->\\)"))))
(defun typerex-find-then-match ()
  (let ((kwop (typerex-find-then-kwop)))
    (cond ((string= kwop "if")
           (let ((back (point)))
             (typerex-back-to-paren-or-indentation)
             (if (looking-at "else[ \t]*\\((\\*.*\\*)\\)*[ \t]*if")
                 "else if"
               (goto-char back)
               kwop)))
          (t kwop))))

(defun typerex-find-then-else-match ()
  (let ((kwop (typerex-find-then-kwop)))
    (cond
     ((string= kwop "if")
      (let ((pos (point)))
        (if (and (not (typerex-in-indentation-p))
                 (string= "else" (typerex-find-meaningful-word)))
            "else"
          (goto-char pos)
          kwop)))
     (t
      kwop))))

(defun typerex-find-else-match ()
  (let ((kwop (typerex-find-kwop typerex-find-else-match-regexp
                                "\\<then\\>")))
    (cond
     ((string= kwop "then")
      (typerex-find-then-else-match))
     ((string= kwop ";")
      (typerex-find-semicolon-match)
      (typerex-find-else-match)))))

(defconst typerex-do-match-stop-regexp (typerex-ro "down" "downto"))
(defun typerex-find-do-match ()
  (let ((kwop (typerex-find-kwop typerex-find-do-match-regexp
                                typerex-do-match-stop-regexp)))
    (if (or (string= kwop "to") (string= kwop "downto"))
        (typerex-find-match)
      kwop)))

(defconst typerex-done-match-stop-regexp (typerex-ro "and" "do"))
(defun typerex-find-done-match ()
  (let ((kwop (typerex-find-kwop (typerex-give-find-kwop-regexp)
                                typerex-done-match-stop-regexp)))
    (cond
     ((string= kwop "and")
      (typerex-find-and-match))
     ((string= kwop "done")
      (typerex-find-done-match)
      (typerex-find-done-match))
     ((string= kwop "do")
      (typerex-find-do-match))
     (t
      kwop))))

(defconst typerex-and-stop-regexp-ls3 (typerex-ro "and" "do" "where"))
(defun typerex-give-and-stop-regexp ()
  (if (typerex-editing-ls3)
      typerex-and-stop-regexp-ls3
    "\\<and\\>"))

(defun typerex-find-and-match ()
  (let* ((kwop (typerex-find-kwop
                typerex-find-and-match-regexp
                (typerex-give-and-stop-regexp)))
         (old-point (point)))
    (cond
     ((or (string= kwop "type") (string= kwop "module"))
      (let ((kwop2 (typerex-find-meaningful-word)))
        (cond ((string= kwop2 "with")
               kwop2)
              ((string= kwop2 "and")
               (typerex-find-and-match))
              ((and (string= kwop "module")
                    (string= kwop2 "let"))
               kwop2)
              (t (goto-char old-point) kwop))))
     (t kwop))))

(defconst typerex-=-stop-regexp-ls3
  (concat (typerex-ro "and" "do" "in" "where") "\\|="))
(defconst typerex-=-stop-regexp (concat (typerex-ro "and" "in") "\\|="))
(defun typerex-give-=-stop-regexp ()
  (if (typerex-editing-ls3)
      typerex-=-stop-regexp-ls3
    typerex-=-stop-regexp))

(defun typerex-find-=-match ()
  (let ((kwop (typerex-find-kwop
               typerex-find-=-match-regexp
               (typerex-give-=-stop-regexp))))
    (cond
     ((string= kwop "and")
      (typerex-find-and-match))
     ((and (string= kwop "=")
           (not (typerex-false-=-p)))
      (while (and (string= kwop "=")
                  (not (typerex-false-=-p)))
        (setq kwop (typerex-find-=-match)))
      kwop)
     (t kwop))))

(defconst typerex-if-when-regexp (typerex-ro "if" "when"))
(defun typerex-if-when-= ()
  (save-excursion
    (typerex-find-=-match)
    (looking-at typerex-if-when-regexp)))

(defconst typerex-captive-regexp
  (typerex-ro "let" "if" "when" "module" "type" "class"))
(defun typerex-captive-= ()
  (save-excursion
    (typerex-find-=-match)
    (looking-at typerex-captive-regexp)))

(defconst typerex-pipe-stop-regexp
  (concat (typerex-ro "and" "with") "\\||"))
(defconst typerex-pipe-stop-regexp-ls3
  (concat typerex-pipe-stop-regexp typerex-ls3-extras))
(defun typerex-give-pipe-stop-regexp ()
  (if (typerex-editing-ls3)
      typerex-pipe-stop-regexp-ls3
    typerex-pipe-stop-regexp))

(defun typerex-find-pipe-match ()
  (let ((kwop
         (let ((k (typerex-find-kwop
                   typerex-find-pipe-match-regexp
                   (typerex-give-pipe-stop-regexp))))
           (if (and k (string-match "|[^!]" k))
               "|" k)))
        (old-point (point)))
    (cond
     ((string= kwop "and")
      (setq old-point (point))
      (setq kwop (typerex-find-and-match))
      (if (not (string= kwop "do"))
          (goto-char old-point)
        (setq kwop (typerex-find-arrow-match)))
      kwop)
     ((and (string= kwop "|")
           (looking-at "|[^|]")
           (typerex-in-indentation-p))
      kwop)
     ((string= kwop "|") (typerex-find-pipe-match))
     ((and (string= kwop "=")
           (or (looking-at (typerex-no-code-after "="))
               (typerex-false-=-p)
               (not (string= (save-excursion (typerex-find-=-match))
                             "type"))))
      (typerex-find-pipe-match))
     ((string= kwop "parse")
      (if (and (typerex-editing-camllex)
               (save-excursion
                 (string= (typerex-find-meaningful-word) "=")))
          kwop
        (typerex-find-pipe-match)))
     (t
      kwop))))

(defun typerex-find-arrow-match ()
  (let ((kwop (typerex-find-kwop (typerex-give-find-arrow-match-regexp)
                                "\\<with\\>")))
    (cond
     ((string= kwop "|")
      (if (typerex-in-indentation-p)
          kwop
        (progn (forward-char -1) (typerex-find-arrow-match))))
     ((string= kwop "fun")
      (let ((pos (point)))
        (or (typerex-monadic-operator-p (typerex-find-meaningful-word))
            (progn (goto-char pos) kwop))))
     ((not (string= kwop ":"))
      kwop)
     ;; If we get this far, we know we're looking at a colon.
     ((or (char-equal (char-before) ?:)
          (char-equal (char-after (1+ (point))) ?:)
          (char-equal (char-after (1+ (point))) ?>))
      (typerex-find-arrow-match))
     ;; Patch by T. Freeman
     (t
      (let ((oldpoint (point))
            (match (typerex-find-arrow-match)))
        (if (looking-at ":")
            match
          (progn
            ;; Go back to where we were before the recursive call.
            (goto-char oldpoint)
            kwop)))))))

(defconst typerex-semicolon-match-stop-regexp
  (typerex-ro "and" "do" "end" "in" "with"))
(defconst typerex-no-code-after-paren-regexp
  (typerex-no-code-after "[[{(][|<]?"))
(defun typerex-semicolon-indent-kwop-point (&optional leading-semi-colon)
  ;; return (kwop kwop-point indentation)
  (let ((kwop (typerex-find-kwop typerex-find-semicolon-match-regexp
                                typerex-semicolon-match-stop-regexp))
        (point (point)))
    ;; We don't need to find the keyword matching `and' since we know it's `let'!
    (list
     (cond
       ((string= kwop ";")
        (forward-line 1)
        (while (or (typerex-in-comment-p)
                   (looking-at typerex-no-code-this-line-regexp))
          (forward-line 1))
        (back-to-indentation)
        (current-column))
       ((and leading-semi-colon
             (looking-at "\\((\\|\\[[<|]?\\|{<?\\)[ \t]*[^ \t\n]")
             (not (looking-at typerex-no-code-after-paren-regexp)))
        (current-column))
       ;; ((looking-at (typerex-no-code-after "\\((\\|\\[[<|]?\\|{<?\\)"))
       ;;  (+ (current-column) typerex-default-indent))
       ((looking-at (typerex-no-code-after "\\<begin\\>\\|\\((\\|\\[[<|]?\\|{<?\\)"))
        (if (typerex-in-indentation-p)
            (+ (current-column) typerex-default-indent)
          (typerex-indent-from-previous-kwop)))
       ((looking-at "\\(\\.<\\|(\\|\\[[<|]?\\|{<?\\)") ; paren with subsequent text
        (typerex-search-forward-paren)
        (current-column))
       ((string= kwop "method")
        (+ (typerex-paren-or-indentation-column) typerex-method-indent))
       ((string= kwop "->")
        (if (save-excursion
              (typerex-find-arrow-match)
              (or (looking-at "\\<fun\\>\\||")
                  (looking-at (typerex-give-extra-unindent-regexp))))
            (typerex-paren-or-indentation-indent)
          (typerex-find-semicolon-match)))
       ((string= kwop "end")
        (typerex-find-match)
        (typerex-find-semicolon-match))
       ((string= kwop "in")
        (typerex-find-in-match)
        (+ (current-column) typerex-in-indent))
       ((string= kwop "where")
        (typerex-find-in-match)
        (+ (typerex-paren-or-indentation-column) typerex-in-indent))
       ((string= kwop "let")
        (+ (current-column) typerex-let-indent))
       ((string= kwop "try")
        (forward-char 3) (skip-syntax-forward " ")
        (current-column))
       (t (typerex-paren-or-indentation-indent)))
     kwop point)))

(defun typerex-find-semicolon-match (&optional leading-semi-colon)
  (car (typerex-semicolon-indent-kwop-point leading-semi-colon)))

(defmacro typerex-reset-and-kwop (kwop)
  `(when (and ,kwop (string= ,kwop "and"))
     (setq ,kwop (typerex-find-and-match))))

(defconst typerex-phrase-regexp-1 (typerex-ro "module" "type"))
(defconst typerex-phrase-regexp-2 (typerex-ro "and" "let" "module" "with"))
(defconst typerex-phrase-regexp-3
  (typerex-ro "and" "end" "every" "in" "with"))
(defun typerex-find-phrase-indentation (&optional phrase-break)
  (if (and (looking-at typerex-phrase-regexp-1) (> (point) (point-min))
           (save-excursion
             (typerex-find-meaningful-word)
             (looking-at typerex-phrase-regexp-2)))
      (progn
        (typerex-find-meaningful-word)
        (+ (current-column) typerex-default-indent))
    (let ((looking-at-and (looking-at "\\<and\\>"))
          (kwop (typerex-find-kwop
                 (if phrase-break
                     typerex-find-phrase-indentation-break-regexp
                   typerex-find-phrase-indentation-regexp)
                 typerex-phrase-regexp-3))
          (tmpkwop nil) (curr nil))
      (typerex-reset-and-kwop kwop)
      (cond ((not kwop) (current-column))
            ((string= kwop "every")
             (if (typerex-editing-ls3)
                 (progn
                   (typerex-find-done-match)
                   (typerex-find-phrase-indentation phrase-break)
                   (current-column))
               (typerex-find-phrase-indentation phrase-break)))
            ((string= kwop "end")
             (if (not (save-excursion
                        (setq tmpkwop (typerex-find-match))
                        (setq curr (point))
                        (string= tmpkwop "object")))
                 (progn
                   (typerex-find-match)
                   (typerex-find-phrase-indentation phrase-break))
               (typerex-find-kwop typerex-find-phrase-indentation-class-regexp)
               (current-column)))
            ((and (string= kwop "with")
                  (not (save-excursion
                         (setq tmpkwop (typerex-find-with-match))
                         (setq curr (point))
                         (string= tmpkwop "module"))))
             (goto-char curr)
             (typerex-find-phrase-indentation phrase-break))
            ((and (string= kwop "in")
                  (not (save-excursion
                         (setq tmpkwop (typerex-find-in-match))
                         (typerex-reset-and-kwop tmpkwop)
                         (setq curr (point))
                         (and (string= tmpkwop "let")
                              (not (typerex-looking-at-internal-let))))))
             (goto-char curr)
             (typerex-find-phrase-indentation phrase-break))
            ((typerex-at-phrase-break-p)
             (end-of-line)
             (typerex-skip-blank-and-comments)
             (current-column))
            ((string= kwop "let")
             (if (typerex-looking-at-internal-let)
                 (typerex-find-phrase-indentation phrase-break)
                 (current-column)))
            ((string= kwop "with")
             (current-column))
            ((string= kwop "end")
             (current-column))
            ((or (string= kwop "in") (string= kwop "where"))
             (typerex-find-in-match)
             (current-column))
            ((string= kwop "class")
             (typerex-paren-or-indentation-column))
            ((looking-at typerex-inside-module-or-class-opening)
             (+ (typerex-paren-or-indentation-column)
                (typerex-assoc-indent kwop)))
            ((or (string= kwop "type") (string= kwop "module"))
             (if (or (typerex-looking-at-false-type)
                     (typerex-looking-at-false-module))
                 (if looking-at-and
                     (current-column)
                   (if (string= "and" (typerex-find-meaningful-word))
                       (progn
                         (typerex-find-and-match)
                         (typerex-find-phrase-indentation phrase-break))
                     (typerex-find-phrase-indentation phrase-break)))
               (current-column)))
            ((looking-at "\\(\\.<\\|(\\|\\[[<|]?\\|{<?\\)[ \t]*[^ \t\n]")
             (typerex-search-forward-paren)
             (current-column))
            ((string= kwop "open") ; compatible with Caml Light `#open'
             (typerex-paren-or-indentation-column))
            (t (current-column))))))

(defconst typerex-paren-or-indentation-stop-regexp
  (typerex-ro "and" "do" "in" "with"))
(defun typerex-back-to-paren-or-indentation ()
  "Search backwards for the first open paren in line, or skip to indentation.
Returns t iff skipped to indentation."
  (if (or (bolp) (typerex-in-indentation-p))
      (progn (back-to-indentation) t)
    (let ((kwop (typerex-find-kwop
                 typerex-back-to-paren-or-indentation-regexp
                 typerex-paren-or-indentation-stop-regexp))
          (retval))
      (when (string= kwop "with")
        (let ((with-point (point)))
          (setq kwop (typerex-find-with-match))
          (if (or (string= kwop "match") (string= kwop "try"))
              (typerex-find-kwop typerex-back-to-paren-or-indentation-regexp
                                "\\<and\\>")
            (setq kwop "with") (goto-char with-point))))
      (setq retval
            (cond
             ((string= kwop "with") nil)
             ((or (string= kwop "in") (string= kwop "do"))
              (typerex-in-indentation-p))
;            ((looking-at "[[{(]") (typerex-search-forward-paren) nil)
;            ((looking-at "\\.<")
;             (if typerex-support-metaocaml
;                 (progn
;                   (typerex-search-forward-paren) nil)
;               (typerex-back-to-paren-or-indentation)))
             (t (back-to-indentation) t)))
      (cond
    ;   ((looking-at "|[^|]")
    ;    (re-search-forward "|[^|][ \t]*") nil)
       ((or (string= kwop "in") (string= kwop "do"))
        (typerex-find-in-match)
        (typerex-back-to-paren-or-indentation)
        (if (looking-at "\\<\\(let\\|and\\)\\>")
            (forward-char typerex-in-indent)) nil)
       (t retval)))))

(defun typerex-paren-or-indentation-column ()
  (typerex-back-to-paren-or-indentation)
  (current-column))

(defun typerex-paren-or-indentation-indent ()
  (+ (typerex-paren-or-indentation-column) typerex-default-indent))

(defun typerex-search-forward-paren ()
  (re-search-forward "\\(\\.<\\|(\\|\\[[<|]?\\|{<?\\)[ \t]*"))

(defun typerex-add-default-indent (leading-operator)
  (if leading-operator 0 typerex-default-indent))

(defconst typerex-internal-let-regexp
  (concat "[[({;=]\\|"
           (typerex-ro "begin" "open" "if" "in" "do" "try" "then" "else"
                      "match" "while" "when")))
(defun typerex-looking-at-internal-let ()
  (save-excursion
    (typerex-find-meaningful-word)
    (and (not (typerex-at-phrase-break-p))
         (not (and typerex-support-metaocaml
                   (char-equal ?. (following-char))
                   (char-equal ?> (preceding-char))))
         (or (looking-at typerex-internal-let-regexp)
             (looking-at typerex-operator-regexp)))))

(defconst typerex-false-module-regexp (typerex-ro "and" "let" "with"))
(defun typerex-looking-at-false-module ()
  (save-excursion
    (typerex-find-meaningful-word)
    (looking-at typerex-false-module-regexp)))

(defun typerex-looking-at-false-sig-struct ()
  (save-excursion
    (typerex-find-module)
    (looking-at "\\<module\\>\\|(")))

(defconst typerex-false-type-regexp (typerex-ro "and" "class" "module" "with"))
(defun typerex-looking-at-false-type ()
  (save-excursion
    (typerex-find-meaningful-word)
    (looking-at typerex-false-type-regexp)))

(defun typerex-looking-at-in-let ()
  (save-excursion
    (string= (typerex-find-meaningful-word) "in")))

(defun typerex-find-module ()
  (typerex-find-kwop typerex-find-module-regexp))

(defun typerex-indent-from-previous-kwop ()
  (let* ((start-pos (point))
         (kwop (typerex-find-argument-kwop-non-blank t))
         (captive= (and (string= kwop "=") (typerex-captive-=)))
         (kwop-pos (point)))
    (forward-char (length kwop))
    (typerex-skip-blank-and-comments)
    (cond ((or (not captive=)
               (/= (point) start-pos)) ; code between paren and kwop
           (goto-char start-pos)
           (typerex-paren-or-indentation-indent))
          (t
           (goto-char kwop-pos)
           (when (string= kwop "=")
             (setq kwop (typerex-find-=-match)))
           (+ typerex-default-indent
              (if (assoc kwop typerex-leading-kwop-alist)
                  (typerex-compute-kwop-indent kwop)
                  (current-column)))))))

(defun typerex-find-colon-typespec (start-pos)
  (let* ((old-pos (point))
         (new-pos (search-forward ":" start-pos t)))
    (when new-pos
      (backward-char 1)
      (skip-syntax-backward " ")
      (skip-syntax-backward "w")
      (skip-syntax-backward " ")
      (let ((char (char-before)))
        (cond ((or (char-equal char ??) (char-equal char ?~))
               (goto-char old-pos) nil)
              (t (goto-char new-pos) t))))))

(defun typerex-indent-from-paren (leading-operator start-pos)
  (cond
   ((looking-at (typerex-no-code-after "\\(\\(\\.<\\|(\\|\\[[<|]?\\|{<?\\)\\)"))
    (cond ((typerex-in-indentation-p)
           (+ typerex-default-indent
              (current-column)))
          ((typerex-find-colon-typespec start-pos)
           (if (looking-at typerex-no-code-this-line-regexp)
               (typerex-paren-or-indentation-indent)
             (typerex-skip-blank-and-comments)
             (current-column)))
          (t (typerex-indent-from-previous-kwop))))
   ((looking-at "\\<begin\\>")
    (typerex-paren-or-indentation-indent))
   ((looking-at "([ \t]*\\(\\w\\)")
    (goto-char (match-beginning 1))
    (current-column))
   (t
    (+ (typerex-add-default-indent leading-operator)
       (current-column)))))

(defun typerex-skip-to-next-form (old-point)
  (while (and (not (looking-at typerex-no-more-code-this-line-regexp))
              (< (point) old-point)) ; do not go beyond old-point
    (forward-sexp 1))
  (typerex-skip-blank-and-comments)
  (typerex-back-to-paren-or-indentation))

(defun typerex-find-argument-kwop (leading-operator)
  (typerex-find-kwop (if leading-operator
                      typerex-compute-argument-indent-regexp
                      typerex-compute-normal-indent-regexp)
                    (typerex-give-keyword-regexp)))

(defun typerex-find-argument-kwop-clean (leading-operator)
  (let (kwop)
    (while (or (progn (setq kwop (typerex-find-argument-kwop leading-operator))
                      (typerex-reset-and-kwop kwop)
                      nil)
               (and (string= kwop "=") (typerex-false-=-p))
               (and (looking-at typerex-no-code-this-line-regexp)
                    (not (= (point) (point-min))))))
    kwop))

(defun typerex-find-argument-kwop-non-blank (leading-operator)
  (let ((kwop "") (point (1+ (point))))
    (while (and (> point (point)) (string= "" kwop))
      (setq point (point)
            kwop (typerex-find-argument-kwop-clean leading-operator)))
    kwop))

(defun typerex-compute-argument-indent (leading-operator)
  (let* ((old-point (line-beginning-position))
         (kwop (typerex-find-argument-kwop-non-blank leading-operator))
         (match-end-point (+ (point) (length kwop)))) ; match-end is invalid!
    (cond
     ((and (string= kwop "->")
           (not (looking-at (typerex-no-code-after "->"))))
      (let (matching-kwop matching-pos)
        (save-excursion
          (setq matching-kwop (typerex-find-arrow-match))
          (setq matching-pos (point)))
        (cond
         ((string= matching-kwop ":")
          (goto-char matching-pos)
          (typerex-find-arrow-match) ; matching `val' or `let'
          (+ (current-column) typerex-val-indent))
         ((or (string= matching-kwop "val") (string= matching-kwop "let"))
          (+ (current-column) typerex-val-indent))
         ((string= matching-kwop "|")
          (goto-char matching-pos)
          (+ (typerex-add-default-indent leading-operator)
             (current-column)
             (- typerex-pipe-extra-unindent)
             typerex-default-indent))
         (t
          (+ (typerex-paren-or-indentation-column)
             (typerex-add-default-indent leading-operator))))))
     ((string= kwop "fun")
      (+ (typerex-paren-or-indentation-column)
         (typerex-add-default-indent leading-operator)
         (typerex-assoc-indent kwop)))
     ((<= old-point (point))
      (+ (typerex-add-default-indent leading-operator)
         (current-column)))
     (t
      (goto-char match-end-point) ; skip kwop == (forward-char (length kwop))
      (typerex-skip-to-next-form old-point)
      (+ (typerex-add-default-indent
          (if (save-excursion (goto-char match-end-point)
                              (looking-at typerex-no-more-code-this-line-regexp))
              (or leading-operator (string= kwop "{")
                  (looking-at (typerex-no-code-after "[[:upper:]].*\\.")))
            (not (looking-at typerex-operator-regexp))))
         (current-column))))))

(defun typerex-compute-arrow-indent (start-pos)
  (let (kwop pos)
    (save-excursion (setq kwop (typerex-find-arrow-match) pos (point)))
    (cond ((string= kwop "|")
           (typerex-find-arrow-match)
           (+ (current-column) typerex-default-indent))
          ((or (string= kwop "val")
               (string= kwop "let"))
           (goto-char pos)
           (+ (current-column) typerex-val-indent))
          ((string= kwop "type")
           (goto-char pos)
           (+ (current-column) typerex-type-indent
              typerex-default-indent))
          ((string= kwop "(")
           (goto-char pos)
           (typerex-indent-after-next-char))
          ((or (string= kwop "{")
               (string= kwop ";"))
           (if (and (looking-at "->")
                    (search-backward ":" pos t))
               (typerex-indent-after-next-char)
             (typerex-back-to-paren-or-indentation)
             (current-column)))
          ((typerex-monadic-operator-p kwop)
           ;; find the last ">>=" or ">>>"
           ;; (goto-char pos)
           ;; (let ((back (point)))
           ;;   (while (typerex-monadic-operator-p (typerex-find-arrow-match))
           ;;     (setq back (point)))
           ;;   (goto-char back))
           ;; (if (not (re-search-backward
           ;;           (concat "(\\|" (typerex-give-keyword-regexp))
           ;;           (point-min) t))
           ;;     0
           ;;   (goto-char (match-end 0))
           ;;   (typerex-skip-blank-and-comments)
           ;;   (typerex-compute-indent))

           ;; this is not perfect, in particular, inside match.
           ;; (see example in sample.ml)
           ;; the problem is that we cannot skip an expression backwards.
           ;; workaround: wrap code in parens
           (destructuring-bind (indent kwop point)
               (typerex-semicolon-indent-kwop-point)
             (- indent
                (if (string= kwop "in")
                    typerex-in-indent 0))))
          (t (typerex-paren-or-indentation-indent)))))

(defun typerex-compute-keyword-indent (kwop leading-operator start-pos)
  (cond ((string= kwop ";")
         (if (looking-at (typerex-no-code-after ";"))
             (let* ((pos (point)) (indent (typerex-find-semicolon-match)))
               (if (looking-at typerex-phrase-regexp-1)
                   (progn
                     (goto-char start-pos)
                     (if (search-backward ":" pos t)
                         (typerex-indent-after-next-char)
                       indent))
                 indent))
           (typerex-paren-or-indentation-indent)))
        ((string= kwop ",")
         (if (looking-at (typerex-no-code-after ","))
             (let ((mkwop (typerex-find-comma-match)))
               (cond ((or (string= mkwop "[")
                          (string= mkwop "{")
                          (string= mkwop "("))
                      (forward-char 1) (skip-syntax-forward " ")
                      (current-column))
                     ((looking-at "[[{(]\\|\\.<")
                      (typerex-indent-from-paren t start-pos))
                     ((or (and (looking-at "[<|]")
                               (char-equal ?\[ (preceding-char)))
                          (and (looking-at "<")
                               (char-equal ?\{ (preceding-char))))
                      (typerex-backward-char)
                      (typerex-indent-from-paren t start-pos))
                     ((and (looking-at "\\<let\\>") (string= mkwop "in"))
                      (+ (current-column) typerex-in-indent))
                     (t (+ (typerex-paren-or-indentation-column)
                           (typerex-assoc-indent mkwop)))))
           (typerex-paren-or-indentation-indent)))
        ((looking-at "\\<begin\\>\\|->")
         (if (looking-at (typerex-no-code-after "\\(\\<begin\\>\\|->\\)"))
             (typerex-indent-from-paren leading-operator start-pos)
           (+ typerex-default-indent
              (typerex-indent-from-paren leading-operator start-pos))))
        ((or (string= kwop "let") (string= kwop "and"))
         (typerex-back-to-paren-or-indentation)
         (+ (typerex-paren-or-indentation-indent)
            (typerex-assoc-indent kwop t)))
        ((string= kwop "with")
         (if (save-excursion
               (let ((tmpkwop (typerex-find-with-match)))
                 (or (string= tmpkwop "module")
                     (string= tmpkwop "{"))))
             (typerex-paren-or-indentation-indent)
           (+ (typerex-paren-or-indentation-column)
              (* 2 typerex-default-indent) ; assume a missing first "|"
              (typerex-assoc-indent kwop t))))
        ((string-match "\\<\\(fun\\|of\\)\\>" kwop)
         (+ (typerex-paren-or-indentation-column)
            (typerex-add-default-indent leading-operator)
            (typerex-assoc-indent kwop t)))
        ((string-match (typerex-give-extra-unindent-regexp) kwop)
         (+ (typerex-paren-or-indentation-column)
            (typerex-assoc-indent kwop t)))
        ((string= kwop "in")
         (when (looking-at (typerex-no-code-after "\\<in\\>"))
           (typerex-find-in-match))
         (+ (current-column)
            typerex-in-indent))
        ((string-match (typerex-give-matching-kwop-regexp) kwop)
         (typerex-find-leading-kwop-match kwop)
         (if (typerex-in-indentation-p)
             (+ (current-column)
                (typerex-assoc-indent kwop t))
           (typerex-back-to-paren-or-indentation)
           (+ (typerex-paren-or-indentation-indent)
              (typerex-assoc-indent kwop t))))
        ((string= kwop "try")
         (forward-char 3)
         (if (looking-at typerex-no-more-code-this-line-regexp)
             (+ (current-column) -3 typerex-default-indent)
           (skip-syntax-forward " ")
           (+ (current-column) typerex-default-indent)))
        (t (+ (if (typerex-in-indentation-p)
                  (current-column)
                (typerex-paren-or-indentation-indent))
              (typerex-assoc-indent kwop t)))))

(defconst typerex-=-indent-regexp-1
  (typerex-ro "val" "let" "method" "module" "class" "when" "for" "if" "do"))

(defun typerex-compute-=-indent (start-pos)
  (let ((current-column-module-type nil) (kwop1 (typerex-find-=-match))
        (next-pos (point)))
    (+ (save-excursion
         (typerex-reset-and-kwop kwop1)
         (cond ((string= kwop1 "type")
                (typerex-find-meaningful-word)
                (cond ((looking-at "\\<module\\>")
                       (setq current-column-module-type (current-column))
                       typerex-default-indent)
                      ((looking-at "\\<\\(with\\|and\\)\\>")
                       (typerex-find-with-match)
                       (setq current-column-module-type (current-column))
                       typerex-default-indent)
                      (t (goto-char start-pos)
                         (beginning-of-line)
                         (+ (typerex-add-default-indent
                             (looking-at "[ \t]*[\[|]"))
                            typerex-type-indent))))
               ((looking-at typerex-=-indent-regexp-1)
                (let ((matched-string (typerex-match-string 0)))
                  (setq current-column-module-type (current-column))
                  (typerex-assoc-indent matched-string)))
               ((looking-at "\\<object\\>")
                (typerex-back-to-paren-or-indentation)
                (setq current-column-module-type (current-column))
                (+ (typerex-assoc-indent "object")
                   typerex-default-indent))
               ((looking-at typerex-no-code-after-paren-regexp)
                (setq current-column-module-type
                      (typerex-indent-from-paren nil next-pos))
                typerex-default-indent)
               (t (setq current-column-module-type
                        (typerex-paren-or-indentation-indent))
                  typerex-default-indent)))
       (or current-column-module-type
           (current-column)))))

(defun typerex-indent-after-next-char ()
  (forward-char 1)
  (typerex-skip-blank-and-comments)
  (current-column))

(defconst typerex-definitions-regexp
  (typerex-ro "and" "val" "type" "module" "class" "exception" "let")
  "Regexp matching definition phrases.")

(defun typerex-compute-normal-indent ()
  (let ((leading-operator (looking-at typerex-operator-regexp)))
    (beginning-of-line)
    (save-excursion
      (let ((start-pos (point))
            (kwop (typerex-find-argument-kwop-clean leading-operator)))
        (cond
          ((not kwop) (current-column))
          ((typerex-at-phrase-break-p)
           (typerex-find-phrase-indentation t))
          ((and (string= kwop "|") (not (char-equal ?\[ (preceding-char))))
           (typerex-backward-char)
           (+ (typerex-paren-or-indentation-indent)
              (typerex-add-default-indent leading-operator)))
          ((or (looking-at "[[{(]")
               (and (looking-at "[<|]")
                    (char-equal ?\[ (preceding-char))
                    (progn (typerex-backward-char) t))
               (and (looking-at "<")
                    (char-equal ?\{ (preceding-char))
                    (progn (typerex-backward-char) t)))
           (cond ((looking-at "{ *[A-Z]")
                  (forward-char 1) (skip-syntax-forward " ")
                  (current-column))
                 ((looking-at (typerex-no-code-after "[[{(][<|]?"))
                  (typerex-indent-from-paren leading-operator start-pos))
                 ((and leading-operator (string= kwop "("))
                  (typerex-indent-after-next-char))
                 (t (+ typerex-default-indent
                       (typerex-indent-from-paren leading-operator start-pos)))))
          ((looking-at "\\.<")
           (if (looking-at (typerex-no-code-after "\\.<"))
               (typerex-indent-from-paren leading-operator start-pos)
             (+ typerex-default-indent
                (typerex-indent-from-paren leading-operator start-pos))))
          ((looking-at "->")
           (typerex-compute-arrow-indent start-pos))
          ((looking-at (typerex-give-keyword-regexp))
           (typerex-compute-keyword-indent kwop leading-operator start-pos))
          ((and (string= kwop "=") (not (typerex-false-=-p))
                (or (null leading-operator)
                    ;; defining "=", not testing for equality
                    (string-match typerex-definitions-regexp
                                  (save-excursion
                                    (typerex-find-argument-kwop-clean t)))))
           (typerex-compute-=-indent start-pos))
          (nil 0)
          (t (typerex-compute-argument-indent leading-operator)))))))

(defun typerex-compute-pipe-indent (matching-kwop old-point)
  (cond
    ((string= matching-kwop "|")
     (typerex-back-to-paren-or-indentation)
     (current-column))
    ((and (string= matching-kwop "=")
          (not (typerex-false-=-p)))
     (re-search-forward "=[ \t]*")
     (current-column))
    ((and matching-kwop
          (looking-at (typerex-give-match-pipe-kwop-regexp)))
     (when (looking-at (typerex-give-extra-unindent-regexp))
       (typerex-back-to-paren-or-indentation))
     (+ (typerex-assoc-indent matching-kwop t)
        (typerex-add-default-indent (not (looking-at "|")))
        (current-column)
        (if (or (string= matching-kwop "type")
                (string= matching-kwop "["))
            0
            typerex-pipe-extra-unindent)))
    (t
     (goto-char old-point)
     (typerex-compute-normal-indent))))

(defun typerex-compute-paren-indent (paren-match-p old-point)
  (unless paren-match-p
    (typerex-search-forward-paren))
  (let ((looking-at-paren (char-equal ?\( (char-after))) (start-pos (point)))
    (when (or looking-at-paren
              (looking-at (typerex-no-code-after "\\(\{\\(.*with[ \t]*\\([[:upper:]].*\\.\\)?\\)?\\|\\[\\)")))
      (if (or (typerex-in-indentation-p)
              (save-excursion (string= ":" (typerex-find-meaningful-word))))
          (typerex-back-to-paren-or-indentation)
        (typerex-indent-from-previous-kwop))
      (when looking-at-paren
        (skip-chars-forward "( \t" start-pos))
      (while (and (looking-at "[([{]")
                  (> (scan-sexps (point) 1)
                     (save-excursion (goto-char old-point)
                                     (line-end-position))))
        (forward-char 1)
        (skip-syntax-forward " "))))
  (current-column))

(defun typerex-compute-kwop-indent-general (kwop matching-kwop)
  (let* ((looking-at-matching (looking-at matching-kwop))
         (extra-unindent        ; non-paren code before matching-kwop
          (unless (save-excursion
                    (skip-chars-backward "( \t" (line-beginning-position))
                    (bolp))
            (typerex-back-to-paren-or-indentation)
            t)))
    (+ (current-column)
       (typerex-add-default-indent
        (if extra-unindent
            (or (string= matching-kwop "struct")
                (string= matching-kwop "object")
                (string= matching-kwop "with")
                (string= kwop "end"))
            (or (not (string= kwop "then"))
                looking-at-matching))))))

(defun typerex-compute-kwop-indent (kwop)
  (when (string= kwop "rec")
    (setq kwop "and"))
  (let* ((old-point (point))
         (paren-match-p (looking-at "[|>]?[]})]\\|>\\."))
         (real-pipe (looking-at "|\\([^|]\\|$\\)"))
         (matching-kwop (typerex-find-leading-kwop-match kwop)))
    (cond ((string= kwop "|")
           (if real-pipe
               (typerex-compute-pipe-indent matching-kwop old-point)
             (goto-char old-point)
             (typerex-compute-normal-indent)))
          ((looking-at "[[{(][<|]?\\|\\.<")
           (typerex-compute-paren-indent paren-match-p old-point))
          ((string= kwop "with")
           (when (string= matching-kwop "type")
             (setq old-point (point)
                   matching-kwop (typerex-find-meaningful-word)))
           (while (string= matching-kwop "with")
             (typerex-find-with-match)
             (setq matching-kwop (typerex-find-leading-kwop-match kwop)))
           (cond ((or (string= matching-kwop "module")
                      (string= matching-kwop "struct"))
                  (typerex-paren-or-indentation-indent))
                 ((or (string= matching-kwop "try")
                      (string= matching-kwop "match"))
                  (typerex-compute-kwop-indent-general kwop matching-kwop))
                 (t (goto-char old-point)
                    (typerex-compute-kwop-indent-general kwop matching-kwop))))
          ((and (typerex-editing-ls3)
                (or (string= kwop "do")
                    (string= kwop "done")
                    (string= kwop "reset")
                    (string= kwop "unless")
                    (string= kwop "until")))
           (typerex-back-to-paren-or-indentation)
           (if (string= matching-kwop "->")
               (+ (current-column) typerex-default-indent)
             (current-column)))
          ((or (and (string= kwop "and")
                    (string= matching-kwop "reset"))
               (and (string= kwop "end")
                    (typerex-editing-ls3)
                    (or (string= matching-kwop "match")
                        (string= matching-kwop "automaton")
                        (string= matching-kwop "present"))))
           (if (typerex-in-indentation-p)
               (current-column)
             (typerex-paren-or-indentation-column)))
          ((string= kwop "in")
           (+ (current-column)
              (typerex-add-default-indent (string= matching-kwop "let"))))
          ((not (string= kwop "and")) ; pretty general case
           (typerex-compute-kwop-indent-general kwop matching-kwop))
          ((string= matching-kwop "with")
           (current-column))
          (t (typerex-paren-or-indentation-column)))))

(defun typerex-indent-to-code (beg-pos match)
  (unless (and (string= match "(")
               (search-forward "->" beg-pos t))
    (forward-char (length match)))
  (typerex-skip-blank-and-comments)
  (current-column))

(defun typerex-indent-command (&optional from-leading-star)
  "Indent the current line in TypeRex mode.

Compute new indentation based on Caml syntax."
  (interactive "*")
  (unless from-leading-star
    (typerex-auto-fill-insert-leading-star))
  (let ((case-fold-search nil))
   (typerex-with-internal-syntax
    (save-excursion
      (back-to-indentation)
      (indent-line-to (max 0 (typerex-compute-indent))))
    (when (typerex-in-indentation-p) (back-to-indentation)))))

(defconst typerex-sig-struct-regexp (typerex-ro "sig" "struct"))
(defconst typerex-top-level-command-regexp
  (concat "#" (typerex-ro "open" "load" "use")))
(defun typerex-compute-indent ()
  (save-excursion
    (cond
     ((typerex-in-comment-p)
      (cond
       ((looking-at "(\\*")
        (if typerex-indent-leading-comments
            (save-excursion
              (typerex-skip-blank-and-comments)
              (back-to-indentation)
              (current-column))
          (current-column)))
       ((looking-at "\\*\\**)")
        (typerex-beginning-of-literal-or-comment-fast)
        (if (typerex-leading-star-p)
            (+ (current-column)
               (if (save-excursion
                     (forward-line 1)
                     (back-to-indentation)
                     (looking-at "*")) 1
                 typerex-comment-end-extra-indent))
          (+ (current-column) typerex-comment-end-extra-indent)))
       (typerex-indent-comments
        (let ((star (and (typerex-leading-star-p)
                         (looking-at "\\*"))))
          (typerex-beginning-of-literal-or-comment-fast)
          (if star (re-search-forward "(") (re-search-forward "(\\*+[ \t]*"))
          (current-column)))
       (t (current-column))))
     ((typerex-in-literal-p)
      (current-column))
     ((or (looking-at "\\<let\\>") (looking-at "\\<open\\>"))
      (if (typerex-looking-at-internal-let)
          (if (typerex-looking-at-in-let)
              (progn
                (typerex-find-meaningful-word)
                (typerex-find-in-match)
                (current-column))
            (typerex-compute-normal-indent))
        (typerex-find-phrase-indentation)))
     ((or (looking-at typerex-governing-phrase-regexp)
          (looking-at ";;"))
      (typerex-find-phrase-indentation))
     ((and typerex-sig-struct-align (looking-at typerex-sig-struct-regexp))
      (if (string= (typerex-find-module) "module") (current-column)
        (typerex-paren-or-indentation-indent)))
     ((looking-at ";")
      (typerex-find-semicolon-match t))
     ((looking-at "|!")
      (typerex-indent-to-code (line-beginning-position)
                             (typerex-find-pipe-bang-match)))
     ((looking-at ">>[=>|]")
      (typerex-indent-to-code (line-beginning-position)
                             (typerex-find-monadic-match)))
     ((or (looking-at "%\\|;;")
          (and typerex-support-camllight (looking-at "#"))
          (looking-at typerex-top-level-command-regexp))
      0)
     ((or (looking-at (typerex-give-matching-kwop-regexp))
          (looking-at "\\<rec\\>")
          (and typerex-support-metaocaml
               (looking-at ">\\.")))
      (typerex-compute-kwop-indent (typerex-match-string 0)))
     (t (typerex-compute-normal-indent)))))

(defun typerex-split-string ()
  "Called whenever a line is broken inside a Caml string literal."
  (insert-before-markers "\\ ")
  (typerex-backward-char))

(defadvice newline-and-indent (around
                               typerex-newline-and-indent
                               activate)
  "Handle multi-line strings in TypeRex mode."
  (let ((hooked (and (eq major-mode 'typerex-mode) (typerex-in-literal-p)))
        (split-mark))
    (when hooked
      (setq split-mark (set-marker (make-marker) (point)))
      (typerex-split-string))
    ad-do-it
    (when hooked
      (goto-char split-mark)
      (set-marker split-mark nil))))

(defun typerex-electric-pipe ()
  "If inserting a | operator at beginning of line, reindent the line."
  (interactive "*")
  (let ((electric (and typerex-electric-indent
                       (typerex-in-indentation-p)
                       (not (typerex-in-literal-p))
                       (not (typerex-in-comment-p)))))
    (self-insert-command 1)
    (and electric
         (not (and (char-equal ?| (preceding-char))
                   (save-excursion
                     (typerex-backward-char)
                     (typerex-find-pipe-match)
                     (not (looking-at (typerex-give-match-pipe-kwop-regexp))))))
         (indent-according-to-mode))))

(defun typerex-electric-rp ()
  "If inserting a ) operator or a comment-end at beginning of line,
reindent the line."
  (interactive "*")
  (let ((electric (and typerex-electric-indent
                       (or (typerex-in-indentation-p)
                           (char-equal ?* (preceding-char)))
                       (not (typerex-in-literal-p))
                       (or (not (typerex-in-comment-p))
                           (save-excursion
                             (back-to-indentation)
                             (looking-at "\\*"))))))
    (self-insert-command 1)
    (and electric
         (indent-according-to-mode))))

(defun typerex-electric-rc ()
  "If inserting a } operator at beginning of line, reindent the line.

Reindent also if } is inserted after a > operator at beginning of line.
Also, if the matching { is followed by a < and this } is not preceded
by >, insert one >."
  (interactive "*")
  (let* ((prec (preceding-char))
         (look-bra (and typerex-electric-close-vector
                        (not (typerex-in-literal-or-comment-p))
                        (not (char-equal ?> prec))))
         (electric (and typerex-electric-indent
                        (or (typerex-in-indentation-p)
                            (and (char-equal ?> prec)
                                 (save-excursion (typerex-backward-char)
                                                 (typerex-in-indentation-p))))
                        (not (typerex-in-literal-or-comment-p)))))
    (self-insert-command 1)
    (when look-bra
      (save-excursion
        (let ((inserted-char
               (save-excursion
                 (typerex-backward-char)
                 (typerex-backward-up-list)
                 (cond ((looking-at "{<") ">")
                       (t "")))))
          (typerex-backward-char)
          (insert inserted-char))))
    (when electric (indent-according-to-mode))))

(defun typerex-electric-rb ()
  "If inserting a ] operator at beginning of line, reindent the line.

Reindent also if ] is inserted after a | operator at beginning of line.
Also, if the matching [ is followed by a | and this ] is not preceded
by |, insert one |."
  (interactive "*")
  (let* ((prec (preceding-char))
         (look-pipe-or-bra (and typerex-electric-close-vector
                                (not (typerex-in-literal-or-comment-p))
                                (not (and (char-equal ?| prec)
                                          (not (char-equal
                                                (save-excursion
                                                  (typerex-backward-char)
                                                  (preceding-char)) ?\[))))))
         (electric (and typerex-electric-indent
                        (or (typerex-in-indentation-p)
                            (and (char-equal ?| prec)
                                 (save-excursion (typerex-backward-char)
                                                 (typerex-in-indentation-p))))
                        (not (typerex-in-literal-or-comment-p)))))
    (self-insert-command 1)
    (when look-pipe-or-bra
      (save-excursion
        (let ((inserted-char
               (save-excursion
                 (typerex-backward-char)
                 (typerex-backward-up-list)
                 (cond ((looking-at "\\[|") "|")
                       (t "")))))
          (typerex-backward-char)
          (insert inserted-char))))
    (when electric (indent-according-to-mode))))

(defun typerex-abbrev-hook ()
  "If inserting a leading keyword at beginning of line, reindent the line."
  (unless (typerex-in-literal-or-comment-p)
    (let* ((bol (line-beginning-position))
           (kw (save-excursion
                 (and (re-search-backward "^[ \t]*\\(\\w\\|_\\)+\\=" bol t)
                      (typerex-match-string 1)))))
      (when kw
        (insert " ")
        (indent-according-to-mode)
        (backward-delete-char-untabify 1)))))

(defun typerex-skip-to-end-of-phrase ()
  (let ((old-point (point)))
    (when (and (string= (typerex-find-meaningful-word) ";")
               (char-equal (preceding-char) ?\;))
      (setq old-point (1- (point))))
    (goto-char old-point)
    (let ((kwop (typerex-find-meaningful-word)))
      (goto-char (+ (point) (length kwop))))))

(defun typerex-skip-blank-and-comments ()
  (skip-syntax-forward " ")
  (while (and (not (eobp)) (typerex-in-comment-p)
              (search-forward "*)" nil t))
    (skip-syntax-forward " ")))

(defun typerex-skip-back-blank-and-comments ()
  (skip-syntax-backward " ")
  (while (save-excursion (typerex-backward-char)
                         (and (> (point) (point-min)) (typerex-in-comment-p)))
    (typerex-backward-char)
    (typerex-beginning-of-literal-or-comment) (skip-syntax-backward " ")))

(defun typerex-find-phrase-beginning (&optional stop-at-and)
  "Find `real' phrase beginning and return point."
  (beginning-of-line)
  (typerex-skip-blank-and-comments)
  (end-of-line)
  (typerex-skip-to-end-of-phrase)
  (let ((old-point (point)) (pt (point)))
    (if stop-at-and
        (typerex-find-kwop typerex-find-phrase-beginning-and-regexp "and")
      (typerex-find-kwop typerex-find-phrase-beginning-regexp))
    (while (and (> (point) (point-min)) (< (point) old-point)
                (or (not (looking-at typerex-find-phrase-beginning-and-regexp))
                    (and (looking-at "\\<let\\>")
                         (typerex-looking-at-internal-let))
                    (and (looking-at "\\<and\\>")
                         (save-excursion
                           (typerex-find-and-match)
                           (typerex-looking-at-internal-let)))
                    (and (looking-at "\\<module\\>")
                         (typerex-looking-at-false-module))
                    (and (looking-at typerex-sig-struct-regexp)
                         (typerex-looking-at-false-sig-struct))
                    (and (looking-at "\\<type\\>")
                         (typerex-looking-at-false-type))))
      (when (= pt (point))
        (error "typerex-find-phrase-beginning: inf loop at %d" pt))
      (setq pt (point))
      (if (looking-at "\\<end\\>")
          (typerex-find-match)
        (unless (bolp) (typerex-backward-char))
        (setq old-point (point))
        (if stop-at-and
            (typerex-find-kwop typerex-find-phrase-beginning-and-regexp "and")
          (typerex-find-kwop typerex-find-phrase-beginning-regexp))))
    (when (typerex-at-phrase-break-p)
      (end-of-line) (typerex-skip-blank-and-comments))
    (back-to-indentation)
    (point)))

(defun typerex-imenu-prev-index-position ()
  "The default value for `imenu-prev-index-position-function'."
  (let ((pos (point)) ret)
    (while (and (<= 0 pos)
                (<= pos (setq ret (typerex-find-phrase-beginning t))))
      (setq pos (goto-char (1- pos))))
    (and (<= 0 pos) ret)))

(defun typerex-imenu-extract-index-name ()
  "The default value for `imenu-extract-index-name-function'."
  (forward-sexp 1)
  (skip-syntax-forward " ")
  (buffer-substring-no-properties (point) (scan-sexps (point) 1)))

(defun typerex-search-forward-end ()
  (let ((begin (point)) (current -1) (found) (move t))
    (while (and move (> (point) current))
      (if (re-search-forward "\\<end\\>" (point-max) t)
          (let ((stop (point)) (kwop))
            (unless (typerex-in-literal-or-comment-p)
              (save-excursion
                (typerex-backward-char 3)
                (setq kwop (typerex-find-match))
                (cond
                 ((string= kwop "object")
                  (typerex-find-phrase-beginning))
                 ((and (looking-at typerex-sig-struct-regexp)
                       (typerex-looking-at-false-sig-struct))
                  (typerex-find-phrase-beginning)))
                (cond
                 ((or
                   (> (point) begin)
                   (and
                    (string= kwop "sig")
                    (looking-at "[ \t\n]*\\(\\<with\\>[ \t\n]*\\<type\\>\\|=\\)")))
                  (if (> (point) current)
                      (progn
                        (setq current (point))
                        (goto-char stop))
                    (setq found nil move nil)))
                 (t (setq found t move nil))))))
        (setq found nil move nil)))
    found))

(defun typerex-inside-module-or-class-find-kwop ()
  (let ((kwop (typerex-find-kwop typerex-inside-module-or-class-regexp
                                "\\<\\(and\\|end\\)\\>")))
    (typerex-reset-and-kwop kwop)
    (when (string= kwop "with") (setq kwop nil))
    (if (string= kwop "end")
        (progn
          (typerex-find-match)
          (typerex-find-kwop typerex-inside-module-or-class-regexp)
          (typerex-inside-module-or-class-find-kwop))
      kwop)))

(defun typerex-inside-module-or-class-p ()
  (let ((begin) (end) (and-end) (and-iter t) (kwop t))
    (save-excursion
      (when (looking-at "\\<and\\>")
        (typerex-find-and-match))
      (setq begin (point))
      (unless (or (and (looking-at "\\<class\\>")
                       (save-excursion
                         (re-search-forward "\\<object\\>"
                                            (point-max) t)
                         (typerex-find-phrase-beginning)
                         (> (point) begin)))
                  (and (looking-at "\\<module\\>")
                       (save-excursion
                         (re-search-forward typerex-sig-struct-regexp
                                            (point-max) t)
                         (typerex-find-phrase-beginning)
                         (> (point) begin))))
        (unless (looking-at typerex-inside-module-or-class-opening-full)
          (setq kwop (typerex-inside-module-or-class-find-kwop)))
        (when kwop
          (setq begin (point))
          (when (typerex-search-forward-end)
            (typerex-backward-char 3)
            (when (looking-at "\\<end\\>")
              (typerex-forward-char 3)
              (setq end (point))
              (setq and-end (point))
              (typerex-skip-blank-and-comments)
              (while (and and-iter (looking-at "\\<and\\>"))
                (setq and-end (point))
                (when (typerex-search-forward-end)
                  (typerex-backward-char 3)
                  (when (looking-at "\\<end\\>")
                    (typerex-forward-char 3)
                    (setq and-end (point))
                    (typerex-skip-blank-and-comments)))
                (when (<= (point) and-end)
                  (setq and-iter nil)))
              (list begin end and-end))))))))

(defun typerex-move-inside-module-or-class-opening ()
  "Go to the beginning of the enclosing module or class.

Notice that white-lines (or comments) located immediately before a
module/class are considered enclosed in this module/class."
  (interactive)
  (let* ((old-point (point))
         (kwop (typerex-inside-module-or-class-find-kwop)))
    (unless kwop
      (goto-char old-point))
    (typerex-find-phrase-beginning)))

(defun typerex-discover-phrase (&optional quiet stop-at-and)
  (end-of-line)
  (let ((end (point)) (case-fold-search nil))
   (typerex-with-internal-syntax
    (typerex-find-phrase-beginning stop-at-and)
    (when (> (point) end) (setq end (point)))
    (save-excursion
      (let ((begin (point)) (cpt 0) (lines-left 0) (stop)
            (inside-module-or-class (typerex-inside-module-or-class-p))
            (looking-block
             (looking-at typerex-inside-module-or-class-opening-full)))
        (if (and looking-block inside-module-or-class)
            (progn
              (setq begin (nth 0 inside-module-or-class))
              (setq end (nth 2 inside-module-or-class))
              (goto-char end))
          (if inside-module-or-class
              (progn
                (setq stop (save-excursion
                             (goto-char (nth 1 inside-module-or-class))
                             (line-beginning-position)))
                (if (< stop end) (setq stop (point-max))))
            (setq stop (point-max)))
          (save-restriction
            (goto-char end)
            (while (and (= lines-left 0)
                        (or (not inside-module-or-class) (< (point) stop))
                        (<= (save-excursion
                              (typerex-find-phrase-beginning stop-at-and)) end))
              (unless quiet
                (setq cpt (1+ cpt))
                (when (= 8 cpt)
                  (message "Looking for enclosing phrase...")))
              (setq end (point))
              (typerex-skip-to-end-of-phrase)
              (narrow-to-region (line-beginning-position) (point-max))
              (goto-char end)
              (setq lines-left (forward-line 1)))))
        (when (>= cpt 8) (message "Looking for enclosing phrase... done."))
        (save-excursion (typerex-skip-blank-and-comments) (setq end (point)))
        (typerex-skip-back-blank-and-comments)
        (list begin (point) end))))))

(defun typerex-mark-phrase ()
  "Put mark at end of this Caml phrase, point at beginning.
The Caml phrase is the phrase just before the point."
  (interactive)
  (let ((pair (typerex-discover-phrase)))
    (goto-char (nth 1 pair)) (push-mark (nth 0 pair) t t)))

(defun typerex-next-phrase (&optional quiet stop-at-and)
  "Skip to the beginning of the next phrase."
  (interactive "i")
  (goto-char (save-excursion
               (nth 2 (typerex-discover-phrase quiet stop-at-and))))
  (cond
   ((looking-at "\\<end\\>")
    (typerex-next-phrase quiet stop-at-and))
   ((looking-at ")")
    (forward-char 1)
    (typerex-skip-blank-and-comments))
   ((looking-at ";;")
    (forward-char 2)
    (typerex-skip-blank-and-comments))))

(defun typerex-previous-phrase ()
  "Skip to the beginning of the previous phrase."
  (interactive)
  (beginning-of-line)
  (typerex-skip-to-end-of-phrase)
  (typerex-discover-phrase))

(defun typerex-indent-phrase ()
  "Depending of the context: justify and indent a comment,
or indent all lines in the current phrase."
  (interactive)
  (save-excursion
    (back-to-indentation)
    (if (typerex-in-comment-p)
        (let* ((cobpoint (save-excursion
                           (typerex-beginning-of-literal-or-comment)
                           (point)))
               (begpoint (save-excursion
                           (while (and (> (point) cobpoint)
                                       (typerex-in-comment-p)
                                       (not (looking-at "^[ \t]*$")))
                             (forward-line -1))
                           (max cobpoint (point))))
               (coepoint (save-excursion
                           (while (typerex-in-comment-p)
                             (re-search-forward "\\*)" nil 'end))
                           (point)))
               (endpoint (save-excursion
                           (re-search-forward "^[ \t]*$" coepoint 'end)
                           (line-beginning-position 2)))
               (leading-star (typerex-leading-star-p)))
          (goto-char begpoint)
          (while (and leading-star
                      (< (point) endpoint)
                      (not (looking-at "^[ \t]*$")))
            (forward-line 1)
            (back-to-indentation)
            (when (looking-at "\\*\\**\\([^)]\\|$\\)")
              (delete-char 1)
              (setq endpoint (1- endpoint))))
          (goto-char (min (point) endpoint))
          (fill-region begpoint endpoint)
          (re-search-forward "\\*)" nil 'end)
          (setq endpoint (point))
          (when leading-star
            (goto-char begpoint)
            (forward-line 1)
            (if (< (point) endpoint)
                (typerex-auto-fill-insert-leading-star t)))
          (indent-region begpoint endpoint nil))
      (let ((pair (typerex-discover-phrase)))
        (indent-region (nth 0 pair) (nth 1 pair) nil)))))

(defun typerex-complete (arg)
  "Completes qualified ocaml identifiers."
  (interactive "p")
  (modify-syntax-entry ?_ "w" typerex-mode-syntax-table)
  (caml-complete arg)
  (modify-syntax-entry ?_ "_" typerex-mode-syntax-table))

(defun typerex--try-find-alternate-file (partial-name extension)
  (let* ((filename (concat partial-name extension))
         (buffer (get-file-buffer filename))
         (what (cond 
                ((string= extension ".ml") "implementation")
                ((string= extension ".mli") "interface"))))
    (if buffer
        (progn (switch-to-buffer buffer) t)
      (if (file-exists-p filename)
          (progn (find-file filename) t)
        (when (and (not (string= extension ".mll"))
                   (y-or-n-p 
                    (format "Create %s file (%s)" what 
                            (file-name-nondirectory filename))))
          (find-file filename)))
      nil)))

(defun typerex-find-alternate-file ()
  "Switch Implementation/Interface."
  (interactive)
  (let ((name (buffer-file-name)))
    (when (string-match "\\`\\(.*\\)\\.ml\\([il]\\)?\\'" name)
      (let ((partial-name (typerex-match-string 1 name)))
        (let ((c (match-string 2 name)))
          (cond 
           ((string= "i" c) (unless (typerex--try-find-alternate-file 
                                     partial-name ".mll")
                              (typerex--try-find-alternate-file
                               partial-name ".ml")))
           ((string= "l" c) (typerex--try-find-alternate-file 
                             partial-name ".mli"))
           ((eq nil c) (typerex--try-find-alternate-file 
                             partial-name ".mli"))))))))

(defun typerex-ensure-space ()
  (let ((prec (preceding-char)))
    (when (and prec (not (char-equal ?\  (char-syntax prec))))
      (insert " "))))

(defun typerex-insert-class-form ()
  "Insert a nicely formatted class-end form, leaving a mark after end."
  (interactive "*")
  (typerex-ensure-space)
  (let ((old (point)))
    (insert "class  = object (self)\ninherit  as super\nend;;\n")
    (end-of-line)
    (indent-region old (point) nil)
    (indent-according-to-mode)
    (push-mark)
    (forward-line -2)
    (indent-according-to-mode)))

(defun typerex-insert-begin-form ()
  "Insert a nicely formatted begin-end form, leaving a mark after end."
  (interactive "*")
  (typerex-ensure-space)
  (let ((old (point)))
    (insert "begin\n\nend\n")
    (end-of-line)
    (indent-region old (point) nil)
    (push-mark)
    (forward-line -2)
    (indent-according-to-mode)))

(defun typerex-insert-for-form ()
  "Insert a nicely formatted for-to-done form, leaving a mark after done."
  (interactive "*")
  (typerex-ensure-space)
  (let ((old (point)))
    (insert "for  do\n\ndone\n")
    (end-of-line)
    (indent-region old (point) nil)
    (push-mark)
    (forward-line -2)
    (indent-according-to-mode)
    (beginning-of-line 1)
    (backward-char 4)))

(defun typerex-insert-while-form ()
  "Insert a nicely formatted for-to-done form, leaving a mark after done."
  (interactive "*")
  (typerex-ensure-space)
  (let ((old (point)))
    (insert "while  do\n\ndone\n")
    (end-of-line)
    (indent-region old (point) nil)
    (push-mark)
    (forward-line -2)
    (indent-according-to-mode)
    (beginning-of-line 1)
    (backward-char 4)))

(defun typerex-insert-if-form ()
  "Insert a nicely formatted if-then-else form, leaving a mark after else."
  (interactive "*")
  (typerex-ensure-space)
  (let ((old (point)))
    (insert "if\n\nthen\n\nelse\n")
    (end-of-line)
    (indent-region old (point) nil)
    (indent-according-to-mode)
    (push-mark)
    (forward-line -2)
    (indent-according-to-mode)
    (forward-line -2)
    (indent-according-to-mode)))

(defun typerex-insert-match-form ()
  "Insert a nicely formatted math-with form, leaving a mark after with."
  (interactive "*")
  (typerex-ensure-space)
  (let ((old (point)))
    (insert "match\n\nwith\n")
    (end-of-line)
    (indent-region old (point) nil)
    (indent-according-to-mode)
    (push-mark)
    (forward-line -2)
    (indent-according-to-mode)))

(defun typerex-insert-let-form ()
  "Insert a nicely formatted let-in form, leaving a mark after in."
  (interactive "*")
  (typerex-ensure-space)
  (let ((old (point)))
    (insert "let  in\n")
    (end-of-line)
    (indent-region old (point) nil)
    (indent-according-to-mode)
    (push-mark)
    (beginning-of-line)
    (backward-char 4)
    (indent-according-to-mode)))

(defun typerex-insert-try-form ()
  "Insert a nicely formatted try-with form, leaving a mark after with."
  (interactive "*")
  (typerex-ensure-space)
  (let ((old (point)))
    (insert "try\n\nwith\n")
    (end-of-line)
    (indent-region old (point) nil)
    (indent-according-to-mode)
    (push-mark)
    (forward-line -2)
    (indent-according-to-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            TypeRex interactive mode

;; Augment TypeRex mode with a Caml toplevel.

(require 'comint)

(defvar typerex-interactive-mode-map
  (let ((map (copy-keymap comint-mode-map)))
    (define-key map "|" 'typerex-electric-pipe)
    (define-key map ")" 'typerex-electric-rp)
    (define-key map "}" 'typerex-electric-rc)
    (define-key map "]" 'typerex-electric-rb)
    (define-key map "\C-c\C-i" 'typerex-interrupt-caml)
    (define-key map "\C-c\C-k" 'typerex-kill-caml)
    (define-key map "\C-c`" 'typerex-interactive-next-error-toplevel)
    (define-key map "\C-c?" 'typerex-interactive-next-error-toplevel)
    (define-key map "\C-m" 'typerex-interactive-send-input)
    (define-key map "\C-j" 'typerex-interactive-send-input-or-indent)
    (define-key map "\M-\C-m" 'typerex-interactive-send-input-end-of-phrase)
    (define-key map [kp-enter] 'typerex-interactive-send-input-end-of-phrase)
    map))

(defconst typerex-interactive-buffer-name "*caml-toplevel*")

(defconst typerex-interactive-toplevel-error-regexp
  "[ \t]*Characters \\([0-9]+\\)-\\([0-9]+\\):"
  "Regexp matching the char numbers in ocaml toplevel's error messages.")
(defvar typerex-interactive-last-phrase-pos-in-source 0)
(defvar typerex-interactive-last-phrase-pos-in-toplevel 0)

(defun typerex-interactive-filter (text)
  (when (eq major-mode 'typerex-interactive-mode)
    (save-excursion
      (when (>= comint-last-input-end comint-last-input-start)
        (when typerex-interactive-read-only-input
          (add-text-properties
           comint-last-input-start comint-last-input-end
           (list 'read-only t)))
        (when (and font-lock-mode typerex-interactive-input-font-lock)
          (font-lock-fontify-region comint-last-input-start
                                    comint-last-input-end))
        (when typerex-interactive-output-font-lock
          (save-excursion
            (goto-char (point-max))
            (re-search-backward comint-prompt-regexp
                                comint-last-input-end t)
            (add-text-properties
             comint-last-input-end (point)
             '(face typerex-font-lock-interactive-output-face))))
        (when typerex-interactive-error-font-lock
          (save-excursion
            (goto-char comint-last-input-end)
            (while (re-search-forward typerex-interactive-error-regexp () t)
              (let ((matchbeg (match-beginning 1))
                    (matchend (match-end 1)))
                (save-excursion
                  (goto-char matchbeg)
                  (put-text-property
                   matchbeg matchend
                   'face 'typerex-font-lock-interactive-error-face)
                  (when (looking-at typerex-interactive-toplevel-error-regexp)
                    (let ((beg (string-to-number (typerex-match-string 1)))
                          (end (string-to-number (typerex-match-string 2))))
                      (put-text-property
                       (+ comint-last-input-start beg)
                       (+ comint-last-input-start end)
                       'face 'typerex-font-lock-error-face))))))))))))

(easy-menu-define
  typerex-interactive-mode-menu typerex-interactive-mode-map
  "TypeRex Interactive Mode Menu."
  '("TypeRex"
    ("Interactive Mode"
     ["Run Caml Toplevel" typerex-run-caml t]
     ["Interrupt Caml Toplevel" typerex-interrupt-caml
      :active (comint-check-proc typerex-interactive-buffer-name)]
     ["Kill Caml Toplevel" typerex-kill-caml
      :active (comint-check-proc typerex-interactive-buffer-name)]
     ["Evaluate Region" typerex-eval-region :active (region-active-p)]
     ["Evaluate Phrase" typerex-eval-phrase t]
     ["Evaluate Buffer" typerex-eval-buffer t])
    "---"
    ["Customize TypeRex Mode..." (customize-group 'ocp) t]
    ("TypeRex Options" ["Dummy" nil t])
    ("TypeRex Interactive Options" ["Dummy" nil t])
    "---"
    ["About" typerex-about t]
    ["Help" typerex-interactive-help t]))

(define-derived-mode typerex-interactive-mode comint-mode "TypeRex-Interactive"
  "Major mode for interacting with a Caml process.
Runs a Caml toplevel as a subprocess of Emacs, with I/O through an
Emacs buffer. A history of input phrases is maintained. Phrases can
be sent from another buffer in Caml mode.

Short cuts for interactions with the toplevel:
\\{typerex-interactive-mode-map}"
  (typerex-install-font-lock)
  (when (or typerex-interactive-input-font-lock
            typerex-interactive-output-font-lock
            typerex-interactive-error-font-lock)
    (font-lock-mode 1))
  (add-hook 'comint-output-filter-functions 'typerex-interactive-filter)
  (when (boundp 'after-change-functions)
    (remove-hook 'after-change-functions 'font-lock-after-change-function t))
  (when (boundp 'pre-idle-hook)
    (remove-hook 'pre-idle-hook 'font-lock-pre-idle-hook t))
  (setq comint-prompt-regexp "^#  *")
  (setq comint-process-echoes nil)
  (setq comint-get-old-input 'typerex-interactive-get-old-input)
  (setq comint-scroll-to-bottom-on-output
        typerex-interactive-scroll-to-bottom-on-output)
  (set-syntax-table typerex-mode-syntax-table)
  (setq local-abbrev-table typerex-mode-abbrev-table)

  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'typerex-indent-command)

  (easy-menu-add typerex-interactive-mode-menu)
  (typerex-update-options-menu))

(defun typerex-run-caml ()
  "Run a Caml toplevel process. I/O via buffer `*caml-toplevel*'."
  (interactive)
  (typerex-run-process-if-needed)
  (display-buffer typerex-interactive-buffer-name))

(defun typerex-run-process-if-needed (&optional cmd)
  "Run a Caml toplevel process if needed, with an optional command name.
I/O via buffer `*caml-toplevel*'."
  (if cmd
      (setq typerex-interactive-program cmd)
    (unless (comint-check-proc typerex-interactive-buffer-name)
      (setq typerex-interactive-program
            (read-shell-command "Caml toplevel to run: "
                                typerex-interactive-program))))
  (unless (comint-check-proc typerex-interactive-buffer-name)
    (let ((cmdlist (typerex-args-to-list typerex-interactive-program))
          (process-connection-type nil))
      (set-buffer (apply (function make-comint) "caml-toplevel"
                         (car cmdlist) nil (cdr cmdlist)))
      (typerex-interactive-mode)
      (sleep-for 1))))

(defun typerex-args-to-list (string)
  (let ((where (string-match "[ \t]" string)))
    (cond ((null where) (list string))
          ((/= where 0)
           (cons (substring string 0 where)
                 (typerex-args-to-list (substring string (+ 1 where)
                                                 (length string)))))
          (t (let ((pos (string-match "[^ \t]" string)))
               (when pos
                 (typerex-args-to-list (substring string pos
                                                 (length string)))))))))

(defun typerex-interactive-get-old-input ()
  (save-excursion
    (let ((end (point)))
      (re-search-backward comint-prompt-regexp (point-min) t)
      (when (looking-at comint-prompt-regexp)
        (re-search-forward comint-prompt-regexp))
      (buffer-substring-no-properties (point) end))))

(defun typerex-interactive-end-of-phrase ()
  (save-excursion
    (end-of-line)
    (typerex-find-meaningful-word)
    (typerex-find-meaningful-word)
    (looking-at ";;")))

(defun typerex-interactive-send-input-end-of-phrase ()
  (interactive)
  (goto-char (point-max))
  (unless (typerex-interactive-end-of-phrase)
    (insert ";;"))
  (comint-send-input))

(defconst typerex-interactive-send-warning
  "Note: toplevel processing requires a terminating `;;'")

(defun typerex-interactive-send-input ()
  "Process if the current line ends with `;;' then send the
current phrase else insert a newline."
  (interactive)
  (if (typerex-interactive-end-of-phrase)
      (progn
        (comint-send-input)
        (goto-char (point-max)))
    (insert "\n")
    (message typerex-interactive-send-warning)))

(defun typerex-interactive-send-input-or-indent ()
  "Process if the current line ends with `;;' then send the
current phrase else insert a newline and indent."
  (interactive)
  (if (typerex-interactive-end-of-phrase)
      (progn
        (goto-char (point-max))
        (comint-send-input))
    (insert "\n")
    (indent-according-to-mode)
    (message typerex-interactive-send-warning)))

(defun typerex-eval-region (start end)
  "Eval the current region in the Caml toplevel."
  (interactive "r")
  (save-excursion (typerex-run-process-if-needed))
  (comint-preinput-scroll-to-bottom)
  (setq typerex-interactive-last-phrase-pos-in-source start)
  (save-excursion
    (goto-char start)
    (typerex-skip-blank-and-comments)
    (setq start (point))
    (goto-char end)
    (typerex-skip-to-end-of-phrase)
    (setq end (point))
    (let ((text (buffer-substring-no-properties start end)))
      (goto-char end)
      (if (string= text "")
          (message "Cannot send empty commands to Caml toplevel!")
        (set-buffer typerex-interactive-buffer-name)
        (goto-char (point-max))
        (setq typerex-interactive-last-phrase-pos-in-toplevel (point))
        (comint-send-string typerex-interactive-buffer-name
                            (concat text ";;"))
        (let ((pos (point)))
          (comint-send-input)
          (when typerex-interactive-echo-phrase
            (save-excursion
              (goto-char pos)
              (insert (concat text ";;")))))))
    (when typerex-display-buffer-on-eval
      (display-buffer typerex-interactive-buffer-name))))

(defun typerex-narrow-to-phrase ()
  "Narrow the editting window to the surrounding Caml phrase (or block)."
  (interactive)
  (save-excursion
    (let ((pair (typerex-discover-phrase)))
      (narrow-to-region (nth 0 pair) (nth 1 pair)))))

(defun typerex-eval-phrase ()
  "Eval the surrounding Caml phrase (or block) in the Caml toplevel."
  (interactive)
  (let ((end))
    (save-excursion
      (let ((pair (typerex-discover-phrase)))
        (setq end (nth 2 pair))
        (typerex-eval-region (nth 0 pair) (nth 1 pair))))
    (when typerex-skip-after-eval-phrase
      (goto-char end))))

(defun typerex-eval-buffer ()
  "Send the buffer to the TypeRex Interactive process."
  (interactive)
  (typerex-eval-region (point-min) (point-max)))

(defun typerex-interactive-next-error-source ()
  (interactive)
  (let ((error-pos) (beg 0) (end 0))
    (with-current-buffer typerex-interactive-buffer-name
      (goto-char typerex-interactive-last-phrase-pos-in-toplevel)
      (setq error-pos
            (re-search-forward typerex-interactive-toplevel-error-regexp
                               (point-max) t))
      (when error-pos
        (setq beg (string-to-number (typerex-match-string 1))
              end (string-to-number (typerex-match-string 2)))))
    (if (not error-pos)
        (message "No syntax or typing error in last phrase.")
      (setq beg (+ typerex-interactive-last-phrase-pos-in-source beg)
            end (+ typerex-interactive-last-phrase-pos-in-source end))
      (goto-char beg)
      (put-text-property beg end 'face 'typerex-font-lock-error-face))))

(defun typerex-interactive-next-error-toplevel ()
  (interactive)
  (let ((error-pos) (beg 0) (end 0))
    (save-excursion
      (goto-char typerex-interactive-last-phrase-pos-in-toplevel)
      (setq error-pos
            (re-search-forward typerex-interactive-toplevel-error-regexp
                               (point-max) t))
      (when error-pos
        (setq beg (string-to-number (typerex-match-string 1))
              end (string-to-number (typerex-match-string 2)))))
    (if (not error-pos)
        (message "No syntax or typing error in last phrase.")
      (setq beg (+ typerex-interactive-last-phrase-pos-in-toplevel beg)
            end (+ typerex-interactive-last-phrase-pos-in-toplevel end))
      (put-text-property beg end 'face 'typerex-font-lock-error-face)
      (goto-char beg))))

(defun typerex-interrupt-caml ()
  (interactive)
  (when (comint-check-proc typerex-interactive-buffer-name)
    (with-current-buffer typerex-interactive-buffer-name
      (comint-interrupt-subjob))))

(defun typerex-kill-caml ()
  (interactive)
  (when (comint-check-proc typerex-interactive-buffer-name)
    (with-current-buffer typerex-interactive-buffer-name
      (comint-kill-subjob))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Menu support

(defun typerex-about ()
  (interactive)
  (describe-variable 'typerex-mode-version))

(defun typerex-short-cuts ()
  "Short cuts for the TypeRex mode:
\\{typerex-mode-map}

Short cuts for interaction within the toplevel:
\\{typerex-interactive-mode-map}"
  (interactive)
  (describe-function 'typerex-short-cuts))

(defun typerex-help ()
  (interactive)
  (describe-function 'typerex-mode))

(defun typerex-interactive-help ()
  (interactive)
  (describe-function 'typerex-interactive-mode))

(defvar typerex-definitions-menu (list ["Scan..." typerex-list-definitions t])
  "Initial content of the definitions menu.")
(make-variable-buffer-local 'typerex-definitions-menu)

(defvar typerex-definitions-menu-last-buffer nil)
(defvar typerex-definitions-keymaps nil)

(defun typerex-update-definitions-menu ()
  (when (eq major-mode 'typerex-mode)
    (easy-menu-change
     '("TypeRex") "Definitions"
     typerex-definitions-menu)))

(defun typerex-with-emacs-update-definitions-menu ()
  (when (current-local-map)
    (let ((keymap
           (lookup-key (current-local-map) [menu-bar TypeRex Definitions])))
      (if (and
           (keymapp keymap)
           (not (eq typerex-definitions-menu-last-buffer (current-buffer))))
          (setcdr keymap typerex-definitions-keymaps)
        (setq typerex-definitions-menu-last-buffer (current-buffer))))))

(defun typerex-toggle-option (symbol)
  (interactive)
  (set symbol (not (symbol-value symbol)))
  (when (eq 'typerex-use-abbrev-mode symbol)
    (abbrev-mode typerex-use-abbrev-mode)) ; toggle abbrev minor mode
  (unless typerex-with-xemacs
    (typerex-update-options-menu)))

(defun typerex-update-options-menu ()
  (easy-menu-change
   '("TypeRex" "More") "TypeRex Options"
   (mapcar (lambda (pair)
             (if (consp pair)
                 (vector (car pair)
                         (list 'typerex-toggle-option (cdr pair))
                         ':style 'toggle
                         ':selected (nth 1 (cdr pair))
                         ':active t)
               pair)) typerex-options-list))
  (easy-menu-change
   '("TypeRex" "More") "TypeRex Interactive Options"
   (mapcar (lambda (pair)
             (if (consp pair)
                 (vector (car pair)
                         (list 'typerex-toggle-option (cdr pair))
                         ':style 'toggle
                         ':selected (nth 1 (cdr pair))
                         ':active t)
               pair)) typerex-interactive-options-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Browse Manual

;; From M. Quercia

(defun typerex-browse-manual ()
  "*Browse Caml reference manual."
  (interactive)
  (setq typerex-manual-url (read-from-minibuffer "URL: " typerex-manual-url))
  (funcall typerex-browser typerex-manual-url))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Browse Library

;; From M. Quercia

(defvar typerex-library-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map [return] 'typerex-library-find-file)
    (define-key map [mouse-2] 'typerex-library-mouse-find-file)
    map))

(defun typerex-browse-library()
  "Browse the Caml library."
  (interactive)
  (let ((buf-name "*caml-library*") (opoint)
        (dir (read-from-minibuffer "Library path: " typerex-library-path)))
    (when (and (file-directory-p dir) (file-readable-p dir))
      (setq typerex-library-path dir)
      ;; List *.ml and *.mli files
      (with-output-to-temp-buffer buf-name
        (buffer-disable-undo standard-output)
        (with-current-buffer buf-name
          (kill-all-local-variables)
          (make-local-variable 'typerex-library-path)
          (setq typerex-library-path dir)
          ;; Help
          (insert "Directory \"" dir "\".\n")
          (insert "Select a file with middle mouse button or RETURN.\n\n")
          (insert "Interface files (.mli):\n\n")
          (insert-directory (concat dir "/*.mli") "-C" t nil)
          (insert "\n\nImplementation files (.ml):\n\n")
          (insert-directory (concat dir "/*.ml") "-C" t nil)
          ;; '.', '-' and '_' are now letters
          (modify-syntax-entry ?. "w")
          (modify-syntax-entry ?_ "w")
          (modify-syntax-entry ?- "w")
          ;; Every file name is now mouse-sensitive
          (goto-char (point-min))
          (while (< (point) (point-max))
            (re-search-forward "\\.ml.?\\>")
            (setq opoint (point))
            (re-search-backward "\\<" (point-min) 1)
            (put-text-property (point) opoint 'mouse-face 'highlight)
            (goto-char (+ 1 opoint)))
          ;; Activate typerex-library mode
          (setq major-mode 'typerex-library-mode)
          (setq mode-name "typerex-library")
          (use-local-map typerex-library-mode-map)
          (setq buffer-read-only t))))))

(defun typerex-library-find-file ()
  "Load the file whose name is near point."
  (interactive)
  (when (text-properties-at (point))
    (save-excursion
      (let (beg)
        (re-search-backward "\\<") (setq beg (point))
        (re-search-forward "\\>")
        (find-file-read-only (concat typerex-library-path "/"
                                     (buffer-substring-no-properties
                                      beg (point))))))))

(defun typerex-library-mouse-find-file (event)
  "Visit the file name you click on."
  (interactive "e")
  (let ((owindow (selected-window)))
    (mouse-set-point event)
    (typerex-library-find-file)
    (select-window owindow)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Definitions List

;; Designed from original code by M. Quercia

(defconst typerex--id-regexp "[[:alpha:]][_'[:alnum:]]*")

(defconst typerex-definitions-bind-skip-regexp
  (concat (typerex-ro "rec" "type" "virtual") "\\|'"
          typerex--id-regexp "\\|('.*)")
  "Regexp matching stuff to ignore after a binding keyword.")

(defconst typerex-identifier-regexp (concat "\\<" typerex--id-regexp "\\>"))

(defun typerex-list-definitions ()
  "Parse the buffer and gather toplevel definitions
for a quick jump via the definitions menu."
  (interactive)
  (message "Searching for definitions...")
  (save-excursion
    (let ((cpt 0) (kw) (menu)
          (value-list) (type-list) (module-list) (class-list) (misc-list))
      (goto-char (point-min))
      (typerex-skip-blank-and-comments)
      (while (and (< (point) (point-max)))
        (when (looking-at typerex-definitions-regexp)
          (setq kw (typerex-match-string 0))
          (save-match-data (typerex-reset-and-kwop kw))
          (when (or (string= kw "exception") (string= kw "val"))
            (setq kw "let"))
          ;; Skip optional elements
          (goto-char (match-end 0))
          (typerex-skip-blank-and-comments)
          (when (looking-at typerex-definitions-bind-skip-regexp)
            (goto-char (match-end 0)))
          (typerex-skip-blank-and-comments)
          (when (looking-at typerex-identifier-regexp)
            ;; Menu item : [name (goto-char ...) t]
            (let* ((p (make-marker))
                   (ref (vector (typerex-match-string 0)
                                (list 'typerex-goto p) t)))
              (setq cpt (1+ cpt))
              (message (concat "Searching definitions... ("
                               (number-to-string cpt) ")"))
              (set-marker p (point))
              (cond ((string= kw "let")
                     (setq value-list (cons ref value-list)))
                    ((string= kw "type")
                     (setq type-list (cons ref type-list)))
                    ((string= kw "module")
                     (setq module-list (cons ref module-list)))
                    ((string= kw "class")
                     (setq class-list (cons ref class-list)))
                    (t (setq misc-list (cons ref misc-list)))))))
        ;; Skip to next phrase or next top-level `and'
        (typerex-forward-char)
        (let ((old-point (point))
              (last-and (progn (typerex-next-phrase t t) (point))))
          (when (< last-and old-point) (error "scan error"))
          (save-excursion
            (while (and (re-search-backward "\\<and\\>" old-point t)
                        (not (typerex-in-literal-or-comment-p))
                        (save-excursion (typerex-find-and-match)
                                        (>= old-point (point))))
              (setq last-and (point))))
          (goto-char last-and)))
      ;; Sort and build lists
      (dolist (pair (list (cons "Miscellaneous" misc-list)
                          (cons "Values" value-list)
                          (cons "Classes" class-list)
                          (cons "Types" type-list)
                          (cons "Modules" module-list)))
        (when (cdr pair)
          (setq menu
                (append (typerex-split-long-list
                         (car pair) (typerex-sort-definitions (cdr pair)))
                        menu))))
      ;; Update definitions menu
      (setq typerex-definitions-menu
            (append menu (list "---"
                               ["Rescan..." typerex-list-definitions t])))
      (unless (or typerex-with-xemacs
                  (not (functionp 'easy-menu-create-menu)))
        ;; Patch for Emacs
        (setq typerex-definitions-keymaps
              (cdr (easy-menu-create-menu
                    "Definitions" typerex-definitions-menu)))
        (setq typerex-definitions-menu-last-buffer nil))
      (message "Searching definitions... done")))
  (typerex-update-definitions-menu))

(defun typerex-goto (pos)
  (goto-char pos)
  (recenter))

(defun typerex-sort-definitions (list)
  (let* ((last "") (cpt 1)
         (list (sort (nreverse list)
                     (lambda (p q) (string< (elt p 0) (elt q 0)))))
         (tail list))
    (while tail
      (if (string= (elt (car tail) 0) last)
          (progn
            (setq cpt (1+ cpt))
            (aset (car tail) 0 (format "%s (%d)" last cpt)))
        (setq cpt 1)
        (setq last (elt (car tail) 0)))
      (setq tail (cdr tail)))
    list))

;; Look for the (n-1)th or last element of a list
(defun typerex-nth (n list)
  (if (or (<= n 1) (null list) (null (cdr list))) list
    (typerex-nth (1- n) (cdr list))))

;; Split a definition list if it is too long
(defun typerex-split-long-list (title list)
  (let ((tail (typerex-nth typerex-definitions-max-items list)))
    (if (or (null tail) (null (cdr tail)))
        ;; List not too long, cons the title
        (list (cons title list))
      ;; List too long, split and add initials to the title
      (let (lists)
        (while list
          (let ((beg (substring (elt (car list) 0) 0 1))
                (end (substring (elt (car tail) 0) 0 1)))
            (setq lists (cons
                         (cons (format "%s %s-%s" title beg end) list)
                         lists))
            (setq list (cdr tail))
            (setcdr tail nil)
            (setq tail (typerex-nth typerex-definitions-max-items list))))
        (nreverse lists)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Hooks and Exit

(eval-when-compile
  (autoload 'speedbar-add-supported-extension "speedbar"))
(when (require 'speedbar nil t)
  (speedbar-add-supported-extension
   '(".ml" ".mli" ".mll" ".mly" ".ls")))

(defvar typerex-load-hook nil
  "This hook is run when TypeRex is loaded in. It is a good place to put
key-bindings or hack Font-Lock keywords...")

(run-hooks 'typerex-load-hook)

(provide 'typerex)
;; For compatibility with caml support modes
;; you may also link caml.el to ocp.el
(provide 'caml)
