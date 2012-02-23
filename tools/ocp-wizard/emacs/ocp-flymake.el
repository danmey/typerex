;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                        ;
;                        TypeRex OCaml Studio                            ;
;                                                                        ;
;                           Wojciech Meyer                               ;
;                                                                        ;
;  Copyright 2011-2012 INRIA Saclay - Ile-de-France / OCamlPro           ;
;  All rights reserved.  This file is distributed under the terms of     ;
;  the GNU Public License version 3.0.                                   ;
;                                                                        ;
;  TypeRex is distributed in the hope that it will be useful,            ;
;  but WITHOUT ANY WARRANTY; without even the implied warranty of        ;
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         ;
;  GNU General Public License for more details.                          ;
;                                                                        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The simplest incremental compilation using flymake.
;; Uses ocamlbuild even when the project doesn't use it.
;; The root of the project is where the .typerex file is found.
;; This implementation handles OCaml style multiline diagnostics,
;; and doesn't need any external scripts.

;; Interesting package to install would be flymake-cursor which provides
;; a mode-line updates with the last diagnostic.

(add-hook
 'typerex-mode-hook
 '(lambda ()
    (set (make-local-variable 'multiline-flymake-mode) t)))

(add-to-list 
 'flymake-err-line-patterns
 `(,(concat "File \"\\(.*\\)\", line \\([0-9]+\\), characters \\([0-9]+\\)"
            "--?\\([0-9]+\\):\\(Error\\|Warning\\): \\(.*\\)$") 1 2 3 6))

(add-to-list 
 'flymake-allowed-file-name-masks 
 '("\\.ml\\'" ocp-flymake-init))

(defun ocp-flymake-init ()
  "Create syntax check ocamlbuild command line for TypeRex projects."
  (let* ((args nil)
         (dir (locate-dominating-file buffer-file-name ".typerex"))
	 (buildfile-dir (flymake-init-find-buildfile-dir dir ".typerex"))
         (source-file-name (file-relative-name buffer-file-name dir))
         (source (replace-regexp-in-string "\.ml$" ".cmo" source-file-name)))
    (list "ocamlbuild" (list "-use-ocamlfind" source) dir)))


;; Code below is taken from the Emacs wiki: http://www.emacswiki.org/emacs/FlymakeHaskell
;; OCaml compiler diagnostics are multiline and flymake that I checked (0.3) that comes
;; with Emacs assumes single line error/warnings messages
;; We use buffer local variable to enable that behavior.

(defvar multiline-flymake-mode
  "Decides whetever to join lines, during flymake parsing of the build command output."
  nil)
(defvar flymake-split-output-multiline nil)

(defadvice flymake-split-output
  (around flymake-split-output-multiline activate protect)
  (if multiline-flymake-mode
      (let ((flymake-split-output-multiline t))
        ad-do-it)
    ad-do-it))
    
(defadvice flymake-split-string
  (before flymake-split-string-multiline activate)
  (when flymake-split-output-multiline
    (ad-set-arg 1 "^\\s *$")))

(provide 'ocp-flymake)
