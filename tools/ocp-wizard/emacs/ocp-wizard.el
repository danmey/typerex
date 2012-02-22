;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                        ;
;                        TypeRex OCaml Studio                            ;
;                                                                        ;
;                           Tiphaine Turpin                              ;
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


; If we keep tuareg separately, we could use 'typerex-load-hook
;(add-hook 'typerex-mode-hook 'start-ocp-wizard-server)

(ocp-restart-server)

;; Plugin with menus
;;;;;;;;;;;;;;;;;;;;

(defcustom ocp-menu-trigger nil
  "mouse event to trigger the contextual menu (default nil)"
  :group 'ocp)

(defun ocp-wizard-menu-plugin ()
  "Register the commands as a keyboard menu"
  (defvar ocp-prefix (make-sparse-keymap "OCP"))
;  (define-prefix-command 'ocp-prefix nil "OCP")
  (define-key typerex-mode-map [(control o)] ocp-prefix)

  (defvar ocp-prefix-top (make-sparse-keymap "Toplevel"))
;  (define-prefix-command 'ocp-prefix-top nil "Toplevel")
;  (define-key typerex-mode-map [(t)] 'ocp-prefix-top)
  (define-key ocp-prefix-top [(r)] '("Rename" . ocp-rename-toplevel))
  (define-key ocp-prefix-top [(g)] '("Grep" . ocp-grep-toplevel))

  (define-key ocp-prefix [(u)] `("Undo (global)" . ocp-undo))
  (define-key ocp-prefix [(q)] '("Qualify" . ocp-eliminate-open))
  (define-key ocp-prefix [(p)] '("Prune" . ocp-prune-lids))
  (define-key ocp-prefix [(t)] `("Toplevel-(Rename/Grep)" . ,ocp-prefix-top))
  (define-key ocp-prefix [(r)] '("Rename" . ocp-rename))
  (define-key ocp-prefix [(g)] '("Grep" . ocp-grep))
  (define-key ocp-prefix [(a)] '("Alternate definitions" . ocp-cycle-definitions))
  (define-key ocp-prefix [(d)] '("Definition" . ocp-goto-definition))
  (define-key ocp-prefix [(c)] '("Comment" . ocp-comment-definition))

;;   (defvar typerex-mode-menu)
;; ;; keys are irrelevant, but should be all different !
;;   (define-key typerex-mode-menu [(separator)] '(menu-item "---"))
;;   (mapc
;;    (lambda (c)
;;      (define-key typerex-mode-menu (cadr (cdr c))
;;     `(menu-item ,(cadr c) ,(car c) ,:help ,(documentation (car c)))))
;;    '(
;;      (ocp-restart-server "Restart" "s")
;;      (ocp-undo "Undo (global)" "u")
;;      (ocp-eliminate-open "Qualify" "q")
;;      (ocp-prune-lids "Prune" "p")
;;      (ocp-rename "Rename" "r")
;;      (ocp-grep "Grep" "g")
;;      (ocp-cycle-definitions "Alternate definitions" "a")
;;      (ocp-goto-definition "Definition" "d")
;;      (ocp-comment-definition "Comment" "c")
;;      (ocp-rename-toplevel "Toplevel Rename" [(tr)])
;;      (ocp-grep-toplevel "Toplevel Grep" [(tg)])
;;      ))
  (when ocp-menu-trigger
;    (add-hook
;     'typerex-mode-hook
;     (lambda ()
    (define-key (current-local-map) ocp-menu-trigger ocp-prefix))
  )

(defun action-item (c)
  `[,(cadr c) ,(car c) ,:help ,(documentation (car c))])

(defun typerex-build-menu ()
  (easy-menu-define
   typerex-mode-menu (list typerex-mode-map)
   "TypeRex Mode Menu."
   (append
   '("TypeRex")
   (mapcar
    'action-item
    '(
      (ocp-grep-toplevel "Toplevel Grep")
      (ocp-rename-toplevel "Toplevel Rename")
      (ocp-comment-definition "Comment")
      (ocp-goto-definition "Definition")
      (ocp-cycle-definitions "Alternate definitions")
      (ocp-grep "Grep")
      (ocp-rename "Rename")
      (ocp-prune-lids "Prune")
      (ocp-eliminate-open "Qualify")
      (ocp-undo "Undo (global)")
    ))
   `(
   "---"
     [ "Show type at point" caml-types-show-type
       typerex-with-caml-mode-p]
     [ "Help for identifier" caml-help
       typerex-with-caml-mode-p]
     [ "Show fully qualified ident at point" caml-types-show-ident
       typerex-with-caml-mode-p]
     ["Switch .ml/.mli" typerex-find-alternate-file t]
   "---"
     ["Compile..." compile t]

     ("Interactive Mode"
      ["Run Caml Toplevel" typerex-run-caml t]
      ["Interrupt Caml Toplevel" typerex-interrupt-caml
       :active (comint-check-proc typerex-interactive-buffer-name)]
      ["Kill Caml Toplevel" typerex-kill-caml
       :active (comint-check-proc typerex-interactive-buffer-name)]
      ["Evaluate Region" typerex-eval-region
       ;; Region-active-p for XEmacs and mark-active for Emacs
       :active (if (fboundp 'region-active-p) (region-active-p) mark-active)]
      ["Evaluate Phrase" typerex-eval-phrase t]
      ["Evaluate Buffer" typerex-eval-buffer t])
   "---"

     ("Caml Forms"
      ["try .. with .." typerex-insert-try-form t]
      ["match .. with .." typerex-insert-match-form t]
      ["let .. in .." typerex-insert-let-form t]
      ["if .. then .. else .." typerex-insert-if-form t]
      ["while .. do .. done" typerex-insert-while-form t]
      ["for .. do .. done" typerex-insert-for-form t]
      ["begin .. end" typerex-insert-begin-form t])

     ("Definitions"
      ["Scan..." typerex-list-definitions t])
     ;; [ "Complete identifier" caml-complete
     ;;   typerex-with-caml-mode-p]
     [ "Add path for documentation" ocaml-add-path
       typerex-with-caml-mode-p]
     [ "Open module for documentation" ocaml-open-module
       typerex-with-caml-mode-p]
     [ "Close module for documentation" ocaml-close-module
       typerex-with-caml-mode-p]
     "---"
     ("More"
     ,(action-item '(ocp-restart-server "Restart"))
     ["Customize TypeRex Mode..." (customize-group 'ocp) t]
     ("TypeRex Options" ["Dummy" nil t])
     ("TypeRex Interactive Options" ["Dummy" nil t])
     ["Short Cuts" typerex-short-cuts]
     ["TypeRex Mode Help" typerex-help t]
     ["OCaml Reference Manual..." typerex-browse-manual t]
     ["OCaml Library..." typerex-browse-library t]
     ["About" typerex-about t])

     )))
  (easy-menu-add typerex-mode-menu)
  (typerex-update-options-menu)
  ;; Save and update definitions menu
  (if typerex-with-xemacs
      (add-hook 'activate-menubar-hook 'typerex-update-definitions-menu)
    (when (functionp 'easy-menu-create-menu)
      ;; Patch for Emacs
      (add-hook 'menu-bar-update-hook
                'typerex-with-emacs-update-definitions-menu)
      (make-local-variable 'typerex-definitions-keymaps)
      (setq typerex-definitions-keymaps
            (cdr (easy-menu-create-menu
                  "Definitions" typerex-definitions-menu)))
      (setq typerex-definitions-menu-last-buffer nil))))


; This overwrites the bindings of ocp-wizard-plugin, so this file
; should be evaluated after hooks are evaluated
;(ocp-wizard-menu-plugin)
(add-hook 'typerex-mode-hook 'ocp-wizard-menu-plugin)
