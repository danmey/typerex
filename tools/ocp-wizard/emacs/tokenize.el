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

; Tokenization and Font-lock with ocp-wizard

; This file registers hooks to let ocp-wizard know about the current
; buffer contents, enabling completion and a fontification.

(defcustom ocp-syntax-coloring t
  "If true, use new TypeRex coloring rather than old tuareg mode"
  :group 'ocp :type '(boolean))

(defcustom ocp-theme nil "coloring theme (one of tuareg_like, syntactic)"
  :group 'ocp :type '(string))

(defcustom ocp-auto-complete nil
  "If true, enable TypeRex auto-completion"
  :group 'ocp :type '(boolean))

; If buffer-bytes = 0, then next update will (re)set the buffer
; contents on the server.
(defvar buffer-bytes 0 "length as known to ocp-wizard")
(make-variable-buffer-local 'buffer-bytes)

; If start <= end then this region has been modified
(defvar buffer-modified-start 1)
(make-variable-buffer-local 'buffer-modified-start)
(defvar buffer-modified-end 1)
(make-variable-buffer-local 'buffer-modified-end)

(defun update-modified (begin end &optional old-len)
  "Enlarge the recorded modified region according to the given
parameters. This function is set as an after-change hook, as well
as a find-file-hook (with point-min and point-max as parameters)."
(unless (eq buffer-bytes 0)
  (if (eq old-len nil) (setq old-len 0))
;  (message "update-modified [%d, %d[ (old-len=%d)" begin end old-len)
  (if (<= buffer-modified-start buffer-modified-end)
      (progn
        (setq buffer-modified-start (min buffer-modified-start begin))
        (setq buffer-modified-end
              (max end
                   (if (<= begin buffer-modified-end)
                       (let ((growth (- (- end begin) old-len)))
                         (+ buffer-modified-end growth))
                     buffer-modified-end))))
    (setq buffer-modified-start begin)
    (setq buffer-modified-end end)
    ;; See auto-completion
    (if (fboundp 'discard-completion-data) (discard-completion-data)))
;  (message "-> [%d, %d["  buffer-modified-start  buffer-modified-end)
  ))

(defun reset-tokenization ()
  "Ensure that the next change committed to the server
will (re-)load the whole buffer"
  (setq buffer-bytes 0))

; We use this hack to get the absolute filename before the
; buffer-local variable has been initialized (when fontifying for the
; first time).
(defun filename-of-buffer-name (buffer-name directory)
  "Try to get the absolute filename for a buffer name and directory name"
  (let* ((len (length buffer-name))
         (filename
          (if (and (> len 3)
                   (char-equal (elt buffer-name (- len 1)) ?>)
                   (char-equal (elt buffer-name (- len 3)) ?<))
              (substring buffer-name 0 (- len 3))
            buffer-name)))
    (expand-file-name filename directory)))

(defun is-ocaml-buffer ()
  (let* ((filename (filename-of-buffer-name (buffer-name) default-directory))
         (len (length filename)))
    (and (> len 4)
         (or
          (string= (substring filename (- len 3)) ".ml")
          (string= (substring filename (- len 4)) ".mli")
          (string= (substring filename (- len 4)) ".mll")
          (string= (substring filename (- len 4)) ".mly")))))

(defun modify-region
  (start end start-bytes end-bytes old-length-bytes first-time)
  "Commit a region modification to the server, and return the
string result, which is either the fontification command, or
OK. All positions count from 1."
;  (message "modify-region [%d, %d[, old=%d, first-time=%s"
;           start-bytes end-bytes old-length-bytes first-time)
  (let* ((filename (filename-of-buffer-name (buffer-name) default-directory))
         (time-before (float-time)))
    (owz-string-command
     (concat "modify-buffer "
             (buffer-name) " "
             filename " "
             (int-to-string (- start-bytes 1)) " "
             (int-to-string (- end-bytes 1)) " "
             (int-to-string old-length-bytes)
             (if first-time " true" " false")
             "\n"
             (buffer-substring start end)))))

(defun try-once-modify-changed-region ()
  "Commit the currently changed region (and set the state to
unchanged)."
;  (message "typerex-fontify-changed-region")
  (if (eq buffer-bytes 0)
      (progn
        (setq buffer-modified-start (point-min))
        (setq buffer-modified-end (point-max))))
  (let* ((last-pos (+ (buffer-size) 1))
         (last-pos-bytes (position-bytes last-pos))
         (new-buffer-bytes (- last-pos-bytes 1))
         (growth-bytes (- new-buffer-bytes buffer-bytes))
         (first-time (eq buffer-bytes 0)))
;  (message "modified-start=%d, modified-end=%d" buffer-modified-start buffer-modified-end)
    (if (<= buffer-modified-start buffer-modified-end)
        (let*
            ((begin-bytes (position-bytes buffer-modified-start))
             (end-bytes (position-bytes buffer-modified-end))
             (old-length-bytes (- (- end-bytes begin-bytes) growth-bytes)))
          (setq buffer-bytes new-buffer-bytes)
          (let ((res
                 (modify-region
                  buffer-modified-start buffer-modified-end
                  begin-bytes end-bytes old-length-bytes first-time)))
            (setq buffer-modified-start last-pos)
            (setq buffer-modified-end 1)
            res))
      nil)))

(defun modify-changed-region ()
  "Same as try-once-modify-change-region, but in case of error,
try to reset tokenization. Actual arguments are ignored; the
modifications are tracked explicitely thanks to update-modified."
  (condition-case e
      (try-once-modify-changed-region)
    (error
     (progn
       (message "Error during tokenization: %s" e)
       (message "Trying to reset tokenization")
       (reset-tokenization)
       (condition-case e
           (try-once-modify-changed-region)
         (error
          (message "Error again: %s\nAbort" e)
          nil))))))

(defun typerex-fontify-changed-region (begin end &optional verbose)
  "Commit any pending modifications and re-fontify the modified part"
  (modify-changed-region)
;        (let ((compute-time (- (float-time) time-before)))
;          (if (> compute-time 0.01)
;              (message "fontification computed in %f seconds" compute-time)))
  (let ((command
         (owz-string-command
          (concat "fontify-buffer " (buffer-name)))))
;      (message "fontification command: %s" command)
    (condition-case e
        (eval (read command))
;        (let ((fontification-time (- (float-time) time-before)))
;          (if (> fontification-time 0.01)
;              (message "fontifying done in %f seconds" fontification-time)))
      (error (message "Error during fontification: %s" e)))))

(defun modify-only-changed-region (begin end &optional verbose)
  "Commit any pending modifications"
  (modify-changed-region))

(defun pre-cache-buffer ()
  (message "Pre-caching program data...")
  (owz-string-command (concat "pre-cache-buffer " (buffer-name))))

;; Install modification hooks and font-lock function
(add-hook
 'typerex-mode-hook
 (lambda ()
   (if (is-ocaml-buffer)
       (progn
         ;; If either coloring or completion are enabled, then track
         ;; modifications in each TypeRex-enabled buffer.
         (if (or ocp-syntax-coloring ocp-auto-complete)
             (progn
               ;; Record each modification
               (add-hook 'after-change-functions 'update-modified nil t)
               ;; Reset fontification when visiting a file, since it may be
               ;; a C-x C-w and change the buffer name
               (add-hook 'find-file-hook 'reset-tokenization nil t)))
         ;; If coloring is enabled, modification are commited by the
         ;; typerex-fontify-changed-region, which we register by redefining
         ;; typerex-install-font-lock.
         (make-local-variable 'font-lock-fontify-region-function)
         (if ocp-syntax-coloring
             (setq font-lock-fontify-region-function
                   'typerex-fontify-changed-region))
         ;; If auto-completion is enabled), modifications
         ;; are also commited after each change (this hook is appended).
;         (if ocp-auto-complete
;             (add-hook 'after-change-functions 'modify-only-changed-region t t))
         (if ocp-auto-complete
             (add-hook
              'find-file-hook
              (lambda () (run-with-idle-timer 0 nil 'pre-cache-buffer))
              t t))
         ))))
