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

; Emacs lisp implementation of our RPC protocol.

; Usage:

; - Connect to the listening server using by evaluating
;     (start-connection port-number)
;   or setup a server with
;     (start-rpc-server)
;   The protocol is the one implemented by class
;     Server.tagged_connection

; - Then use
;     (string-command command)
;   which behaves as method send_tagged_command, to execute requests.

; - Callbacks are processed by calling the function process-callback
;   on the buffer which contains the contents. Contents should be read
;   from the point. The process-callback function is responsible for
;   error encoding.

; - Accepting requests is not implemented, since we don't use it in
;   TypeRex because Emacs initiates the requests.

(defconst end-of-message "END_OF_MESSAGE"
  "mark for the client to detect the end of the answer")

(defconst request-start "REQUEST_START"
  "mark for the client to detect the beginning of a request")

(defvar connection-buffer nil
  "all input received from ocp-wizard goes here")

(defvar current-callback-start nil
  "start of a callback, if any")

(defvar current-answer-start nil
  "start of an answer, if any")

(defvar next-message-start 1
  "start of an answer, if any")

(defvar last-answer nil
  "region containing the last answer")

(defvar connection nil
  "connection process, (used, e.g. for sending data)")

(defvar connection-server nil
  "server process")

(defun connection-filter (proc string)
;  (message "inserting %d chars" (length string))
  (with-current-buffer connection-buffer
; always go to the end BEFORE (in case the user moves the pointer)
    (goto-char (point-max))
    (insert string)
; try to determine the nature of the next message
    (if (not (eq next-message-start nil))
        (progn
;        (message "message starts at %s" next-message-start)
          (goto-char next-message-start)
          (if (< (line-number-at-pos next-message-start)
                 (line-number-at-pos (point-max)))
              (progn
                (end-of-line)
                (let ((next-line (buffer-substring next-message-start (point))))
;                (message "next line complete")
                  (if (string= next-line request-start)
                      (setq current-callback-start (+ (point) 1))
                    (setq current-answer-start next-message-start))
                  (setq next-message-start nil))))
          (goto-char (point-max)))))
; if we are receiving a callback
  (if (not (eq current-callback-start nil))
      (let ((complete
             (with-current-buffer connection-buffer
               (forward-line -1)
               (let ((bol (point)))
                 (end-of-line)
                 (let ((last-line (buffer-substring bol (point))))
                   (if (string= last-line end-of-message)
                       (progn
                         (goto-char current-callback-start)
                         (setq current-callback-start nil)
                         (setq next-message-start (point-max))
;                    (message "received message %s" (buffer-substring (point) (point-max)))
                         t)
                     nil))))))
        (if complete
            (let ((result (process-callback connection-buffer)))
              (process-send-string connection
                                   (concat result "\n" end-of-message "\n"))
              )))
; if we are receiving an answer
    (if (not (eq current-answer-start nil))
        (with-current-buffer connection-buffer
;            (message "reading answer from %d" current-answer-start)
          (forward-line -1)
          (let ((bol (point)))
            (end-of-line)
            (let ((last-line (buffer-substring bol (point))))
              (if (string= last-line end-of-message)
                  (progn
;                      (message "answer ends at %d" (- bol 1))
                    (setq last-answer `(,current-answer-start ,(- bol 1)))
                    (setq current-answer-start nil)
                    (setq next-message-start (point-max))))))))))

(defun get-answer ()
  "get an expected answer"
  (let ((quit-inhibited inhibit-quit))
    (setq inhibit-quit t)
    (while (and (eq last-answer nil) (eq (process-status connection) 'open))
      (accept-process-output connection 1 0))
    (unless (eq (process-status connection) 'open)
      (signal 'error '("Error: connection closed")))
    (let ((answer last-answer))
      (setq last-answer nil)
;    (message "last answer = [%d, %d[" (car answer) (cadr answer))
      (setq inhibit-quit quit-inhibited)
      answer))
  )

(defun delete-until (p)
  (let ((shift (- p (point-min))))
    (with-current-buffer connection-buffer
      (delete-region (point-min) p))
    (unless (eq next-message-start nil)
      (setq next-message-start (- next-message-start shift)))
    (unless (eq current-answer-start nil)
      (setq current-answer-start (- current-answer-start shift)))
    (unless (eq current-callback-start nil)
      (setq current-callback-start (- current-callback-start shift)))))

(defun string-command (c)
;  (message "sending command %s" c)
  (process-send-string connection
                       (concat request-start "\n" c "\n" end-of-message "\n"))
;  (message "command sent:   %s" c)
  (let*
      ((answer-region (get-answer))
       (answer
        (with-current-buffer connection-buffer
          (buffer-substring (car answer-region) (cadr answer-region)))))
;    (message "received answer %s" answer)
    (delete-until (cadr answer-region))
    answer))

(defun start-connection (port)
  "start connection by listening on specified port"
  (setq max-lisp-eval-depth 10000)
  (setq connection-buffer (get-buffer-create-clear " *output-from-ocp-wizard*"))
  (setq next-message-start 1)
  (setq current-answer-start nil)
  (setq current-callback-start nil)
  (setq last-answer nil)
  (setq connection
        (open-network-stream "ocp-wizard-client" nil 'local port))
  (set-process-filter connection 'connection-filter)
  (set-process-query-on-exit-flag connection nil)
  )

(defun connection-sentinel (process event)
  (if (string= (substring event 0 4) "open")
      (progn
;        (message "connected")
        (setq connection process)
        (set-process-query-on-exit-flag connection nil))
    ))

(defun start-rpc-server ()
  "start a server connection and return the listening port"
  (setq max-lisp-eval-depth 10000)
  (setq connection-buffer (get-buffer-create-clear " *output-from-ocp-wizard*"))
  (setq next-message-start 1)
  (setq current-answer-start nil)
  (setq current-callback-start nil)
  (setq last-answer nil)
  (setq connection nil)
  (setq connection-server
        (make-network-process
         :name "ocp-connection"
         :server 0 :host "127.0.0.1" :family 'ipv4 :service t :reuseaddr))
  (set-process-sentinel connection-server 'connection-sentinel)
  (set-process-filter connection-server 'connection-filter)
  (set-process-query-on-exit-flag connection-server nil)
  (set-process-coding-system connection-server 'emacs-mule 'emacs-mule)
;; from the emacs-devel:
;; The byte sequence of a buffer after decoded is
;; always in emacs-mule (in emacs-unicode-2 branch, it's
;; utf-8).  So, changing buffer-file-coding-system or any other
;; coding-system-related variables doesn't affects
;; position-bytes.
  (process-contact connection-server :service)
  )
