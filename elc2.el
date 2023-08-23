;;; elc2.el --- remote execution of elisp -*- lexical-binding: t; -*-
;; Copyright (C) 2021  ellis
;; 
;; Author: ellis
;; Keywords: local, vc, net, process
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;; 
;; Commentary:
;; 
;; This package provides functions for executing elisp on a running
;; emacs instance remotely.
;; 
;;; Code:
;;;; Custom
(defgroup elc2 nil
  "elisp server")

(defcustom elc2-dir "~/elc2" "elc2 directory."
  :group 'elc2)

(defcustom elc2-after-make-frame-hook nil
  "Hook run when elc2 creates a client frame.
The created frame is selected when the hook is called."
  :type 'hook
  :group 'elc2)

(defcustom elc2-done-hook nil
  "Hook run when done editing a buffer with elc2."
  :type 'hook
  :group 'elc2)

(defcustom elc2-port 62824
  "port of the elc2 broadcaster"
  :group 'elc2)

(defvar elc2-process nil
  "The elc2 process handle.")

(defvar elc2-clients nil
  "List of current elc2 clients.
Each element is a process.")

;;;; Bindat
(setq elc2-header-bindat-spec
      '((dest-ip   ip)
        (dest-port u16)
        (src-ip    ip)
        (src-port  u16)))

(setq elc2-body-bindat-spec
      '((type      u8)
        (opcode    u8)
        (length    u16)  ; network byte order
        (id        strz 8)
        (data      vec (length))
        (align     4)))

(setq elc2-packet-bindat-spec
      '((header    struct header-spec)
        (counters  vec 2 u32r)   ; little endian order
        (items     u8)
        (fill      3)
        (item      repeat (items)
                   (struct data-spec))))

(defun elc2-insert-string (string)
  (insert string 0 (make-string (- 3 (% (length string) 4)) 0)))

(defun elc2-insert-int32 (value)
  (let (bytes)
    (dotimes (i 4)
      (push (% value 256) bytes)
      (setq value (/ value 256)))
    (dolist (byte bytes)
      (insert byte))))

(defun elc2-insert-float32 (value)
  (let (s (e 0) f)
    (cond
     ((string= (format "%f" value) (format "%f" -0.0))
      (setq s 1 f 0))
     ((string= (format "%f" value) (format "%f" 0.0))
      (setq s 0 f 0))
     ((= value 1.0e+INF)
      (setq s 0 e 255 f (1- (expt 2 23))))
     ((= value -1.0e+INF)
      (setq s 1 e 255 f (1- (expt 2 23))))
     ((string= (format "%f" value) (format "%f" 0.0e+NaN))
      (setq s 0 e 255 f 1))
     (t
      (setq s (if (>= value 0.0)
		  (progn (setq f value) 0)
		(setq f (* -1 value)) 1))
      (while (>= (* f (expt 2.0 e)) 2.0) (setq e (1- e)))
      (if (= e 0) (while (< (* f (expt 2.0 e)) 1.0) (setq e (1+ e))))
      (setq f (round (* (1- (* f (expt 2.0 e))) (expt 2 23)))
	    e (+ (* -1 e) 127))))
    (insert (+ (lsh s 7) (lsh (logand e #XFE) -1))
	    (+ (lsh (logand e #X01) 7) (lsh (logand f #X7F0000) -16))
	    (lsh (logand f #XFF00) -8)
	    (logand f #XFF))))

(defun elc2-read-string ()
  (let ((pos (point)) string)
    (while (not (= (following-char) 0)) (forward-char 1))
    (setq string (buffer-substring-no-properties pos (point)))
    (forward-char (- 4 (% (length string) 4)))
    string))

(defun elc2-read-int32 ()
  (let ((value 0))
    (dotimes (i 4)
      (setq value (logior (* value 256) (following-char)))
      (forward-char 1))
    value))

(defun elc2-read-float32 ()
  (let ((s (lsh (logand (following-char) #X80) -7))
	(e (+ (lsh (logand (following-char) #X7F) 1)
	      (lsh (logand (progn (forward-char) (following-char)) #X80) -7)))
	(f (+ (lsh (logand (following-char) #X7F) 16)
	      (lsh (progn (forward-char) (following-char)) 8)
	      (prog1 (progn (forward-char) (following-char)) (forward-char)))))
    (cond
     ((and (= e 0) (= f 0))
      (* 0.0 (expt -1 s))
      ((and (= e 255) (or (= f (1- (expt 2 23))) (= f 0)))
       (* 1.0e+INF (expt -1 s)))
      ((and (= e 255) (not (or (= f 0) (= f (1- (expt 2 23))))))
       0.0e+NaN)
      (t
       (* (expt -1 s)
	  (expt 2.0 (- e 127))
	  (1+ (/ f (expt 2.0 23)))))))))

;;;; Network
;;;###autoload
(defun net-check-opts ()
  ;; https://gnu.huihoo.org/emacs/24.4/emacs-lisp/Network-Options.html#Network-Options
  ;; non-blocking
  (featurep 'make-network-process '(:nowait t))
  ;; UNIX socket
					;(featurep 'make-network-process '(:family local))
  ;; UDP
  (featurep 'make-network-process '(:type datagram)))

;;;; Process
(defun elc2-make-client (host port)
  (make-network-process
   :name "elc2-client"
   :coding 'binary
   :host host
   :service port
   :type 'datagram
   :nowait t))

(defun elc2-sentinel (proc msg)
  (when (string= msg "connection broken by remote peer\n")
    (setq elc2-clients (assq-delete-all proc elc2-clients))
    (elc2-log (format "client %s has quit" proc))))

;;from server.el
(defun elc2-log (string &optional client)
  "If a *elc2* buffer exists, write STRING to it for logging purposes."
  (if (get-buffer "*elc2*")
      (with-current-buffer "*elc2*"
        (goto-char (point-max))
        (insert (if client (format "<%s>: " (format-network-address (process-datagram-address client))))
                string)
        (or (bolp) (newline)))))

;;;###autoload
(defun elc2-start nil
  "start elc2 over udp"
  (interactive)
  (unless (process-status "elc2")
    (make-network-process :name "elc2"
			  :buffer "*elc2*"
			  :family 'ipv4
			  :service elc2-port
			  :type 'datagram
			  :coding 'binary
			  :sentinel 'elc2-sentinel
			  :filter 'elc2-filter
			  :server t
			  :broadcast t) 
    (setq elc2-clients '())

    ;; setup additional filters
    (add-function :after (process-filter (get-process "elc2")) #'elc2-eval-response-filter))
  (message "elc2: ONLINE"))

;;;###autoload
(defun elc2-stop ()
  "stop the elc2 server."
  (interactive)
  (while  elc2-clients
    (delete-process (car (car elc2-clients)))
    (setq elc2-clients (cdr elc2-clients)))
  (with-current-buffer "*elc2*"
    (let ((proc (get-buffer-process (current-buffer))))
      (if proc (delete-process proc)))
    (set-buffer-modified-p nil)
    (kill-this-buffer))
  (message "elc2 stopped"))

(defun elc2-filter (proc string)   
  (let ((pending (assoc proc elc2-clients))
        message
        index)
    ;;create entry if required
    (unless pending
      (setq elc2-clients (cons (cons proc "") elc2-clients))
      (setq pending  (assoc proc elc2-clients)))
    (setq message (concat (cdr pending) string))
    (while (setq index (string-match "\n" message))
      (setq index (1+ index))
;      (process-send-string proc (substring message 0 index))
      (elc2-log  (substring message 0 index) proc)
      (setq message (substring message index)))
    (setcdr pending message)))

(defun elc2-packet-filter (proc string)
  "process-filter for decoding 'elc2-packet-bindat-spec'"
  (bindat-unpack packet-spec string))

(defun ordinary-insertion-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))

        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc)))))))

(defun elc2-eval-response-filter (proc string)
  "execute STRING from PROC."
  (let ((msg (car (read-from-string string))))
    (process-send-string proc (concat (format "%s" (ignore-errors "error: %S" (eval msg))) "\n"))))

;;;; Signals
;;;###autoload
(defun elc2-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

;;;###autoload
(defun elc2-restart ()
  "Handler for SIGUSR1 signal, to (re)start an emacs server.

Can be tested from within emacs with:
  (signal-process (emacs-pid) 'sigusr1)

or from the command line with:
$ kill -USR1 <emacs-pid>
$ emacsclient -c
"
  (interactive)
  (server-force-delete)
  (server-start)
  )

(define-key special-event-map [sigusr1] 'elc2-restart)

;;;; provide
(provide 'elc2)
;;; elc2.el ends here
