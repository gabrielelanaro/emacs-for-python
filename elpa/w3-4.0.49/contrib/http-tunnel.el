;;; http-tunnel.el --- Tunneling a connection through http?

;; Copyright (c) 2013  Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(defvar http-tunnel-host "firewallmachine")
(defvar http-tunnel-port 80)

(defun open-http-tunneled-connection (name buffer host service)
  (let ((proc (open-network-stream name buffer http-tunnel-host http-tunnel-port))
	(need-to-spin t))
    (if (or (null proc) (not (memq (process-status proc) '(run open))))
	(error "Could not open connection"))
    (process-send-string proc (format (eval-when-compile
					(concat
					 "CONNECT %s:%s HTTP/1.0\r\n"
					 "User-Agent: Emacs/%d.%d\r\n"
					 "\r\n"))
				      host service
				      emacs-major-version
				      emacs-minor-version))
    (with-current-buffer buffer
      (while (and (memq (process-status proc) '(open run)) need-to-spin)
	(accept-process-output proc 3)
	(goto-char (point-min))
	(if (re-search-forward "\r\n\r\n" nil t)
	    (progn
	      (delete-region (point-min) (match-end 0))
	      (setq need-to-spin nil)))
	(goto-char (point-max))))
    (if (not (memq (process-status proc) '(open run)))
	(error "Could not open connection"))
    proc))

(let ((socks-noproxy '(".*")))
  (setq x (open-http-tunneled-connection "x" "x" "www.cs.indiana.edu" 80))
  (process-send-string x "GET / HTTP/1.0\r\n\r\n"))
