;;; images.el --- Automatic image converters

;; Copyright (c) 1996-1999, 2007, 2013 Free Software Foundation Inc.

;; Author: $Author: legoscia $
;; Created: $Date: 2007/11/15 12:22:34 $
;; Keywords: images

;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(defvar image-temp-stack nil "Do no touch - internal storage.")
(defvar image-converters nil "Storage for the image converters.")
(defvar image-native-formats
  (cond
   ((featurep 'xemacs)
    (delq nil (cons (if (featurep 'x) 'xbm)
		    (mapcar (function (lambda (x) (if (featurep x) x)))
			    '(xpm gif jpeg tiff png imagick)))))
   ((boundp 'image-types)
    image-types)
   (t nil))
  "A list of image formats that this version of emacs supports natively.")

(defun image-register-converter (from to converter)
  "Register the image converter for FROM to TO.  CONVERTER is the actual
command used to convert the image.  If this is a string, it will be executed
in a subprocess.  If a symbol, it is assumed to be a function.  It will be
called with two arguments, the start and end of the data to be converted.
The function should replace that data with the new image data.  The return
value is not significant."
  (let* ((node (assq from image-converters))
	 (replace (assq to (cdr-safe node))))
    (cond
     (replace				; Replace existing converter
      (setcdr replace converter)
      (display-warning 'image (format "Replacing image converter %s->%s"
				      from to)))
     (node				; Add to existing node
      (setcdr node (cons (cons to converter) (cdr node))))
     (t					; New toplevel converter
      (setq image-converters (cons (cons from (list (cons to converter)))
				   image-converters))))))

(defun image-unregister-converter (from to)
  "Unregister the image converter for FROM to TO"
  (let* ((node (assq from image-converters))
	 (tos (cdr-safe node))
	 (new nil))
    (while tos
      (if (eq to (car (car tos)))
	  nil
	(setq new (cons (car tos) new)))
      (setq tos (cdr tos)))
    (setcdr node new)))

(defun image-converter-registered-p (from to)
  (cdr-safe (assq to (cdr-safe (assq from image-converters)))))

(defun image-converter-chain (from to)
  "Return the shortest converter chain for image format FROM to TO"
  (setq image-temp-stack (cons from image-temp-stack))
  (let ((converters (cdr-safe (assq from image-converters)))
	(thisone nil)
	(possibles nil)
	(done nil))
    (while (and (not done) converters)
      (setq thisone  (car converters))
      (cond
       ((eq (car thisone) to)
	(setq done t))
       ((memq (car thisone) image-temp-stack)
	nil)
       (t
	(setq possibles (cons (image-converter-chain (car thisone) to)
			      possibles))))
      (setq converters (cdr converters)))
    (setq image-temp-stack (cdr image-temp-stack)
	  possibles (sort (delq nil possibles)
			  (function
			   (lambda (x y)
			     (< (length (delete 'ignore x))
				(length (delete 'ignore y)))))))
    (if (not done)
	(setq done (car possibles)))
    (cond
     ((eq done t) (list (cdr thisone)))
     (done (setq done (cons (cdr thisone) done)))
     (t nil))))

(defun image-normalize (format data)
  "Return an image specification for XEmacs 19.13 and later.  FORMAT specifies
the image format, DATA is the image data as a string.  Any conversions to get
to a suitable internal image format will be carried out."
  (setq image-temp-stack nil)
  (if (stringp format) (setq format (intern format)))
  (if (not (memq format image-native-formats))
      (let* ((winner (car-safe
		      (sort (mapcar
			     (function
			      (lambda (x)
				(cons x
				      (delete 'ignore
					      (image-converter-chain format
								     x)))))
				    image-native-formats)
			    (function
			     (lambda (x y)
			       (cond
				((null (cdr x)) nil)
				((= (length (cdr x))
				    (length (cdr y)))
				 (< (length (memq (car x)
						  image-native-formats))
				    (length (memq (car y)
						  image-native-formats))))
				(t
				 (< (length (cdr x))
				    (length (cdr y))))))))))
	     (type (car-safe winner))
	     (chain (cdr-safe winner))
	     )
	(if chain
	    (with-current-buffer (generate-new-buffer " *image-conversion*")
	      (erase-buffer)
              (set-buffer-multibyte nil)
	      (insert data)
	      (while chain
		(cond
		 ((stringp (car chain))
                  (call-process-region
                   (point-min) (point-max)
                   shell-file-name t
                   (list (current-buffer) nil)
                   ;; FIXME: Shouldn't there be a nil before this arg?
                   shell-command-switch
                   (car chain)))
		 ((and (symbolp (car chain)) (fboundp (car chain)))
		  (funcall (car chain) (point-min) (point-max))))
		(setq chain (cdr chain)))
	      (setq data (buffer-string))
	      (kill-buffer (current-buffer)))
	  (setq type format))
	(vector type ':data data))
    (vector format ':data data)))

(defun image-register-netpbm-utilities ()
  "Register all the netpbm utility packages converters."
  (interactive)
  (if (image-converter-registered-p 'pgm 'pbm)
      nil
    (image-register-converter 'pgm 'pbm "pgmtopbm")
    (image-register-converter 'ppm 'pgm "ppmtopgm")
    (image-register-converter 'pnm 'xpm "ppmtoxpm")
    (image-register-converter 'ppm 'xpm "ppmtoxpm")
    (image-register-converter 'xpm 'ppm "xpmtoppm")
    (image-register-converter 'gif 'ppm "giftopnm")
    (image-register-converter 'pnm 'gif "(ppmquant 256 | ppmtogif)")
    (image-register-converter 'ppm 'gif "(ppmquant 256 | ppmtogif)")
    (image-register-converter 'bmp 'ppm "bmptoppm")
    (image-register-converter 'ppm 'bmp "ppmtobmp")
    (image-register-converter 'ppm 'ps "pnmtops")
    (image-register-converter 'pnm 'ps "pnmtops")
    (image-register-converter 'ps 'pnm "pstopnm")
    (image-register-converter 'g3  'pbm "g3topbm")
    (image-register-converter 'macpt 'pbm "macptopbm")
    (image-register-converter 'pbm 'macpt "pbmtomacp")
    (image-register-converter 'pcx 'ppm "pcxtoppm")
    (image-register-converter 'ppm 'pcx "ppmtopcx")
    (image-register-converter 'pict 'ppm "picttoppm")
    (image-register-converter 'ppm 'pict "ppmtopict")
    (image-register-converter 'pnm 'sgi "pnmtosgi")
    (image-register-converter 'tga 'ppm "tgatoppm")
    (image-register-converter 'ppm 'tga "ppmtotga")
    (image-register-converter 'sgi 'pnm "sgitopnm")
    (image-register-converter 'tiff 'pnm "tifftopnm")
    (image-register-converter 'pnm 'tiff "pnmtotiff")
    (image-register-converter 'xbm 'pbm "xbmtopbm")
    (image-register-converter 'pbm 'xbm "pbmtoxbm")
    (image-register-converter 'png 'pnm "pngtopnm")
    (image-register-converter 'pnm 'png "pnmtopng")
    (image-register-converter 'pnm 'jbg "pbmtojbg")
    (image-register-converter 'jbg 'pnm "jbgtopbm")
    (image-register-converter 'jpeg 'ppm "djpeg")))
   
(provide 'images)
