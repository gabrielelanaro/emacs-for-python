;;; -*- emacs-lisp -*-
;;; scrartcl.el -- AUCTeX style for scrartcl.cls

;; Copyright (C) 2002, 2005 Free Software Foundation
;; License: GPL, see the file COPYING in the base directory of AUCTeX

;; Author: Mark Trettin <Mark.Trettin@gmx.de>
;; Created: 2002-09-26
;; Keywords: tex

;;; Commentary:

;; This file adds support for `scrartcl.cls'. This file needs
;; `scrbase.el'.

;; This file is part of  AUCTeX.

;;; Code:
(TeX-add-style-hook "scrartcl"
   (lambda ()
     (LaTeX-largest-level-set "section")
     ;; load basic definitons
     (TeX-run-style-hooks "scrbase"))
   LaTeX-dialect)

;;; scrartcl.el ends here
