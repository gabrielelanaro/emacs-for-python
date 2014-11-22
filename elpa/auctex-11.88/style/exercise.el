;;; exercise.el --- AUCTeX style for `exercise.sty'

;; Copyright (C) 2014 Free Software Foundation, Inc.

;; Author: Nicolas Richard <theonewiththeevillook@yahoo.fr>
;; Created: 2014-03-17
;; Keywords: tex

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; AUCTeX is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with AUCTeX; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; This file adds support for `exercise.sty'.

;;; Code:

(TeX-add-style-hook
 "exercise"
 (lambda ()
   (LaTeX-add-environments
    '("Exercise")
    '("Exercise*")
    '("Answer")
    '("ExerciseList")
    )
   (TeX-add-symbols
    '("Exercise")
    '("Exercise*")
    '("Answer")
    '("ExePart")
    '("ExePart*")
    '("Question")
    '("subQuestion")
    '("ExeText")
    '("ExerciseSelect")
    '("ExerciseStopSelect")
    '("refAnswer")
    '("marker")
    '("DifficultyMarker")
    '("listofexercises")
    '("ListOfExerciseInToc")
    '("ExerciseLevelInToc")))
 LaTeX-dialect)

(defvar LaTeX-exercise-package-options '("noexercise" "noanswer" "exerciseonly" "answeronly" "nothing" "answerdelayed" "exercisedelayed" "lastexercise")
  "Package options for the exercise package.")

;;; exercise.el ends here
