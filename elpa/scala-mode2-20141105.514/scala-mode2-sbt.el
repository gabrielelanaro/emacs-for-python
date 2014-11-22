;;; scala-mode-lib.el - Major mode for editing scala, common functions
;;; Copyright (c) 2012 Heikki Vesalainen
;;; For information on the License, see the LICENSE file

(require 'comint)

(define-obsolete-function-alias 'scala-sbt:start 'sbt-start "2013-11-16" "scala-sbt:start has been obsoleted. Please use the sbt-start from the sbt-mode package directly")
(define-obsolete-function-alias 'scala-sbt:command 'sbt-command "2013-11-16" "scala-sbt:command has been obsolted. Please use the sbt-command from the sbt-mode package directly")

(provide 'scala-mode2-sbt)
