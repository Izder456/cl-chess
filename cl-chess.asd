;;;; cl-chess.asd

(asdf:defsystem #:cl-chess
  :description "3d accelerated chess game in common lisp"
  :author "izzy Meyer <izder456@disroot.org>"
  :license  "WTFPL"
  :version "betaBetaBeta.1.1.1.0"
  :serial t
  :components ((:file "package")
               (:file "cl-chess")
               (:file "board")))
