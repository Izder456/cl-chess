;;;; cl-chess.asd

(asdf:defsystem #:cl-chess
  :description "3d accelerated chess game in common lisp"
  :author "izzy Meyer <izder456@disroot.org>"
  :license  "WTFPL"
  :version "0.0.1"
  :serial t
  :homepage "https://github.com/izder456/cl-chess"
  :source-control (:git "git@github.com:izder456/cl-chess.git")
  :components ((:file "package")
               (:file "board")
               (:file "cl-chess"))
  :depends-on (:cl-opengl
               :sdl2))
