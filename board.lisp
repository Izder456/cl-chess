;;;; board.lisp

(in-package #:cl-chess)

(require :sdl2)
(require :cl-opengl)

;; Define colors for light and dark squares
(defparameter *light-col* (list 0.98 0.86 0.7)) ; White color for light squares
(defparameter *dark-col* (list 0.08 0.08 0.08)) ; Black color for dark squares

(defun draw-square (x y size color)
  "Function to draw a square"
  (declare (ignorable color))
  (gl:with-pushed-matrix
    (gl:translate x y 0)
    (gl:with-primitive :quads
      (gl:vertex 0 0)
      (gl:vertex 0 size)
      (gl:vertex size size)
      (gl:vertex size 0))))

(defun create-graphical-board (size)
  "Function for Drawing Checkered board"
  (dotimes (file 8)
    (dotimes (rank 8)
      (let ((is-light-square? (= 0 (rem (+ rank file) 2))))
        (apply #'gl:color (if is-light-square? *light-col* *dark-col*))
        (draw-square file rank size (if is-light-square? *light-col* *dark-col*))))))

(defun draw-window (window-size)
  "Function to draw the window"
  (sdl2:with-init (:video)
    (let* ((window (sdl2:create-window :title "Chess Board" :w window-size :h window-size))
           (renderer (sdl2:create-renderer window)))

      ;; Set the OpenGL viewport
      (gl:viewport 0 0 window-size window-size)

      ;; Set the projection matrix
      (gl:matrix-mode :projection)
      (gl:load-identity)
      (gl:ortho 0 8 0 8 -1 1)

      ;; Main loop
      (sdl2:with-event-loop (:method :poll)
        (:idle ()
          (gl:clear :color-buffer) ; Clear the color buffer
          (gl:matrix-mode :modelview) ; Switch to modelview matrix mode
          (gl:load-identity) ; Reset the current matrix

          (create-graphical-board 1) ; Pass 1 as the size since the viewport handles scaling

          (gl:flush) ; Ensure all commands are executed

          (sdl2:gl-swap-window window) ; Swap the front and back buffers
          (sdl2:delay 1000))

        (:quit ()
          (sdl2:destroy-renderer renderer)
          (sdl2:destroy-window window)
          t))))) ;; Return t to indicate a successful event handle.

;; Open the window with size
;; (draw-window 480)
