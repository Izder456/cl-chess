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

(defun setup-frustum-projection (width height near far)
  "Some Yucky Math Here"
  ;; bind relevant to the tangent of half of the field of view
  (let* ((aspect-ratio (/ width height))
	 (tan-half-fov (tan (/ 45.0 2.0))) ;; 45 Degree FOV
	 (left (- (* near tan-half-fov)))
	 (right (* near tan-half-fov))
	 (bottom (- (* near tan-half-fov (/ aspect-ratio))))
	 (top (* near tan-half-fov (/ aspect-ratio))))
    ;; setup ogl frustum
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (gl:frustum left right bottom top near far)
    (gl:matrix-mode :modelview)
    (gl:load-identity)))

(defun init-sdl2-and-gl (width height)
  "Initialize SDL2 and GL"
  ;; start sdl with video subsystem
  (sdl2:init :video)

  ;; create window with ogl support
  (let ((window (sdl2:create-window :title "Chess Board"
				    :w width :h height
				    :flags '(:shown :opengl))))
    ;; create ogl contest
    (sdl2:gl-create-context window)
    ;; return the window
    window))

(defun draw-window (window-width window-height)
  "Function to draw the window"
  (let ((window (init-sdl2-and-gl window-width window-height)))
    (setup-frustum-projection window-width window-height 0.1 100)
    ;; Main loop
    (sdl2:with-event-loop (:method :poll)
      (:quit () t) ;; Return t to indicate a successful event handle.
      (:idle ()
        (gl:clear :color-buffer-bit) ; Clear the color buffer
        (create-graphical-board 1) ; Pass 1 as the size since the viewport handles scaling
        (sdl2:gl-swap-window window) ; Swap the front and back buffers
        (sdl2:delay 1000)))
      (sdl2:destroy-window window)
      (sdl2:quit)))

;; Open the window with size
;; (draw-window 800 600)
