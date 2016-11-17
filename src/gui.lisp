(in-package :chip8.gui)
(named-readtables:in-readtable :qtools)


;;;; Config -------------------------------------------------------------------
(defparameter *current* nil)
(defparameter *scale* 6)
(defparameter *width* (* *scale* 64))
(defparameter *height* (* *scale* 32))
(defparameter *fps* 60)



;;;; Data ---------------------------------------------------------------------
(defstruct gui chip screen)


;;;; OpenGL -------------------------------------------------------------------
(defun initialize-texture (size)
  (let* ((handle (gl:gen-texture)))
    (gl:bind-texture :texture-2d handle)

    (gl:tex-image-2d :texture-2d 0 :luminance size size 0 :luminance
                     :unsigned-byte (cffi:null-pointer))
    (gl:tex-parameter :texture-2d :texture-min-filter :nearest) ; sharp pixels or gtfo
    (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
    (gl:enable :texture-2d)

    (gl:bind-texture :texture-2d 0)

    handle))


;;;; Screen -------------------------------------------------------------------
(define-widget screen (QGLWidget)
  ((texture :accessor screen-texture)
   (chip :accessor screen-chip :initarg :chip)))

(defun die (screen)
  (setf chip8::*running* nil)
  (q+:close screen))

(define-initializer (screen setup)
  (setf (q+:window-title screen) "cl-chip8"
        (q+:fixed-size screen) (values *width* *height*)))

(define-override (screen "initializeGL") ()
  (setf (screen-texture screen) (initialize-texture 64))
  (stop-overriding))

(define-subwidget (screen timer) (q+:make-qtimer screen)
  (setf (q+:single-shot timer) NIL)
  (q+:start timer (round 1000 *fps*)))

(define-slot (screen update) ()
  (declare (connected timer (timeout)))

  (if chip8::*running*
    (q+:repaint screen)
    (die screen)))


(defun render-screen (screen painter)
  (q+:begin-native-painting painter)

  (gl:clear-color 0.0 0.0 0.0 1.0)
  (gl:clear :color-buffer-bit)

  (gl:bind-texture :texture-2d (screen-texture screen))

  (let ((chip (screen-chip screen)))
    (when t ; (chip8::chip-video-dirty chip)
      (setf (chip8::chip-video-dirty chip) nil)
      (gl:tex-sub-image-2d :texture-2d 0 0 0 64 32 :luminance :unsigned-byte
                           (chip8::chip-video chip))))

  (let ((tw 1)
        (th 0.5))
    (gl:with-primitives :quads
      (gl:tex-coord 0 0)
      (gl:vertex 0 0)

      (gl:tex-coord tw 0)
      (gl:vertex *width* 0)

      (gl:tex-coord tw th)
      (gl:vertex *width* *height*)

      (gl:tex-coord 0 th)
      (gl:vertex 0 *height*)))

  (gl:bind-texture :texture-2d 0)

  (q+:end-native-painting painter))

(defun render-debug (screen painter)
  (declare (ignore screen))
  (when chip8::*paused*
    (with-finalizing* ((font (q+:make-qfont "Menlo" 40))
                       (border-color (q+:make-qcolor 255 255 255))
                       (fill-color (q+:make-qcolor 0 0 0))
                       (path (q+:make-qpainterpath))
                       (pen (q+:make-qpen))
                       (brush (q+:make-qbrush fill-color)))
      (setf (q+:width pen) 1)
      (setf (q+:color pen) border-color)

      (setf (q+:pen painter) pen)
      (setf (q+:brush painter) brush)
      (setf (q+:font painter) font)
      (setf (q+:weight font) (q+:qfont.black))
      (setf (q+:style-hint font) (q+:qfont.type-writer))

      ; (setf (q+:pen painter) (q+:make-qcolor "#ff0000"))
      (q+:add-text path 10 40 font "PAUSED")
      (q+:draw-path painter path))))

(define-override (screen paint-event) (ev)
  (declare (ignore ev))
  (with-finalizing ((painter (q+:make-qpainter screen)))
    (render-screen screen painter)
    (render-debug screen painter)))


(defun pad-key-for (code)
  (cond
    ((= code (q+:qt.key_6)) #x1)
    ((= code (q+:qt.key_7)) #x2)
    ((= code (q+:qt.key_8)) #x3)
    ((= code (q+:qt.key_9)) #xC)

    ((= code (q+:qt.key_y)) #x4)
    ((= code (q+:qt.key_u)) #x5)
    ((= code (q+:qt.key_i)) #x6)
    ((= code (q+:qt.key_o)) #xD)

    ((= code (q+:qt.key_h)) #x7)
    ((= code (q+:qt.key_j)) #x8)
    ((= code (q+:qt.key_k)) #x9)
    ((= code (q+:qt.key_l)) #xE)

    ((= code (q+:qt.key_n)) #xA)
    ((= code (q+:qt.key_m)) #x0)
    ((= code (q+:qt.key_comma)) #xB)
    ((= code (q+:qt.key_period)) #xF)))


(define-override (screen key-press-event) (ev)
  (let* ((key (q+:key ev))
         (pad-key (pad-key-for key)))
    (when pad-key
      (chip8::keydown chip pad-key)))
  (stop-overriding))

(define-override (screen key-release-event) (ev)
  (let* ((key (q+:key ev))
         (pad-key (pad-key-for key)))
    (if pad-key
      (when pad-key
        (chip8::keyup chip pad-key))
      (cond ((= key (q+:qt.key_escape))
             (die screen))

            ((= key (q+:qt.key_space))
             (zapf chip8::*paused* (not %))))))
  (stop-overriding))


;;;; Main ---------------------------------------------------------------------
(defun run-gui (chip)
  (with-main-window
    (window (make-instance 'screen :chip chip))))


