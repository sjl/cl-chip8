(in-package :chip8.gui.screen)
(named-readtables:in-readtable :qtools)


;;;; Config -------------------------------------------------------------------
(defparameter *main-window* nil)
(defparameter *scale* 8)
(defparameter *width* (* *scale* 64))
(defparameter *height* (* *scale* 32))
(defparameter *fps* 60)


;;;; OpenGL -------------------------------------------------------------------
(defun initialize-texture (size)
  (let ((handle (gl:gen-texture)))
    (gl:bind-texture :texture-2d handle)

    (gl:tex-image-2d :texture-2d 0 :luminance size size 0 :luminance
                     :unsigned-byte (cffi:null-pointer))
    (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
    (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
    (gl:enable :texture-2d)

    (gl:bind-texture :texture-2d 0)

    handle))


;;;; Main Window --------------------------------------------------------------
(define-widget main-window (QMainWindow)
  ((debugger :accessor main-debugger :initarg :debugger)
   (chip :accessor main-chip :initarg :chip)))

(define-subwidget (main-window screen) (make-instance 'screen)
  (setf (screen-chip screen) chip))


(define-initializer (main-window main-setup)
  (setf (q+:window-title main-window) "cl-chip8"
        (q+:central-widget main-window) screen
        (q+:focus-proxy main-window) screen)
  (q+:show debugger))


(defun die ()
  (setf (chip8::chip-running (main-chip *main-window*)) nil)
  (q+:close (main-debugger *main-window*))
  (q+:close *main-window*))


;;;; Screen -------------------------------------------------------------------
(define-widget screen (QGLWidget)
  ((texture :accessor screen-texture)
   (chip :accessor screen-chip :initarg :chip)))


(define-initializer (screen screen-setup)
  (setf (q+:focus-policy screen) (q+:qt.strong-focus)
        (q+:fixed-size screen) (values *width* *height*)))

(define-override (screen "initializeGL") ()
  (setf (screen-texture screen) (initialize-texture 64))
  (stop-overriding))


(define-subwidget (screen timer) (q+:make-qtimer screen)
  (setf (q+:single-shot timer) NIL)
  (q+:start timer (round 1000 *fps*)))

(define-slot (screen update) ()
  (declare (connected timer (timeout)))
  (if (chip8::chip-running chip)
    (q+:repaint screen)
    (die)))


(defun render-screen (screen painter)
  (q+:begin-native-painting painter)

  (gl:clear-color 0.0 0.0 0.0 1.0)
  (gl:clear :color-buffer-bit)

  (gl:bind-texture :texture-2d (screen-texture screen))

  (let ((chip (screen-chip screen)))
    (when (chip8::chip-video-dirty chip)
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
  (when (-> screen screen-chip chip8::chip-debugger chip8::debugger-paused)
         (with-finalizing* ((font (q+:make-qfont "Menlo" 20))
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
           (q+:add-text path 10 20 font "PAUSED")
           (q+:draw-path painter path))))

(define-override (screen paint-event) (ev)
  (declare (ignore ev))
  (with-finalizing ((painter (q+:make-qpainter screen)))
    (render-screen screen painter)
    (render-debug screen painter)))


(defun pad-key-for (code)
  ;; Original Chip-8 Pad → Modern Numpad
  ;; ┌─┬─┬─┬─┐             ┌─┬─┬─┬─┐
  ;; │1│2│3│C│             │←│/│*│-│
  ;; ├─┼─┼─┼─┤             ├─┼─┼─┼─┤
  ;; │4│5│6│D│             │7│8│9│+│
  ;; ├─┼─┼─┼─┤             ├─┼─┼─┤ │
  ;; │7│8│9│E│             │4│5│6│ │
  ;; ├─┼─┼─┼─┤             ├─┼─┼─┼─┤
  ;; │A│0│B│F│             │1│2│3│↲│
  ;; └─┴─┴─┴─┘             ├─┴─┼─┤ │
  ;;                       │0  │.│ │
  ;;                       └───┴─┴─┘
  (qtenumcase code
    ((q+:qt.key_clear) #x1)
    ((q+:qt.key_slash) #x2)
    ((q+:qt.key_asterisk) #x3)
    ((q+:qt.key_minus) #xC)

    ((q+:qt.key_7) #x4)
    ((q+:qt.key_8) #x5)
    ((q+:qt.key_9) #x6)
    ((q+:qt.key_plus) #xD)

    ((q+:qt.key_4) #x7)
    ((q+:qt.key_5) #x8)
    ((q+:qt.key_6) #x9)
    ((q+:qt.key_enter) #xE)

    ((q+:qt.key_1) #xA)
    ((q+:qt.key_2) #x0)
    ((q+:qt.key_3) #xB)
    ((q+:qt.key_0) #xF)))

(defun pad-key-for (code)
  ;; Original Chip-8 Pad → Laptop
  ;; ┌─┬─┬─┬─┐             ┌─┬─┬─┬─┐
  ;; │1│2│3│C│             │1│2│3│4│
  ;; ├─┼─┼─┼─┤             ├─┼─┼─┼─┤
  ;; │4│5│6│D│             │Q│W│E│R│
  ;; ├─┼─┼─┼─┤             ├─┼─┼─┼─┤
  ;; │7│8│9│E│             │A│S│D│F│
  ;; ├─┼─┼─┼─┤             ├─┼─┼─┼─┤
  ;; │A│0│B│F│             │Z│X│C│V│
  ;; └─┴─┴─┴─┘             └─┴─┴─┴─┘
  ;;
  (qtenumcase code
    ((q+:qt.key_1) #x1)
    ((q+:qt.key_2) #x2)
    ((q+:qt.key_3) #x3)
    ((q+:qt.key_4) #xC)

    ((q+:qt.key_q) #x4)
    ((q+:qt.key_w) #x5)
    ((q+:qt.key_e) #x6)
    ((q+:qt.key_r) #xD)

    ((q+:qt.key_a) #x7)
    ((q+:qt.key_s) #x8)
    ((q+:qt.key_d) #x9)
    ((q+:qt.key_f) #xE)

    ((q+:qt.key_z) #xA)
    ((q+:qt.key_x) #x0)
    ((q+:qt.key_c) #xB)
    ((q+:qt.key_v) #xF)))


(define-override (screen key-press-event) (ev)
  (let* ((key (q+:key ev))
         (pad-key (pad-key-for key)))
    (when pad-key
      (chip8::keydown chip pad-key)))
  (stop-overriding))

(define-override (screen key-release-event) (ev)
  (let* ((key (q+:key ev))
         (pad-key (pad-key-for key))
         (debugger (chip8::chip-debugger chip)))
    (if pad-key
      (chip8::keyup chip pad-key)
      (qtenumcase key
        ((q+:qt.key_escape) (die))
        ((q+:qt.key_space) (chip8::debugger-toggle-pause debugger))
        ((q+:qt.key_f1) (chip8::reset chip))
        ((q+:qt.key_f7) (chip8::debugger-step debugger))
        (t (pr :unknown-key (format nil "~X" key))))))
  (stop-overriding))


;;;; Menus --------------------------------------------------------------------
(defun get-rom-path (window)
  (let ((path (q+:qfiledialog-get-open-file-name
                window
                "Load ROM"
                (uiop:native-namestring (asdf:system-source-directory :cl-chip8))
                "ROM Files (*.rom);;All Files (*)")))
    (if (string= path "")
      nil
      path)))

(defun load-rom (main-window)
  (when-let* ((rom (get-rom-path main-window)))
    (chip8::load-rom (main-chip main-window) rom)))

(define-menu (main-window File)
  (:item ("Load ROM..." (ctrl o))
   (load-rom main-window))
  (:item ("Quit" (ctrl q))
   (die)))


(defun set-screen-wrapping (main-window enabled)
  (setf (-<> main-window main-chip chip8::chip-screen-wrapping-enabled)
        enabled))

(define-menu (main-window Display)
  (:menu "Screen Wrapping"
   (:item "On" (set-screen-wrapping main-window t))
   (:item "Off" (set-screen-wrapping main-window nil))))


(defun set-sound-type (main-window type)
  (setf (-<> main-window main-chip chip8::chip-sound-type) type))

(define-menu (main-window Sound)
  (:menu "Sound Type"
   (:item "Sine" (set-sound-type main-window :sine))
   (:item "Square" (set-sound-type main-window :square))
   (:item "Sawtooth" (set-sound-type main-window :sawtooth))
   (:item "Triangle" (set-sound-type main-window :triangle))))


;;;; Main ---------------------------------------------------------------------
(defun make-main-window (chip)
  (make-instance 'main-window
    :chip chip
    :debugger (chip8.gui.debugger::make-debugger chip)))


(defun run-gui (chip thunk)
  (with-main-window
      (window (setf *main-window* (make-main-window chip)))
    (funcall thunk)))


