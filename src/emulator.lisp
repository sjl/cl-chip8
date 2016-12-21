(in-package :chip8)
(declaim (optimize (speed 3) (safety 0) (debug 0)))
; (declaim (optimize (speed 3) (safety 1) (debug 2)))


;;;; Constants ----------------------------------------------------------------
(defconstant +cycles-per-second+ 500)
(defconstant +cycles-before-sleep+ 10)
(defconstant +screen-width+ 64)
(defconstant +screen-height+ 32)
(defconstant +memory-size+ (* 1024 4))


;;;; Types --------------------------------------------------------------------
(deftype int4 () '(unsigned-byte 4))
(deftype int8 () '(unsigned-byte 8))
(deftype int12 () '(unsigned-byte 12))
(deftype int16 () '(unsigned-byte 16))


;;;; Utils --------------------------------------------------------------------
(defun-inline not= (x y)
  (not (= x y)))

(defun-inline chop (size integer)
  (ldb (byte size 0) integer))

(defun-inline cat-bytes (high-order low-order)
  (dpb high-order (byte 8 8) low-order))

(defun-inline get-bit (position integer)
  (ldb (byte 1 position) integer))

(defun-inline +_8 (x y)
  (let ((result (+ x y)))
    (values (chop 8 result)
            (if (> result 255) 1 0))))

(defun-inline -_8 (x y)
  (let ((result (- x y)))
    (values (chop 8 result)
            (if (> x y) 1 0))))

(defun-inline >>_8 (v)
  (values (ash v -1)
          (get-bit 0 v)))

(defun-inline <<_8 (v)
  (values (chop 8 (ash v 1))
          (get-bit 7 v)))

(defmacro macro-map (lambda-list items &rest body)
  (with-gensyms (macro)
    `(macrolet ((,macro ,(ensure-list lambda-list) ,@body))
      ,@(iterate (for item :in items)
                 (collect `(,macro ,@(ensure-list item)))))))


;;;; Data ---------------------------------------------------------------------
(defstruct chip
  (running t :type boolean)
  (memory (make-array +memory-size+ :element-type 'int8)
          :type (simple-array int8 (#.+memory-size+))
          :read-only t)
  (registers (make-array 16 :element-type 'int8)
             :type (simple-array int8 (16))
             :read-only t)
  (index 0 :type int16)
  (program-counter #x200 :type int12)
  (keys (make-array 16 :element-type 'boolean :initial-element nil)
        :type (simple-array boolean (16))
        :read-only t)
  (video (make-array (* +screen-height+ +screen-width+) :element-type 'fixnum)
         :type (simple-array fixnum (#.(* +screen-height+ +screen-width+)))
         :read-only t)
  (video-dirty t :type boolean)
  (screen-wrapping-enabled t :type boolean)
  (delay-timer 0 :type fixnum)
  (sound-timer 0 :type fixnum)
  (stack (make-array 16 :element-type 'int12 :fill-pointer 0)
         :type (vector int12 16)
         :read-only t)
  (loaded-rom nil :type (or null string))
  (debugger (make-debugger) :type debugger :read-only t))

(define-with-macro chip
  running
  memory
  registers flag index program-counter
  delay-timer sound-timer
  video video-dirty screen-wrapping-enabled
  keys
  stack
  loaded-rom
  debugger)


(defun-inline chip-flag (chip)
  (aref (chip-registers chip) #xF))

(defun-inline (setf chip-flag) (new-value chip)
  (setf (aref (chip-registers chip) #xF) new-value))


;;;; Graphics -----------------------------------------------------------------
(defun-inline vref (chip x y)
  (aref (chip-video chip) (+ (* +screen-width+ y) x)))

(defun-inline (setf vref) (new-value chip x y)
  (setf (aref (chip-video chip) (+ (* +screen-width+ y) x))
        new-value))


(defun load-font (chip)
  ;; Thanks http://www.multigesture.net/articles/how-to-write-an-emulator-chip-8-interpreter/
  (replace (chip-memory chip)
           #(#xF0 #x90 #x90 #x90 #xF0  ; 0
             #x20 #x60 #x20 #x20 #x70  ; 1
             #xF0 #x10 #xF0 #x80 #xF0  ; 2
             #xF0 #x10 #xF0 #x10 #xF0  ; 3
             #x90 #x90 #xF0 #x10 #x10  ; 4
             #xF0 #x80 #xF0 #x10 #xF0  ; 5
             #xF0 #x80 #xF0 #x90 #xF0  ; 6
             #xF0 #x10 #x20 #x40 #x40  ; 7
             #xF0 #x90 #xF0 #x90 #xF0  ; 8
             #xF0 #x90 #xF0 #x10 #xF0  ; 9
             #xF0 #x90 #xF0 #x90 #x90  ; A
             #xE0 #x90 #xE0 #x90 #xE0  ; B
             #xF0 #x80 #x80 #x80 #xF0  ; C
             #xE0 #x90 #x90 #x90 #xE0  ; D
             #xF0 #x80 #xF0 #x80 #xF0  ; E
             #xF0 #x80 #xF0 #x80 #x80) ; F
           :start1 #x50))

(defun-inline font-location (character)
  (+ #x50 (* character 5)))


(defun-inline wrap (chip x y)
  (cond
    ((chip-screen-wrapping-enabled chip)
     (values (mod x +screen-width+)
             (mod y +screen-height+)
             t))
    ((and (in-range-p 0 x +screen-width+)
          (in-range-p 0 y +screen-height+))
     (values x y t))
    (t (values nil nil nil))))

(defun draw-sprite (chip start-x start-y size)
  (with-chip (chip)
    (setf flag 0)
    (iterate (repeat size)
             (for i :from index)
             (for y :from start-y)
             (for sprite = (aref memory i))
             (iterate
               (for x :from start-x)
               (for col :from 7 :downto 0)
               (multiple-value-bind (x y draw) (wrap chip x y)
                 (when draw
                   (for old-pixel = (plusp (vref chip x y)))
                   (for new-pixel = (plusp (get-bit col sprite)))
                   (when (and old-pixel new-pixel)
                     (setf flag 1))
                   (setf (vref chip x y)
                         (if (xor old-pixel new-pixel) 255 0))))))
    (setf video-dirty t))
  nil)


;;;; Keyboard -----------------------------------------------------------------
(defun keydown (chip key)
  (setf (aref (chip-keys chip) key) t))

(defun keyup (chip key)
  (setf (aref (chip-keys chip) key) nil))


;;;; Instructions -------------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-instruction-argument-bindings (argument-list)
    (flet ((normalize-arg (arg)
             (destructuring-bind (symbol &optional (nibbles 1))
                 (ensure-list arg)
               (list symbol nibbles))))
      (iterate
        (for (symbol nibbles) :in (mapcar #'normalize-arg argument-list))
        (for position :first 3 :then (- position nibbles))
        (when (not (eql symbol '_))
          (collect `(,symbol (ldb (byte ,(* nibbles 4)
                                        ,(* position 4))
                                  instruction))))))))

(defmacro define-instruction (name argument-list &body body)
  `(progn
    (declaim (ftype (function (chip int16)
                              (values null &optional))
                    ,name))
    (defun ,name (chip instruction)
      (declare (ignorable instruction))
      (with-chip (chip)
        (macrolet ((register (index)
                     `(aref registers ,index)))
          (let ,(parse-instruction-argument-bindings argument-list)
            ,@body))
        nil))))


(macro-map                                              ;; LD
    (NAME           ARGLIST         DESTINATION   SOURCE)
    ((op-ld-i<imm   (_ (value 3))   index         value)
     (op-ld-reg<imm (_ r (value 2)) (register r)  value)
     (op-ld-reg<reg (_ rx ry _)     (register rx) (register ry))
     (op-ld-reg<dt  (_ r _ _)       (register r)  delay-timer)
     (op-ld-dt<reg  (_ r _ _)       delay-timer   (register r))
     (op-ld-st<reg  (_ r _ _)       sound-timer   (register r)))
  `(define-instruction ,name ,arglist
    (setf ,destination ,source)))

(define-instruction op-cls ()                           ;; CLS
  (fill video 0)
  (setf video-dirty t))

(define-instruction op-jp-imm (_ (target 3))            ;; JP addr
  (setf program-counter target))

(define-instruction op-jp-imm+reg (_ (target 3))        ;; JP V0 + addr
  (setf program-counter (chop 12 (+ target (register 0)))))

(define-instruction op-call (_ (target 3))              ;; CALL addr
  (vector-push program-counter stack)
  (setf program-counter target))

(define-instruction op-ret ()                           ;; RET
  (setf program-counter (vector-pop stack)))

(define-instruction op-add-reg<imm (_ r (immediate 2))  ;; ADD Vx, Imm
  ;; For some weird reason the ADD immediate op doesn't set the flag
  (zapf (register r) (+_8 % immediate)))

(define-instruction op-add-reg<reg (_ rx ry)            ;; ADD Vx, Vy (8-bit)
    (setf (values (register rx) flag)
          (+_8 (register rx) (register ry))))

(define-instruction op-sub-reg<reg (_ rx ry)            ;; SUB Vx, Vy (8-bit)
  (setf (values (register rx) flag)
        (-_8 (register rx) (register ry))))

(define-instruction op-add-index<reg (_ r)              ;; ADD I, Vx (16-bit)
  (zapf index (chop 16 (+ % (register r)))))

(define-instruction op-subn-reg<reg (_ rx ry)           ;; SUBN Vx, Vy (8-bit)
  (setf (values (register rx) flag)
        ;; subtraction order is swapped for SUBN
        (-_8 (register ry) (register rx))))

(macro-map                                              ;; SE/SNE
  (NAME            TEST X-ARG  X-FORM        Y-ARG         Y-FORM)
  ((op-se-reg-imm  =    (r 1)  (register r)  (immediate 2) immediate)
   (op-sne-reg-imm not= (r 1)  (register r)  (immediate 2) immediate)
   (op-se-reg-reg  =    (rx 1) (register rx) (ry 1)        (register ry))
   (op-sne-reg-reg not= (rx 1) (register rx) (ry 1)        (register ry)))
  `(define-instruction ,name (_ ,x-arg ,y-arg)
    (when (,test ,x-form ,y-form)
      (incf program-counter 2))))

(macro-map                                              ;; AND/OR/XOR
  (NAME   OP)
  ((op-and logand)
   (op-or  logior)
   (op-xor logxor))
  `(define-instruction ,name (_ destination source _)
    (zapf (register destination) (,op % (register source)))))

(define-instruction op-rand (_ r (mask 2))              ;; RND
  (setf (register r)
        (logand (random 256) mask)))

(define-instruction op-skp (_ r _ _)                    ;; SKP
  (when (aref keys (register r))
    (incf program-counter 2)))

(define-instruction op-sknp (_ r _ _)                   ;; SKNP
  (when (not (aref keys (register r)))
    (incf program-counter 2)))

(define-instruction op-ld-mem<regs (_ n _ _)            ;; LD [I] < Vn
  (replace memory registers :start1 index :end2 (1+ n)))

(define-instruction op-ld-regs<mem (_ n _ _)            ;; LD Vn < [I]
  (replace registers memory :end1 (1+ n) :start2 index))

(define-instruction op-ld-reg<key (_ r _ _)             ;; LD Vx, Key (await)
  ;; I'm unsure how this instruction is supposed to interact with the timers.
  ;;
  ;; Either the timers should continue to count down while we wait for a key, or
  ;; they should pause while waiting, but I can't find anything in the docs that
  ;; spells it out.
  ;;
  ;; This implementation chooses the former (timers keep running) for now.
  (let ((key (position t keys)))
    (if key
      (setf (register r) key)
      ;; If we don't have a key, just execute this instruction again next time.
      (decf program-counter 2))))

(define-instruction op-shr (_ r _ _)                    ;; SHR
  (setf (values (register r) flag)
        (>>_8 (register r))))

(define-instruction op-shl (_ r _ _)                    ;; SHL
  (setf (values (register r) flag)
        (<<_8 (register r))))

(define-instruction op-ld-font<vx (_ r _ _)             ;; LD F, Vx
  (setf index (font-location (register r))))

(define-instruction op-ld-bcd<vx (_ r _ _)              ;; LD B, Vx
  (let ((number (register r)))
    (setf (aref memory (+ index 0)) (digit 2 number)
          (aref memory (+ index 1)) (digit 1 number)
          (aref memory (+ index 2)) (digit 0 number))))

(define-instruction op-draw (_ rx ry size)              ;; DRW Vx, Vy, size
  (draw-sprite chip (register rx) (register ry) size))


;;;; Sound --------------------------------------------------------------------
(defconstant +pi+ (coerce pi 'single-float))
(defconstant +tau+ (* 2 +pi+))
(defconstant +1/4tau+ (* 1/4 tau))
(defconstant +1/2tau+ (* 1/2 tau))
(defconstant +3/4tau+ (* 3/4 tau))

(defconstant +sample-rate+ 44100d0)

(defconstant +audio-buffer-size+ 512
  "The number of samples in the audio buffer.")

(defconstant +audio-buffer-time+ (* +audio-buffer-size+ (/ +sample-rate+))
  "The total time the information in the audio buffer represents, in seconds.")


(defun sqr (angle)
  (if (< (mod angle +tau+) +1/2tau+)
    1.0
    -1.0))

(defun saw (angle)
  (let ((a (mod angle +tau+)))
    (if (< a +1/2tau+)
      (map-range 0   +1/2tau+
                 0.0 1.0
                 a)
      (map-range +1/2tau+ +tau+
                 -1.0     0.0
                 a))))

(defun tri (angle)
  (let ((a (mod angle +tau+)))
    (cond ((< a +1/4tau+) (map-range 0   +1/4tau+
                                     0.0 1.0
                                     a))
          ((< a 3/4tau) (map-range +1/4tau+ +3/4tau+
                                   1.0      -1.0
                                   a))
          (t (map-range +3/4tau+ +tau+
                        -1.0     0.0
                        a)))))


(defun make-audio-buffer ()
  (make-array +audio-buffer-size+
    :element-type 'single-float
    :initial-element 0.0))


(defun fill-buffer (buffer function rate start)
  (iterate
    (for i :index-of-vector buffer)
    (for angle :from start :by rate)
    (setf (aref buffer i) (funcall function angle))
    (finally (return (mod angle +tau+)))))

(defun fill-square (buffer rate start)
  (fill-buffer buffer #'sqr rate start))

(defun fill-sine (buffer rate start)
  (fill-buffer buffer #'sin rate start))

(defun fill-sawtooth (buffer rate start)
  (fill-buffer buffer #'saw rate start))

(defun fill-triangle (buffer rate start)
  (fill-buffer buffer #'tri rate start))


(defun audio-rate (frequency)
  (coerce (* (/ +tau+ +sample-rate+) frequency) 'single-float))


(defun run-sound (chip)
  (portaudio:with-audio
    (portaudio:with-default-audio-stream
        (audio-stream 0 1
                      :sample-format :float
                      :sample-rate +sample-rate+
                      :frames-per-buffer +audio-buffer-size+)
      (with-chip (chip)
        (iterate (with buffer = (make-audio-buffer))
                 (with angle = 0.0)
                 (with rate = (audio-rate 440))
                 (while running)
                 (if (and (plusp sound-timer)
                          (not (debugger-paused-p debugger)))
                   (progn
                     (setf angle (fill-sawtooth buffer rate angle))
                     (portaudio:write-stream audio-stream buffer))
                   (sleep +audio-buffer-time+))))))
  nil)


;;;; Timers -------------------------------------------------------------------
(defun decrement-timers (chip)
  (flet ((decrement (i)
           (if (plusp i)
             (1- i)
             0)))
    (with-chip (chip)
      (sb-ext:atomic-update delay-timer #'decrement)
      (sb-ext:atomic-update sound-timer #'decrement)))
  nil)

(defun run-timers (chip)
  (with-chip (chip)
    (iterate
      (while running)
      (when (not (debugger-paused-p debugger))
        (decrement-timers chip))
      (sleep 1/60))))


;;;; CPU ----------------------------------------------------------------------
(defun reset (chip)
  (with-chip (chip)
    (fill memory 0)
    (fill registers 0)
    (fill keys nil)
    (fill video 0)
    (load-font chip)
    (replace memory (read-file-into-byte-vector loaded-rom)
             :start1 #x200)
    (setf running t
          video-dirty t
          program-counter #x200
          delay-timer 0
          sound-timer 0
          (fill-pointer stack) 0))
  (values))

(defun load-rom (chip filename)
  (setf (chip-loaded-rom chip) filename)
  (reset chip))


(defun dispatch-instruction (chip instruction)
  (macrolet ((call (name) `(,name chip instruction)))
    (ecase (logand #xF000 instruction)
      (#x0000 (ecase instruction
                (#x00E0 (call op-cls))
                (#x00EE (call op-ret))))
      (#x1000 (call op-jp-imm))
      (#x2000 (call op-call))
      (#x3000 (call op-se-reg-imm))
      (#x4000 (call op-sne-reg-imm))
      (#x5000 (ecase (logand #x000F instruction)
                (#x0 (call op-se-reg-reg))))
      (#x6000 (call op-ld-reg<imm))
      (#x7000 (call op-add-reg<imm))
      (#x8000 (ecase (logand #x000F instruction)
                (#x0 (call op-ld-reg<reg))
                (#x1 (call op-or))
                (#x2 (call op-and))
                (#x3 (call op-xor))
                (#x4 (call op-add-reg<reg))
                (#x5 (call op-sub-reg<reg))
                (#x6 (call op-shr))
                (#x7 (call op-subn-reg<reg))
                (#xE (call op-shl))))
      (#x9000 (ecase (logand #x000F instruction)
                (#x0 (call op-sne-reg-reg))))
      (#xA000 (call op-ld-i<imm))
      (#xB000 (call op-jp-imm+reg))
      (#xC000 (call op-rand))
      (#xD000 (call op-draw))
      (#xE000 (ecase (logand #x00FF instruction)
                (#x9E (call op-skp))
                (#xA1 (call op-sknp))))
      (#xF000 (ecase (logand #x00FF instruction)
                (#x07 (call op-ld-reg<dt))
                (#x0A (call op-ld-reg<key))
                (#x15 (call op-ld-dt<reg))
                (#x18 (call op-ld-st<reg))
                (#x1E (call op-add-index<reg))
                (#x29 (call op-ld-font<vx))
                (#x33 (call op-ld-bcd<vx))
                (#x55 (call op-ld-mem<regs))
                (#x65 (call op-ld-regs<mem)))))))

(defun emulate-cycle (chip)
  (with-chip (chip)
    (debugger-print debugger chip)
    (if (debugger-should-wait-p debugger program-counter)
      (sleep 10/1000)
      (let ((instruction (cat-bytes (aref memory program-counter)
                                    (aref memory (1+ program-counter)))))
        (zapf program-counter (chop 12 (+ % 2)))
        (dispatch-instruction chip instruction)))
    nil))

(defun run-cpu (chip)
  (iterate
    (while (chip-running chip))
    (emulate-cycle chip)
    (for tick :every-nth +cycles-before-sleep+ :do
         (sleep (/ +cycles-before-sleep+ +cycles-per-second+)))))


;;;; Main ---------------------------------------------------------------------
(defparameter *c* nil)

(defun run (rom-filename &key start-paused)
  (let ((chip (make-chip)))
    (setf *c* chip)
    (load-rom chip rom-filename)
    (when start-paused
      (debugger-pause (chip-debugger chip)))
    (chip8.gui.screen::run-gui
      chip
      (lambda ()
        ;; Really it's just the sound that needs to be here...
        (bt:make-thread (curry #'run-cpu chip))
        (bt:make-thread (curry #'run-timers chip))
        (bt:make-thread (curry #'run-sound chip))))))

