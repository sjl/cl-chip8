(in-package :chip8)

(setf *print-length* 16)
(setf *print-base* 10)
(declaim (optimize (speed 1) (safety 3) (debug 3)))
(declaim (optimize (speed 3) (safety 1) (debug 3)))


;;;; Reference ----------------------------------------------------------------
;;; http://devernay.free.fr/hacks/chip8/C8TECH10.HTM
;;; http://mattmik.com/files/chip8/mastering/chip8.html
;;; https://github.com/AfBu/haxe-chip-8-emulator/wiki/(Super)CHIP-8-Secrets


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
(deftype x-coord () `(integer 0 (,+screen-width+)))
(deftype y-coord () `(integer 0 (,+screen-height+)))

(deftype basic-array (element-type size)
  `(simple-array ,(upgraded-array-element-type element-type) (,size)))

(deftype stack (size)
  `(vector ,(upgraded-array-element-type 'int12) ,size))


;;;; Utils --------------------------------------------------------------------
(declaim
  (inline not= +_8 -_8 chop cat-bytes get-bit bcd))

(defun make-simple-array (element-type size &rest args)
  (apply #'make-array size
         :adjustable nil
         :fill-pointer nil
         :element-type element-type
         args))

(defun not= (x y)
  (not (= x y)))

(defun chop (size integer)
  (ldb (byte size 0) integer))

(defun cat-bytes (high-order low-order)
  (dpb high-order (byte 8 8) low-order))

(defun get-bit (position integer)
  (ldb (byte 1 position) integer))

(defun +_8 (x y)
  (let ((result (+ x y)))
    (values (chop 8 result)
            (if (> result 255) 1 0))))

(defun -_8 (x y)
  (let ((result (- x y)))
    (values (chop 8 result)
            (if (> x y) 1 0))))

(defun bcd (integer)
  (values (-<> integer (floor <> 100) (mod <> 10))
          (-<> integer (floor <> 10) (mod <> 10))
          (-<> integer (floor <> 1) (mod <> 10))))

(defmacro macro-map ((lambda-list items) &rest body)
  (with-gensyms (macro)
    `(macrolet ((,macro ,(ensure-list lambda-list) ,@body))
      ,@(iterate (for item :in items)
                 (collect `(,macro ,@(ensure-list item)))))))


;;;; Data ---------------------------------------------------------------------
(declaim
  (inline chip-flag (setf chip-flag)))

(defstruct debugger
  (paused nil :type boolean)
  (take-step nil :type boolean)
  (print-needed nil :type boolean)
  (callbacks-arrived nil :type list))

(defstruct chip
  (running t :type boolean)
  (memory (make-simple-array 'int8 4096)
          :type (basic-array int8 4096)
          :read-only t)
  (registers (make-simple-array 'int8 16)
             :type (basic-array int8 16)
             :read-only t)
  (keys (make-simple-array 'boolean 16)
        :type (basic-array boolean 16)
        :read-only t)
  (video (make-simple-array 'fixnum (* +screen-height+ +screen-width+))
         :type (basic-array fixnum #.(* +screen-height+ +screen-width+))
         :read-only t)
  (video-dirty t :type boolean)
  (index 0 :type int16)
  (program-counter #x200 :type int12)
  (delay-timer 0 :type int8)
  (sound-timer 0 :type int8)
  (random-state (make-random-state t)
                :type random-state
                :read-only t)
  (stack (make-array 16
           :adjustable nil
           :fill-pointer 0
           :element-type 'int12)
         :type (stack 16)
         :read-only t)
  (loaded-rom nil :type (or null string))
  (debugger (make-debugger) :type debugger :read-only t))


(define-with-macro chip
  running
  memory registers
  flag
  index program-counter
  delay-timer sound-timer
  random-state
  video video-dirty
  keys
  stack
  loaded-rom
  debugger)

(define-with-macro debugger
  paused take-step print-needed
  callbacks-arrived)


(defun chip-flag (chip)
  (aref (chip-registers chip) #xF))

(defun (setf chip-flag) (new-value chip)
  (setf (aref (chip-registers chip) #xF) new-value))


;;;; Disassembler -------------------------------------------------------------
(defun disassemble-instruction (instruction)
  (flet ((v (n) (symb 'v (format nil "~X" n))))
    (let ((_x__ (ldb (byte 4 8) instruction))
          (__x_ (ldb (byte 4 4) instruction))
          (___x (ldb (byte 4 0) instruction))
          (__xx (ldb (byte 8 0) instruction))
          (_xxx (ldb (byte 12 0) instruction)))
      (case (logand #xF000 instruction)
        (#x0000 (case instruction
                  (#x00E0 '(cls))
                  (#x00EE '(ret))))
        (#x1000 `(jp ,_xxx))
        (#x2000 `(call ,_xxx))
        (#x3000 `(se ,(v _x__) ,__xx))
        (#x4000 `(sne ,(v _x__) ,__xx))
        (#x5000 (case (logand #x000F instruction)
                  (#x0 `(se ,(v _x__) ,(v __x_)))))
        (#x6000 `(ld ,(v _x__) ,__xx))
        (#x7000 `(add ,(v _x__) ,__xx))
        (#x8000 (case (logand #x000F instruction)
                  (#x0 `(ld ,(v _x__) ,(v __x_)))
                  (#x1 `(or ,(v _x__) ,(v __x_)))
                  (#x2 `(and ,(v _x__) ,(v __x_)))
                  (#x3 `(xor ,(v _x__) ,(v __x_)))
                  (#x4 `(add ,(v _x__) ,(v __x_)))
                  (#x5 `(sub ,(v _x__) ,(v __x_)))
                  (#x6 `(shr ,(v _x__) ,(v __x_)))
                  (#x7 `(subn ,(v _x__) ,(v __x_)))
                  (#xE `(shl ,(v _x__) ,(v __x_)))))
        (#x9000 (case (logand #x000F instruction)
                  (#x0 `(sne ,(v _x__) ,(v __x_)))))
        (#xA000 `(ld i ,_xxx))
        (#xB000 `(jp ,(v 0) ,_xxx))
        (#xC000 `(rnd ,(v _x__) ,__xx))
        (#xD000 `(drw ,(v _x__) ,(v __x_) ,___x))
        (#xE000 (case (logand #x00FF instruction)
                  (#x9E `(skp ,(v _x__)))
                  (#xA1 `(sknp ,(v _x__)))))
        (#xF000 (case (logand #x00FF instruction)
                  (#x07 `(ld ,(v _x__) dt))
                  (#x0A `(ld ,(v _x__) k))
                  (#x15 `(ld dt ,(v _x__)))
                  (#x18 `(ld st ,(v _x__)))
                  (#x1E `(add i ,(v _x__)))
                  (#x29 `(ld f ,(v _x__)))
                  (#x33 `(ld b ,(v _x__)))
                  (#x55 `(ld (mem i) ,_x__))
                  (#x65 `(ld ,_x__ (mem i)))))))))

(defun bit-diagram (integer)
  (iterate (for high-bit :from 15 :downto 8)
           (for low-bit :from 7 :downto 0)
           (for hi = (logbitp high-bit integer))
           (for lo = (logbitp low-bit integer))
           (collect (cond
                      ((and hi lo) #\full_block)
                      (hi #\upper_half_block)
                      (lo #\lower_half_block)
                      (t #\space))
                    :result-type 'string)))

(defun retrieve-instruction (array index)
  (chip8::cat-bytes
    ;; ugly hacks to handle odd parity
    (if (minusp index)
      0
      (aref array index))
    (if (< (1+ index) (length array))
      (aref array (1+ index))
      0)))

(defun instruction-information (array index)
  (let ((instruction (retrieve-instruction array index)))
    (list index
          instruction
          (disassemble-instruction instruction)
          (bit-diagram instruction))))

(defun print-disassembled-instruction (array index)
  (destructuring-bind (address instruction disassembly bits)
      (instruction-information array index)
    (format t "~3,'0X: ~4,'0X ~24A ~8A~%"
            address
            instruction
            (or disassembly "")
            bits)))

(defun disassemble-instructions (array start)
  (iterate
    (for i :from start :below (length array) :by 2)
    (collect (instruction-information array i) :result-type vector)))

(defun dump-disassembly (array &optional (start 0) (end (length array)))
  (iterate
    (for i :from start :below end :by 2)
    (print-disassembled-instruction array i)
    (sleep 0.001)))


;;;; Debugger -----------------------------------------------------------------
(declaim
  (ftype (function (debugger) boolean) debugger-should-wait-p))

(defun debugger-pause (debugger)
  (with-debugger (debugger)
    (setf paused t print-needed t)))

(defun debugger-unpause (debugger)
  (with-debugger (debugger)
    (setf paused nil print-needed nil)))

(defun debugger-toggle-pause (debugger)
  (if (debugger-paused debugger)
    (debugger-unpause debugger)
    (debugger-pause debugger)))

(defun debugger-step (debugger)
  (with-debugger (debugger)
    (setf take-step t)))

(defun debugger-print (debugger chip)
  (with-debugger (debugger)
    (when (and paused print-needed)
      (let ((pc (chip-program-counter chip)))
        (setf print-needed nil)
        (destructuring-bind (address instruction disassembly bits)
            (instruction-information (chip-memory chip) pc)
          (format t "~3,'0X: ~4,'0X ~24A ~8A~%"
                  address
                  instruction
                  (or disassembly "")
                  bits))
        (mapc (rcurry #'funcall pc) callbacks-arrived))))
  (values))

(defun debugger-paused-p (debugger)
  (debugger-paused debugger))

(defun debugger-should-wait-p (debugger)
  (with-debugger (debugger)
    (if (not paused) ; if we're not paused, we never need to wait
      nil
      (if take-step
        (progn (setf take-step nil ; if we're paused, but are ready to step, go
                     print-needed t)
               nil)
        t)))) ; otherwise we're fully paused -- wait

(defun debugger-add-callback-arrived (debugger function)
  (push function (debugger-callbacks-arrived debugger))
  t)


;;;; Graphics -----------------------------------------------------------------
(declaim
  (inline font-location vref (setf vref))
  (ftype (function (chip int8 int8 int4) null) draw-sprite))


(defun vref (chip x y)
  (aref (chip-video chip) (+ (* +screen-width+ y) x)))

(defun (setf vref) (new-value chip x y)
  (setf (aref (chip-video chip) (+ (* +screen-width+ y) x))
        new-value))


(defun load-font (chip)
  ;; Thanks http://www.multigesture.net/articles/how-to-write-an-emulator-chip-8-interpreter/
  (replace (chip-memory chip)
           (vector #xF0 #x90 #x90 #x90 #xF0  ; 0
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

(defun font-location (character)
  (+ #x50 (* character 5)))


(defun draw-sprite (chip start-x start-y size)
  (with-chip (chip)
    (assert (< (+ index size) +memory-size+) (index)
      "Sprite data of size ~D starting at #x~4,'0X would be out of bounds"
      size index)
    (setf flag 0)
    (iterate
      (repeat size)
      (for i :from index)
      (for sprite = (aref memory i))
      (for y :modulo +screen-height+ :from start-y)
      (iterate (for x :modulo +screen-width+ :from start-x)
               (for col :from 7 :downto 0)
               (for old-pixel = (plusp (vref chip x y)))
               (for new-pixel = (plusp (get-bit col sprite)))
               (when (and old-pixel new-pixel)
                 (setf flag 1))
               (setf (vref chip x y)
                     (if (xor old-pixel new-pixel) 255 0))))
    (setf video-dirty t))
  nil)


;;;; Keyboard -----------------------------------------------------------------
(declaim
  (ftype (function (chip (integer 0 (16)))) keydown keyup))

(defun keydown (chip key)
  (setf (aref (chip-keys chip) key) t))

(defun keyup (chip key)
  (setf (aref (chip-keys chip) key) nil))


;;;; Opcodes ------------------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-opcode-argument-bindings (argument-list)
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
                                  opcode))))))))

(defmacro define-opcode (name argument-list &body body)
  `(progn
    (declaim (ftype (function (chip int16)
                              (values null &optional))
                    ,name))
    (defun ,name (chip opcode)
      (declare (ignorable opcode))
      (with-chip (chip)
        (macrolet ((register (index)
                     `(aref registers ,index)))
          (let ,(parse-opcode-argument-bindings argument-list)
            ,@body))
        nil))))


(macro-map                                              ;; LD ...
    ((name arglist destination source)
     ((op-ld-i<imm   (_ (value 3))   index         value)
      (op-ld-reg<imm (_ r (value 2)) (register r)  value)
      (op-ld-reg<reg (_ rx ry _)     (register rx) (register ry))
      (op-ld-reg<dt  (_ r _ _)       (register r)  delay-timer)
      (op-ld-dt<reg  (_ r _ _)       delay-timer   (register r))
      (op-ld-st<reg  (_ r _ _)       sound-timer   (register r))))
  `(define-opcode ,name ,arglist
    (setf ,destination ,source)))

(define-opcode op-cls ()                                ;; CLS
  (fill video 0)
  (setf video-dirty t))

(define-opcode op-jp-imm (_ (target 3))                 ;; JP addr
  (setf program-counter target))

(define-opcode op-jp-imm+reg (_ (target 3))             ;; JP V0 + addr
  (setf program-counter (+ target (register 0))))

(define-opcode op-call (_ (target 3))                   ;; CALL addr
  (vector-push program-counter stack)
  (setf program-counter target))

(define-opcode op-ret ()                                ;; RET
  (setf program-counter (vector-pop stack)))

(macro-map                                              ;; ADD/SUB (8-bit)
  ((name op source-arg source-expr)
   ((op-add-reg<imm +_8 (immediate 2) immediate)
    (op-add-reg<reg +_8 (ry 1) (register ry))
    (op-sub-reg<reg -_8 (ry 1) (register ry))))
  `(define-opcode ,name (_ rx ,source-arg)
    (multiple-value-bind (result carry)
        (,op (register rx) ,source-expr)
      (setf (register rx) result
            flag carry))))

(define-opcode op-add-index<reg (_ r)                   ;; ADD I, Vx (16-bit)
  (zapf index (chop 16 (+ % (register r)))))

(define-opcode op-subn-reg<reg (_ rx ry)                ;; SUBN
  (multiple-value-bind (result carry)
      (-_8 (register ry) (register rx)) ; subtraction order is swapped for SUBN
    (setf (register rx) result
          flag carry)))

(macro-map                                              ;; SE/SNE
  ((name test x-arg x-form y-arg y-form)
   ((op-se-reg-imm  =    (r 1)  (register r)  (immediate 2) immediate)
    (op-sne-reg-imm not= (r 1)  (register r)  (immediate 2) immediate)
    (op-se-reg-reg  =    (rx 1) (register rx) (ry 1)        (register ry))
    (op-sne-reg-reg not= (rx 1) (register rx) (ry 1)        (register ry))))
  `(define-opcode ,name (_ ,x-arg ,y-arg)
    (when (,test ,x-form ,y-form)
      (incf program-counter 2))))

(macro-map                                              ;; AND/OR/XOR
    ((name function)
     ((op-or logior)
      (op-and logand)
      (op-xor logxor)))
  `(define-opcode ,name (_ destination source _)
    (zapf (register destination)
          (,function % (register source)))))

(define-opcode op-rand (_ r (mask 2))                   ;; RND
  (setf (register r)
        (logand (random 256 random-state) mask)))

(define-opcode op-skp (_ r _ _)                         ;; SKP
  (when (aref keys (register r))
    (incf program-counter 2)))

(define-opcode op-sknp (_ r _ _)                        ;; SKNP
  (when (not (aref keys (register r)))
    (incf program-counter 2)))

(define-opcode op-ld-mem<regs (_ n _ _)                 ;; LD [I] < Vn
  (replace memory registers :start1 index :end2 (1+ n)))

(define-opcode op-ld-regs<mem (_ n _ _)                 ;; LD Vn < [I]
  (replace registers memory :end1 (1+ n) :start2 index))

(define-opcode op-ld-reg<key (_ r _ _)                  ;; LD Vx, Key (await)
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

(define-opcode op-shr (_ r _ _)                         ;; SHR
  (let ((value (register r)))
    (setf flag (get-bit 0 value)
          (register r) (ash value -1))))

(define-opcode op-shl (_ r _ _)                         ;; SHL
  (let ((value (register r)))
    (setf flag (get-bit 7 value)
          (register r) (chop 8 (ash value 1)))))

(define-opcode op-ld-font<vx (_ r _ _)                  ;; LD F, Vx
  (setf index (font-location (register r))))

(define-opcode op-ld-bcd<vx (_ r _ _)                   ;; LD B, Vx
  (multiple-value-bind (hundreds tens ones)
      (bcd (register r))
    (setf (aref memory (+ index 0)) hundreds
          (aref memory (+ index 1)) tens
          (aref memory (+ index 2)) ones)))

(define-opcode op-draw (_ rx ry size)                   ;; DRW Vx, Vy, size
  (draw-sprite chip (register rx) (register ry) size))


;;;; Sound --------------------------------------------------------------------
(defconstant +pi+ (float pi 1.0))
(defconstant +tau+ (* 2 +pi+))
(defconstant +sample-rate+ 44100d0)
(defconstant +audio-buffer-size+ 512)
(defconstant +audio-buffer-time+ (* +audio-buffer-size+ (/ +sample-rate+)))

(defun square (angle)
  (if (< (mod angle +tau+) +pi+)
    1.0
    -1.0))

(defun saw (angle)
  (let ((a (mod angle +tau+)))
    (if (< a +pi+)
      (map-range 0   +pi+
                 1.0 -1.0
                 a)
      (map-range +pi+ +tau+
                 -1.0 1.0
                 a))))


(defun make-audio-buffer ()
  (make-simple-array 'single-float +audio-buffer-size+ :initial-element 0.0))

(defun fill-buffer (buffer function rate start)
  (iterate
    (for i :index-of-vector buffer)
    (for angle :from start :by rate)
    (setf (aref buffer i) (funcall function angle))
    (finally (return (mod angle +tau+)))))

(defun fill-square (buffer rate start)
  (fill-buffer buffer #'square rate start))

(defun fill-sine (buffer rate start)
  (fill-buffer buffer #'sin rate start))

(defun fill-sawtooth (buffer rate start)
  (fill-buffer buffer #'saw rate start))


(defun audio-rate (frequency)
  (float (* (/ +tau+ +sample-rate+) frequency) 1.0))


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
                   (sleep +audio-buffer-time+)))))))


;;;; Timers -------------------------------------------------------------------
(declaim
  (ftype (function (chip) null) decrement-timers run-timers))

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
(declaim
  (ftype (function (chip) null) run-cpu emulate-cycle)
  (ftype (function (chip int16) null) dispatch-instruction))

(defparameter *c* nil)


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
    (if (debugger-should-wait-p debugger)
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
(defun run (rom-filename &key start-paused)
  (let ((chip (make-chip)))
    (setf *c* chip)
    (load-rom chip rom-filename)
    (when start-paused
      (debugger-pause (chip-debugger chip)))
    (chip8.gui::run-gui
      chip
      (lambda ()
        ;; Really it's just the sound that needs to be here...
        (bt:make-thread (curry #'run-cpu chip))
        (bt:make-thread (curry #'run-timers chip))
        (bt:make-thread (curry #'run-sound chip))))))
