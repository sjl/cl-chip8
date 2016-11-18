(in-package :chip8)

(setf *print-length* 16)
(setf *print-base* 10)
(declaim (optimize (speed 1) (safety 3) (debug 3)))
(declaim (optimize (speed 3) (safety 1) (debug 3)))


;;;; Constants ----------------------------------------------------------------
(defconstant +screen-width+ 64)
(defconstant +screen-height+ 32)
(defconstant +memory-size+ (* 1024 4))
(defconstant +timer-tick+ (round (* 1/60 internal-time-units-per-second)))


;;;; Types --------------------------------------------------------------------
(deftype int4 () '(unsigned-byte 4))
(deftype int8 () '(unsigned-byte 8))
(deftype int12 () '(unsigned-byte 12))
(deftype int16 () '(unsigned-byte 16))
(deftype x-coord () `(integer 0 (,+screen-width+)))
(deftype y-coord () `(integer 0 (,+screen-height+)))
(deftype memory-index () `(integer 0 (,+memory-size+)))

(deftype basic-array (element-type size)
  `(simple-array ,(upgraded-array-element-type element-type) (,size)))

(deftype stack (size)
  `(vector ,(upgraded-array-element-type 'int12) ,size))


;;;; Utils --------------------------------------------------------------------
(declaim (inline not= +_8 -_8 chop cat-bytes get-bit bcd))

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
(defstruct chip
  (memory (make-simple-array 'int8 4096)
          :type (basic-array int8 4096)
          :read-only t)
  (registers (make-simple-array 'int8 16)
             :type (basic-array int8 16)
             :read-only t)
  (keys (make-simple-array 'boolean 16)
        :type (basic-array boolean 16)
        :read-only t)
  (awaiting-key nil
                :type (or null (integer 0 15)))
  (video (make-simple-array 'fixnum (* 32 64))
         :type (basic-array fixnum #.(* 32 64))
         :read-only t)
  (video-dirty t :type boolean)
  (index 0 :type int16)
  (program-counter #x200 :type int12)
  (delay-timer 0 :type int8)
  (sound-timer 0 :type int8)
  (timer-clock +timer-tick+ :type fixnum)
  (timer-previous 0 :type fixnum)
  (random-state (make-random-state t)
                :type random-state
                :read-only t)
  (stack (make-array 16
           :adjustable nil
           :fill-pointer 0
           :element-type 'int12)
         :type (stack 16)))

(define-with-macro chip
  memory registers
  flag
  index program-counter
  delay-timer sound-timer timer-clock timer-previous
  random-state
  video video-dirty
  keys awaiting-key
  stack)

(declaim (inline chip-flag (setf chip-flag)))

(defun chip-flag (chip)
  (aref (chip-registers chip) #xF))

(defun (setf chip-flag) (new-value chip)
  (setf (aref (chip-registers chip) #xF) new-value))


;;;; Graphics -----------------------------------------------------------------
(declaim (inline font-location vref (setf vref))
         (ftype (function (chip int8 int8 int4) null) draw-sprite)
         (ftype (function (chip x-coord y-coord) fixnum) vref)
         (ftype (function (fixnum chip x-coord y-coord) fixnum) (setf vref)))


(defun vref (chip x y)
  (aref (chip-video chip) (+ (* 64 y) x)))

(defun (setf vref) (new-value chip x y)
  (setf (aref (chip-video chip) (+ (* 64 y) x))
        new-value))


(defun load-font (chip)
  ;; Thanks http://www.multigesture.net/articles/how-to-write-an-emulator-chip-8-interpreter/
  (replace (chip-memory chip)
           (vector #xF0 #x90 #x90 #x90 #xF0 ; 0
                   #x20 #x60 #x20 #x20 #x70 ; 1
                   #xF0 #x10 #xF0 #x80 #xF0 ; 2
                   #xF0 #x10 #xF0 #x10 #xF0 ; 3
                   #x90 #x90 #xF0 #x10 #x10 ; 4
                   #xF0 #x80 #xF0 #x10 #xF0 ; 5
                   #xF0 #x80 #xF0 #x90 #xF0 ; 6
                   #xF0 #x10 #x20 #x40 #x40 ; 7
                   #xF0 #x90 #xF0 #x90 #xF0 ; 8
                   #xF0 #x90 #xF0 #x10 #xF0 ; 9
                   #xF0 #x90 #xF0 #x90 #x90 ; A
                   #xE0 #x90 #xE0 #x90 #xE0 ; B
                   #xF0 #x80 #x80 #x80 #xF0 ; C
                   #xE0 #x90 #x90 #x90 #xE0 ; D
                   #xF0 #x80 #xF0 #x80 #xF0 ; E
                   #xF0 #x80 #xF0 #x80 #x80) ; F
           :start1 #x50))

(defun font-location (character)
  (+ #x50 (* character 5)))


(defun draw-sprite (chip start-x start-y size)
  (with-chip (chip)
    (assert (< (+ index size) +memory-size+) (index)
      "Sprite data of size ~D starting at #x~4,'0X would be out of bounds"
      size index)
    ; (format t "Drawing sprite at ~d ~d~%" start-x start-y)
    (setf flag 0)
    (iterate
      (declare (iterate:declare-variables))
      (for (the fixnum y) :from start-y :below (+ start-y size))
      (for (the y-coord screen-y) = (mod y 32))
      (for (the fixnum i) :from index)
      (for sprite = (aref memory i))
      (iterate (declare (iterate:declare-variables))
               (for (the fixnum x) :from start-x)
               (for (the x-coord screen-x) = (mod x 64))
               (for (the fixnum col) :from 7 :downto 0)
               (for (the fixnum old-pixel) = (vref chip screen-x screen-y))
               (for (the fixnum new-pixel) = (get-bit col sprite))
               ; (when (= old-pixel new-pixel 1)
               ;   (setf flag 1))
               (when (and (plusp old-pixel) (plusp new-pixel))
                 (setf flag 1))
               ; (setf (vref chip screen-x screen-y)
               ;       (logxor old-pixel new-pixel))
               (setf (vref chip screen-x screen-y)
                     (cond
                       ((and (plusp old-pixel) (plusp new-pixel)) 0)
                       ((or (plusp old-pixel) (plusp new-pixel)) 255)
                       (t 0)))))
    (setf video-dirty t))
  nil)


;;;; Keyboard -----------------------------------------------------------------
(declaim (ftype (function (chip (integer 0 (16)))) keydown keyup))

(defun keydown (chip key)
  (with-chip (chip)
    (setf (aref keys key) t)
    (when-let* ((waiting-for awaiting-key))
      (setf (aref registers waiting-for) key
            awaiting-key nil))))

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
  (replace memory registers :start1 index :end2 n))

(define-opcode op-ld-regs<mem (_ n _ _)                 ;; LD Vn < [I]
  (replace registers memory :end1 n :start2 index))

(define-opcode op-ld-reg<key (_ r _ _)                  ;; LD Vx, Key (await)
  (setf awaiting-key r))

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


;;;; Main ---------------------------------------------------------------------
(declaim
  (ftype (function (chip) null) emulate-cycle)
  (ftype (function (chip int16) null) dispatch-instruction))

(defparameter *running* t)
(defparameter *paused* nil)
(defparameter *c* nil)


(defun load-rom (chip filename)
  (fill (chip-memory chip) 0)
  (load-font chip)
  (replace (chip-memory chip) (read-file-into-byte-vector filename)
           :start1 #x200)
  (values))

(defun update-timers (chip)
  (with-chip (chip)
    (let* ((current-time (get-internal-real-time))
           (elapsed (- current-time timer-previous)))
      (decf timer-clock elapsed)
      (when (minusp timer-clock)
        (setf timer-clock +timer-tick+)
        (when (plusp delay-timer) (decf delay-timer))
        (when (plusp sound-timer) (decf sound-timer))))))

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
    (if (or *paused* awaiting-key)
      (sleep 10/1000)
      (let ((instruction (cat-bytes (aref memory program-counter)
                                    (aref memory (1+ program-counter)))))
        ; (format t "~4,'0X: ~4,'0X~%" program-counter instruction)
        (zapf program-counter (chop 12 (+ % 2)))
        (dispatch-instruction chip instruction)
        (sleep 0.001)
        (update-timers chip)))
    (setf timer-previous (get-internal-real-time))
    nil))


(defun run (rom-filename)
  (let ((chip (make-chip)))
    (setf *running* t
          *paused* nil
          *c* chip)
    (load-rom chip rom-filename)
    (bt:make-thread
      (lambda ()
        (iterate
          (while *running*)
          (emulate-cycle chip))))
    (chip8.gui::run-gui chip)))


;;;; Scratch ------------------------------------------------------------------
