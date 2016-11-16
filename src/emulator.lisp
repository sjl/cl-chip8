(in-package :chip8)

(setf *print-length* 10)
(setf *print-base* 10)
(declaim (optimize (speed 1) (safety 3) (debug 3)))
(declaim (optimize (speed 3) (safety 0) (debug 3)))


;;;; Types --------------------------------------------------------------------
(deftype int8 () '(unsigned-byte 8))
(deftype int12 () '(unsigned-byte 12))
(deftype int16 () '(unsigned-byte 16))

(deftype basic-array (element-type size)
  `(simple-array ,(upgraded-array-element-type element-type) (,size)))

(deftype stack (size)
  `(vector ,(upgraded-array-element-type 'int12) ,size))


;;;; Utils --------------------------------------------------------------------
(declaim (inline nibble not= +_8 -_8))

(defun make-simple-array (element-type size &rest args)
  (apply #'make-array size
         :adjustable nil
         :fill-pointer nil
         :element-type element-type
         args))

(defun nibble (position integer)
  (ldb (byte 4 (* position 4)) integer))

(defun not= (x y)
  (not (= x y)))

(defun +_8 (x y)
  (let ((result (+ x y)))
    (values (ldb (byte 8 0) result)
            (if (> result 255) 1 0))))

(defun -_8 (x y)
  (let ((result (- x y)))
    (values (ldb (byte 8 0) result)
            (if (> x y) 1 0))))


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
  (video (make-simple-array 'fixnum #.(* 64 32))
         :type (basic-array fixnum #.(* 64 32))
         :read-only t)
  (video-dirty t :type boolean)
  (keys (make-simple-array 'boolean 16)
        :type (basic-array boolean 16)
        :read-only t)
  (index 0 :type int16)
  (program-counter 0 :type int12)
  (delay-timer 0 :type int8)
  (sound-timer 0 :type int8)
  (random-state (make-random-state t)
                :type random-state
                :read-only t)
  (stack (make-array 16
           :adjustable nil
           :fill-pointer 0
           :element-type 'int12)
         :type (stack 16)))

(define-with-macro chip
  memory registers video keys
  index program-counter
  delay-timer sound-timer
  random-state
  video-dirty
  stack)


;;;; Opcodes ------------------------------------------------------------------
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
                                opcode)))))))

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
    (setf ,destination ,source)
    (incf program-counter 2)))

(define-opcode op-cls ()                                ;; CLS
  (fill video 0)
  (setf video-dirty t)
  (incf program-counter 2))

(define-opcode op-jp (_ (target 3))                     ;; JP addr
  (setf program-counter target))

(define-opcode op-call (_ (target 3))                   ;; CALL addr
  (vector-push program-counter stack)
  (setf program-counter target))

(define-opcode op-ret ()                                ;; RET
  (setf program-counter (vector-pop stack)))

(macro-map                                              ;; ADD/SUB
    ((name op source-arg source-expr)
     ((op-add-reg<imm +_8 (immediate 2) immediate)
      (op-add-reg<reg +_8 (ry 1) (register ry))
      (op-sub-reg<reg -_8 (ry 1) (register ry))))
  `(define-opcode ,name (_ rx ,source-arg)
    (multiple-value-bind (result carry)
        (,op (register rx) ,source-expr)
      (setf (register rx) result
            (register #xF) carry))
    (incf program-counter 2)))

(define-opcode op-subn-reg<reg (_ rx ry)                ;; SUBN
    (multiple-value-bind (result carry)
        (-_8 (register ry) (register rx)) ; subtraction order is swapped for SUBN
      (setf (register rx) result
            (register #xF) carry))
    (incf program-counter 2))

(macro-map                                              ;; SE/SNE
    ((name test x-arg y-arg)
     ((op-se-reg-imm  =    (r 1) (immediate 2))
      (op-sne-reg-imm not= (r 1) (immediate 2))
      (op-se-reg-reg  =    (rx 1) (ry 1))
      (op-sne-reg-reg not= (rx 1) (ry 1))))
  `(define-opcode ,name (_ ,x-arg ,y-arg)
    (incf program-counter
          (if (,test ,(car x-arg) ,(car y-arg)) 4 2))))

(macro-map                                              ;; AND/OR/XOR
    ((name function)
     ((op-or logior)
      (op-and logand)
      (op-xor logxor)))
  `(define-opcode ,name (_ destination source _)
    (zapf (register destination)
          (,function % (register source)))
    (incf program-counter 2)))

(define-opcode op-rnd (_ r (mask 2))                    ;; RND
  (setf (register r)
        (logand (random 256 random-state) mask))
  (incf program-counter 2))

(define-opcode op-skp (_ r _ _)                         ;; SKP
  (incf program-counter (if (aref keys (register r)) 4 2)))

(define-opcode op-sknp (_ r _ _)                        ;; SKNP
  (incf program-counter (if (not (aref keys (register r))) 4 2)))

(define-opcode op-ld-mem<regs (_ n _ _)                 ;; LD [I] < V_n
  (replace memory registers :start1 index :end2 n)
  (incf program-counter 2))

(define-opcode op-ld-regs<mem (_ n _ _)                 ;; LD V_n < [I]
  (replace registers memory :end1 n :start2 index)
  (incf program-counter 2))


;;;; Keyboard -----------------------------------------------------------------
(defun keydown (chip key)
  (with-chip (chip)
    (setf (aref keys key) t)))

(defun keyup (chip key)
  (with-chip (chip)
    (setf (aref keys key) nil)))


;;;; Graphics -----------------------------------------------------------------

;;;; Main ---------------------------------------------------------------------
(defun load-rom (chip filename)
  (replace (chip-memory chip) (read-file-into-byte-vector filename)
           :start1 #x200))

(defun emulate-cycle (chip)
  (with-chip (chip)
    (let ((opcode (logior (ash (aref memory program-counter) 8)
                          (aref memory (1+ program-counter)))))
      (macrolet ((call (name) `(,name chip opcode)))
        (case (logand #xF000 opcode)
          (#x0 (ecase opcode
                 (#x00E0 (call op-cls))
                 (#x00EE (call op-ret))))
          (#x1 (call op-jp))
          (#x2 (call op-call))
          (#x3 (call op-se-reg-imm))
          (#x4 (call op-sne-reg-imm))
          (#x5 (ecase (logand #x000F opcode)
                 (#x0 (call op-se-reg-reg))))
          (#x6 (call op-ld-reg<imm))
          (#x7 (call op-add-reg<imm))
          (#x8 (ecase (logand #x000F opcode)
                 (#x0)
                 (#x1)
                 (#x2)
                 (#x3)
                 (#x4)
                 (#x5)
                 (#x6)
                 (#x7)
                 (#xE)))
          (#x9 (ecase (logand #x000F opcode)
                 (#x0)))
          (#xA (call op-ld-i<imm))
          (#xB)
          (#xC (call op-rnd))
          (#xD)
          (#xE (ecase (logand #x00FF opcode)
                 (#x9E (call op-skp))
                 (#xA1 (call op-sknp))))
          (#xF (ecase (logand #x00FF opcode)
                 (#x07 (call op-ld-reg<dt))
                 (#x0A)
                 (#x15 (call op-ld-dt<reg))
                 (#x18 (call op-ld-st<reg))
                 (#x1E)
                 (#x29)
                 (#x33)
                 (#x55 (call op-ld-mem<regs))
                 (#x65 (call op-ld-regs<mem)))))))))


(defun draw-graphics (chip))
(defun handle-keys (chip))

(defparameter *running* t)

(defun run ()
  (let ((chip (make-chip)))
    (setf *running* t)
    ;; init
    ;; load rom
    (iterate
      (while *running*)
      (emulate-cycle chip)
      (handle-keys chip)
      )
    )
  )
