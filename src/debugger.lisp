(in-package :chip8)

;;;; Data ---------------------------------------------------------------------
(defstruct debugger
  (paused nil :type boolean)
  (take-step nil :type boolean)
  (awaiting-arrival nil :type boolean)
  (callbacks-arrived nil :type list)
  (breakpoints nil :type list))

(define-with-macro debugger
  paused take-step awaiting-arrival
  callbacks-arrived)


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
  (cat-bytes
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
    (let ((*print-base* 16))
      (format t "~3,'0X: ~4,'0X ~24A ~8A~%"
              address
              instruction
              (or disassembly "")
              bits))))

(defun dump-disassembly (array &optional (start 0) (end (length array)))
  (iterate
    (for i :from start :below end :by 2)
    (print-disassembled-instruction array i)
    (sleep 0.001)))


;;;; Debugger API -------------------------------------------------------------
(defun debugger-pause (debugger)
  (with-debugger (debugger)
    (setf paused t awaiting-arrival t)))

(defun debugger-unpause (debugger)
  (with-debugger (debugger)
    (setf paused nil awaiting-arrival nil)))

(defun debugger-toggle-pause (debugger)
  (if (debugger-paused debugger)
    (debugger-unpause debugger)
    (debugger-pause debugger)))

(defun debugger-step (debugger)
  (setf (debugger-take-step debugger) t))

(defun debugger-arrive (debugger chip)
  (with-debugger (debugger)
    (when awaiting-arrival
      (setf awaiting-arrival nil)
      (debugger-print debugger chip)
      (mapc (rcurry #'funcall (chip-program-counter chip))
            callbacks-arrived))))

(defun debugger-print (debugger chip)
  (declare (ignore debugger))
  (print-disassembled-instruction (chip-memory chip)
                                  (chip-program-counter chip)))

(defun debugger-paused-p (debugger)
  (debugger-paused debugger))

(defun debugger-check-breakpoints (debugger address)
  "Return `t` if the debugger is at a breakpoint, `nil` otherwise."
  (if (member address (debugger-breakpoints debugger))
    (progn (debugger-pause debugger)
           t)
    nil))

(defun debugger-check-wait (debugger address)
  "Return `t` if the debugger wants execution to wait, `nil` otherwise."
  (with-debugger (debugger)
    (cond
      ;; If we're not paused, we just need to check for breakpoints.
      ((not paused) (debugger-check-breakpoints debugger address))
      ;; If we're paused, but are ready to step, go.
      (take-step (setf take-step nil awaiting-arrival t) nil)
      ;; Otherwise we're fully paused -- wait
      (t t))))

(defun debugger-add-breakpoint (debugger address)
  (pushnew address (debugger-breakpoints debugger)))

(defun debugger-remove-breakpoint (debugger address)
  (removef (debugger-breakpoints debugger) address))

(defun debugger-add-callback-arrived (debugger function)
  (push function (debugger-callbacks-arrived debugger))
  t)


