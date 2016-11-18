(in-package :chip8.debugger)
(named-readtables:in-readtable :qtools)
(declaim (optimize (debug 3)))


;;;; Disassembler -------------------------------------------------------------
(defun disassemble-instruction (instruction)
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
      (#x3000 `(se (v ,_x__) ,__xx))
      (#x4000 `(sne (v ,_x__) ,__xx))
      (#x5000 (case (logand #x000F instruction)
                (#x0 `(se (v ,_x__) (v ,__x_)))))
      (#x6000 `(ld (v ,_x__) ,__xx))
      (#x7000 `(add (v ,_x__) ,__xx))
      (#x8000 (case (logand #x000F instruction)
                (#x0 `(ld (v ,_x__) (v ,__x_)))
                (#x1 `(or (v ,_x__) (v ,__x_)))
                (#x2 `(and (v ,_x__) (v ,__x_)))
                (#x3 `(xor (v ,_x__) (v ,__x_)))
                (#x4 `(add (v ,_x__) (v ,__x_)))
                (#x5 `(sub (v ,_x__) (v ,__x_)))
                (#x6 `(shr (v ,_x__) (v ,__x_)))
                (#x7 `(subn (v ,_x__) (v ,__x_)))
                (#xE `(shl (v ,_x__) (v ,__x_)))))
      (#x9000 (case (logand #x000F instruction)
                (#x0 `(sne (v ,_x__) (v ,__x_)))))
      (#xA000 `(ld i ,_xxx))
      (#xB000 `(jp (v 0) ,_xxx))
      (#xC000 `(rnd (v ,_x__) ,__xx))
      (#xD000 `(drw (v ,_x__) (v ,__x_) ,___x))
      (#xE000 (case (logand #x00FF instruction)
                (#x9E `(skp (v ,_x__)))
                (#xA1 `(sknp (v ,_x__)))))
      (#xF000 (case (logand #x00FF instruction)
                (#x07 `(ld (v ,_x__) dt))
                (#x0A `(ld (v ,_x__) k))
                (#x15 `(ld dt (v ,_x__)))
                (#x18 `(ld st (v ,_x__)))
                (#x1E `(add i (v ,_x__)))
                (#x29 `(ld f (v ,_x__)))
                (#x33 `(ld b (v ,_x__)))
                (#x55 `(ld (mem i) ,_x__))
                (#x65 `(ld ,_x__ (mem i))))))))

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
  (chip8::cat-bytes (aref array index)
                    ;; ugly hack to handle odd-sized roms
                    (if (< (1+ index) (length array))
                      (aref array (1+ index))
                      0)))

(defun instruction-information (array index)
  (let ((instruction (retrieve-instruction array index)))
    (list index
          instruction
          (disassemble-instruction instruction)
          (bit-diagram instruction))))


(defun disassemble-instructions (array start)
  (iterate
    (for i :from start :below (length array) :by 2)
    (collect (instruction-information array i) :result-type vector)))

(defun dump (array start &optional (offset 0))
  (iterate (for (address instruction disassembly bits)
                :in-vector (disassemble-instructions array start))
           (sleep 0.005)
           (format t "~3,'0X: ~4,'0X ~24A ~8A~%"
                   (+ address offset)
                   instruction
                   (or disassembly "")
                   bits)))


(defparameter *rom*
  (read-file-into-byte-vector "roms/merlin.rom"))

(defparameter *test*
  (disassemble-instructions *rom* 0))


;;;; Model --------------------------------------------------------------------
(defparameter *font* (q+:make-qfont "Menlo" 12))
(defparameter *font2* (q+:make-qfont "Menlo" 12))

(define-widget disassembly-model (QAbstractTableModel)
  ((data :accessor model-data :initarg :data :initform *test*)
   (chip :accessor model-chip :initarg :chip)))

(define-override (disassembly-model column-count) (index)
  (declare (ignore index))
  4)

(define-override (disassembly-model row-count) (index)
  (declare (ignore index))
  (ceiling 4096 2))


(defun index-valid-p (index)
  (and (q+:is-valid index)
       (< (q+:row index) (ceiling 4096 2))))

(defun get-contents (model row col)
  (let ((data (-<> model
                model-chip
                chip8::chip-memory
                (instruction-information <> (* 2 row))
                (nth col <>))))
    (ecase col
      (0 (format nil "~3,'0X" data))
      (1 (format nil "~4,'0X" data))
      (2 (if data
           (let ((*print-base* 16))
             (format nil "~A ~{~S~^, ~}" (first data) (rest data)))
           ""))
      (3 data))))

(define-override (disassembly-model data) (index role)
  (let ((row (q+:row index))
        (col (q+:column index)))
    (if (not (index-valid-p index))
      (q+:make-qvariant)
      (qtenumcase role
        ((q+:qt.display-role) (get-contents disassembly-model row col))
        ((q+:qt.font-role) (case col
                             (3 *font2*)
                             (t *font*)))
        ((q+:qt.text-alignment-role) (case col
                                       (0 #x0082)
                                       (1 #x0084)
                                       (2 #x0080)
                                       (3 #x0080)))
        (t (q+:make-qvariant))))))

(define-override (disassembly-model header-data) (section orientation role)
  (case role
    (0 (qtenumcase orientation
         ((q+:qt.vertical) (q+:make-qvariant))
         ((q+:qt.horizontal) (case section
                               (0 "Addr")
                               (1 "Inst")
                               (2 "Disassembly")
                               (3 "Bits")))))
    (t (q+:make-qvariant))))


(define-widget debugger (QWidget)
  ((model :accessor debugger-model :initarg :model)))


(define-subwidget (debugger table) (q+:make-qtableview debugger)
  (q+:set-model table model)
  (q+:set-show-grid table nil)
  (q+:set-column-width table 0 40)
  (q+:set-column-width table 1 60)
  (q+:set-column-width table 2 200)
  (q+:set-column-width table 3 90)
  (let ((vheader (q+:vertical-header table)))
    (q+:set-resize-mode vheader (q+:qheaderview.fixed))
    (q+:set-default-section-size vheader 14)
    )
  )

(define-subwidget (debugger layout) (q+:make-qvboxlayout debugger)
  (q+:add-widget layout table))


(defun run (chip)
  (let ((model (make-instance 'disassembly-model :chip chip)))
    (with-main-window (window (make-instance 'debugger :model model)))))

(defparameter *c* (chip8::make-chip))
