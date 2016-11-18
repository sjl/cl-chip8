(in-package :chip8.debugger)
(named-readtables:in-readtable :qtools)
(declaim (optimize (debug 3)))


(defparameter *font* (q+:make-qfont "Menlo" 12))


;;;; Main GUI -----------------------------------------------------------------
(define-widget debugger (QWidget)
  ((model-disassembly :initarg :model-disassembly)
   (model-stack :initarg :model-stack)))

(define-initializer (debugger setup)
  (setf (q+:window-title debugger) "Debugger")
  (q+:resize debugger 560 400))


;;;; Disassembler -------------------------------------------------------------
;;;; Code
(defun disassemble-address (chip address)
  (-<> chip
    chip8::chip-memory
    (chip8::instruction-information <> address)))


;;;; Model
(define-widget disassembly-model (QAbstractTableModel)
  ((chip :accessor model-chip :initarg :chip)))

(define-override (disassembly-model column-count) (index)
  (declare (ignore index))
  4)

(define-override (disassembly-model row-count) (index)
  (declare (ignore index))
  (ceiling 4096 2))


(defun index-valid-p (index)
  (and (q+:is-valid index)
       (< (q+:row index) (ceiling 4096 2))))

(defun get-disassembly-contents (model row col)
  (let ((data (-<> model
                model-chip
                (disassemble-address <> (* 2 row))
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
        ((q+:qt.display-role) (get-disassembly-contents disassembly-model row col))
        ((q+:qt.font-role) *font*)
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


;;;; Layout
(define-subwidget (debugger disassembly-table) (q+:make-qtableview debugger)
  (q+:set-model disassembly-table model-disassembly)
  (q+:set-show-grid disassembly-table nil)
  (q+:set-column-width disassembly-table 0 40)
  (q+:set-column-width disassembly-table 1 60)
  (q+:set-column-width disassembly-table 2 200)
  (q+:set-column-width disassembly-table 3 90)
  (let ((vheader (q+:vertical-header disassembly-table)))
    (q+:set-resize-mode vheader (q+:qheaderview.fixed))
    (q+:set-default-section-size vheader 14)))


;;;; Stack Viewer -------------------------------------------------------------
;;;; Code
(defun stack-value (chip index)
  (aref (chip8::chip-stack chip) index))

(defun stack-size (chip)
  (length (chip8::chip-stack chip)))


;;;; Model
(define-widget stack-model (QAbstractListModel)
  ((chip :initarg :chip)))

(define-override (stack-model row-count) (index)
  (declare (ignore index))
  (stack-size chip))


(defun stack-index-valid-p (index chip)
  (and (q+:is-valid index)
       (< (q+:row index) (stack-size chip))))

(defun get-stack-contents (chip row)
  (format nil "~3,'0X" (stack-value chip row)))


(define-override (stack-model data) (index role)
  (let ((row (q+:row index)))
    (if (not (index-valid-p index))
      (q+:make-qvariant)
      (qtenumcase role
        ((q+:qt.display-role) (get-stack-contents chip row))
        ((q+:qt.font-role) *font*)
        ; ((q+:qt.text-alignment-role) (case col
        ;                                (0 #x0082)
        ;                                (1 #x0084)
        ;                                (2 #x0080)
        ;                                (3 #x0080)))
        (t (q+:make-qvariant))))))


;;;; Layout
(define-subwidget (debugger stack-list) (q+:make-qlistview debugger)
  (q+:set-model stack-list model-stack))

(define-subwidget (debugger stack-label)
  (q+:make-qlabel "Stack" debugger))

(define-subwidget (debugger stack-refresh)
  (q+:make-qpushbutton "Refresh" debugger))

(define-slot (debugger stack-refresh-pressed) ()
  (declare (connected stack-refresh (pressed)))
  (signal! model-stack (layout-changed)))


;;;; Main GUI -----------------------------------------------------------------
(define-subwidget (debugger layout) (q+:make-qhboxlayout debugger)
  (let ((disassembly (q+:make-qvboxlayout)))
    (q+:add-widget disassembly disassembly-table)
    (q+:add-layout layout disassembly))
  (let ((stack (q+:make-qvboxlayout)))
    (q+:set-fixed-width stack-label 70)
    (q+:set-fixed-width stack-list 70)
    (q+:set-fixed-width stack-refresh 80)
    (q+:add-widget stack stack-label)
    (q+:add-widget stack stack-list)
    (q+:add-widget stack stack-refresh)
    (q+:add-layout layout stack)))


(defun run (chip)
  (let ((model-disassembly (make-instance 'disassembly-model :chip chip))
        (model-stack (make-instance 'stack-model :chip chip)))
    (with-main-window (window (make-instance 'debugger
                                :model-disassembly model-disassembly
                                :model-stack model-stack)))))


(defparameter *c* (chip8::make-chip))
(chip8::load-rom *c* "roms/breakout.rom")
