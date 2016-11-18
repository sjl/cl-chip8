(defpackage :chip8
  (:use
    :cl
    :losh
    :iterate
    :cl-arrows
    :chip8.quickutils)
  (:export))


(defpackage :chip8.gui
  (:use :cl+qt :iterate :losh :cl-arrows
    :chip8.quickutils))

(defpackage :chip8.debugger
  (:use :cl+qt :iterate :losh :cl-arrows
    :chip8.quickutils))
