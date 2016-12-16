(defpackage :chip8
  (:use
    :cl
    :losh
    :iterate
    :cl-arrows
    :chip8.quickutils)
  (:export))


(defpackage :chip8.gui.screen
  (:use :cl+qt :iterate :losh :cl-arrows
    :chip8.quickutils))

(defpackage :chip8.gui.debugger
  (:use :cl+qt :iterate :losh :cl-arrows
    :chip8.quickutils))
