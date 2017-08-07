(defpackage :chip8
  (:use
    :cl
    :losh
    :iterate
    :chip8.quickutils)
  (:export))


(defpackage :chip8.gui.screen
  (:use :cl+qt :iterate :losh
    :chip8.quickutils))

(defpackage :chip8.gui.debugger
  (:use :cl+qt :iterate :losh
    :chip8.quickutils))
