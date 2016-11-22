(asdf:defsystem :cl-chip8
  :name "chip8"
  :description "Simple Chip-8 emulator."

  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"
  :version "1.0.0"

  :depends-on (
               :bordeaux-threads
               :cl-arrows
               :cl-opengl
               :cl-portaudio
               :iterate
               :losh
               :qtcore
               :qtgui
               :qtools
               :qtopengl
               )

  :serial t
  :components ((:module "vendor" :serial t
                :components ((:file "quickutils-package")
                             (:file "quickutils")))
               (:file "package")
               (:module "src" :serial t
                :components ((:file "emulator")
                             (:file "debugger")
                             (:file "gui")))))
