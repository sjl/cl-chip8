(asdf:defsystem :cl-chip8
  :name "chip8"
  :description "Simple Chip-8 emulator."

  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"
  :version "1.0.0"

  :depends-on (:iterate
               :losh
               :cl-arrows
               :bordeaux-threads
               :cl-portaudio
               :qtools
               :qtcore
               :qtgui
               :qtopengl
               :cl-opengl)

  :serial t
  :components ((:module "vendor" :serial t
                :components ((:file "quickutils-package")
                             (:file "quickutils")))
               (:file "package")
               (:module "src" :serial t
                :components ((:file "emulator")
                             (:file "debugger")
                             (:file "gui")))))
