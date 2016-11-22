(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :compose
               :curry
               :ensure-boolean
               :ensure-gethash
               :ensure-list
               :once-only
               :rcurry
               :read-file-into-byte-vector
               :symb
               :with-gensyms
               :xor

               )
  :package "CHIP8.QUICKUTILS")
