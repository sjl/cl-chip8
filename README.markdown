A Chip-8 emulator in Common Lisp.

* **License:** MIT

Building
--------

To build this project you'll need to do a few things.

First, install [SBCL][].  You can use your package manager (homebrew, apt,
whatever), or use [Roswell][], or just get some binaries.  This project uses
a few SBCL-specific things, so it won't work with other implementations.

Next, install [Quicklisp][].

Finally you'll need to clone down this repo and [my utility library][losh] and
symlink them into Quicklisp's [local-projects][] directory.

Once you've done all that you should be able to run `sbcl` and use
`(ql:quickload :cl-chip8)` to load the project and all of its dependencies.
Then use `(chip8::run "path/to/a/chip8.rom")` to start it up.

[SBCL]: http://www.sbcl.org/
[Roswell]: https://github.com/roswell/roswell
[Quicklisp]: https://www.quicklisp.org/beta/
[losh]: https://github.com/sjl/cl-losh
[local-projects]: https://www.quicklisp.org/beta/faq.html#local-project

References
----------

* <http://devernay.free.fr/hacks/chip8/C8TECH10.HTM>
* <http://mattmik.com/files/chip8/mastering/chip8.html>
* <https://github.com/AfBu/haxe-chip-8-emulator/wiki/(Super)CHIP-8-Secrets>
* <https://github.com/trapexit/chip-8_documentation>
