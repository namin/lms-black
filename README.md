# LMS Black #

In a reflective programming language such as
[Black](https://github.com/readevalprintlove/black), user programs are
interpreted by an infinite tower of meta-circular interpreters. Each
level of the tower can be accessed and modified, so the semantics of
the language changes dynamically during execution.

In this project, we show that it is possible to compile a user program
under modified, possibly also compiled, semantics -- a question raised
by Kenichi Asai in his GPCE 2014 paper, _Compiling a Reflective
Language using MetaOCaml_.

How? First, all functions (at any level) are polymorphic as to whether
they generate code or run. Second, the code generated is also
polymorphic in this way!
