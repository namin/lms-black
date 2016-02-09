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

How? All functions are polymorphic as to whether they generate code or run.
Generated code, when compiled, is also polymorphic in this way so that there's
no difference between built-in and compiled functions.

An [example](src/test/scala/lms/black/em.scala#L15).
Suppose we change the meta-interpreter so that it increments
a counter each time a variable named `n` is accessed. Re-defining fibonacci
with parameter `n` as a compiled lambda will generate code that includes
code for a counter increment each time `n` is mentioned in the body
(compare [test](src/test/scala/lms/black/gen.scala#L15)-generated code
[without](src/out/fib.check.scala) vs
[with](src/out/fib_em_var_counter.check.scala) the meta-change).
Running this compiled `fib` function has the same behavior as an uncompiled
`fib` function evaluated by the modified interpreter. Still, once we undo
the modifications to the interpreter, the previously compiled `fib` function
will still update the counter.
