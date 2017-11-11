# LMS Black aka Purple #

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

## Running

* `sbt test`
   runs the test suite.
* `sbt console`
   opens a Scala REPL with the system already imported. Then, use `ev` to evaluate Black terms, like in the tests, e.g. `ev("(+ 1 1)")`.

## Code

* __[`eval.scala`](src/main/scala/lms/black/eval.scala)__ defines the stage-polymorphic base interpreter functions and wires the tower.
* __[`stage.scala`](src/main/scala/lms/black/stage.scala)__ defines the [LMS](https://scala-lms.github.io/tutorials/)-specific instantiation of the stage-polymorphic type class.
* The [test directory](/src/test/scala/lms/black) contains many examples.
  * __[`instr.scala`](/src/test/scala/lms/black/instr.scala)__ defines a special form to count calls to several meta-level functions.
  * __[`taba.scala`](/src/test/scala/lms/black/taba.scala)__ defines a special form taba to monitor the stack.
  * __[`delta.scala`](/src/test/scala/lms/black/delta.scala)__ defines the "classic" delta reifer to go up the tower with a reified structure for the current computation from below, and also `call/cc` in terms of `delta`.
  * __[`undo.scala`](/src/test/scala/lms/black/undo.scala)__ defines a scheme to undo at the meta-level.
  * __[`cont.scala`](/src/test/scala/lms/black/cont.scala)__ defines alternative semantics for continuations.
  * __[`re.scala`](/src/test/scala/lms/black/re.scala)__ defines a string matcher, showing that LMS-Black can also collapse ad-hoc user-level towers.

## Further Reading

* Our POPL 2018 paper, _Collapsing Towers of Interpreters ([PDF](http://lampwww.epfl.ch/~amin/pub/collapsing-towers.pdf))_.
* Some older _LMS-Black Design Document ([PDF](http://lampwww.epfl.ch/~amin/doc/lms-black.pdf))_ to be read in conjunction with this code.
