
[home](http://tiny.cc/plm19) |
[copyright](https://github.com/txt/plm19/blob/master/license.md) &copy;2019, timm&commat;ieee.org
<br>
<a href="http://tiny.cc/plm19"><img width=900 src="https://raw.githubusercontent.com/txt/plm19/master/etc/img/banner.png"></a>
<br>
[syllabus](https://github.com/txt/plm19/blob/master/doc/syllabus.md) |
[src](https://github.com/txt/plm19/tree/master/src) |
[submit](http://tiny.cc/plm19give) |
[chat](https://plm19.slack.com/)


# Roll Your Own Language

Why write a language? Many reasons: fun, an itch to scrtach, a good way to learn core computer science, as an exploration of an important conceept, a way to stress test some ideas. 

Also, if your company is the main providor for that language, or tools for that language, then that
language can earn and keep your market share

- e.g. For decades, Microsoft wanted you to use tools that  forever hard-wired you into their products 

(Incidently, there is deep reasons why that works-- many animals, including us, mark territory with langauge.)

## The easiest easiest way to roll your own

- "External" domain-specific languages
    - Write macros, sub-routines, classes to batch up complex sets of operations
      behind simpler, more succinct interfaces.
         - E.g. the "program" is some tiny sub-classes that defines 
	   itself as an extension to some (possibly large) set of super-classes.
- Don't do it
    - Just use Lua (as done in Vim, Tex, 100 games...)
        - Interpreter designed to be embeddable 
        - Very clean interface to "C"

## The easy way

Easy way (for me, but others disagree):

- Meta-interpreters in Prolog
- Macros in Lisp

An increasingly commont way:

- transpilers (source to source translation). 
     - Ugly way: Parse your preferred languages, walk the parse tree spitting  out  phrases in the target language
            - Hint: use a target language with a  very simple, regular semantics (e.g. Lua).
     - Nicer way: grammars
            - e.g. [Coffeescript](https://coffeescript.org/#language)'s [grammer](https://coffeescript.org/v1/annotated-source/grammar.html)
     - The transpiler curse: code in X debug info in Y
     - The transpiler curse: code in X restricted by what you can do in Y

Other ways

- Assume a (simple) core semantics, and build for that:
    - e.g. state machines, 
    - e.g. compartmental models
    - e.g. lambda bodies (ECMAScript, [lis.py](http://norvig.com/lispy.html) or [lispy2](http://norvig.com/lispy2.html))
    - e.g. unification (if you like logic)
    - Rule of thumb: the simpler and more uniform your semantics, the smaller your interpreter
        - But beware! [The microkernel story](https://www.ibm.com/developerworks/community/blogs/6e6f6d1b-95c3-46df-8a26-b7efd8ee4b57/entry/linux_is_obsolete_a_must_read_debate_between_andrew_s_tanenbaum_and_linus_torvalds34?lang=en)
- The traditional way
    - [BNF](https://en.wikipedia.org/wiki/Backus–Naur_form#Example)  to define the language
    - Grammers to  define valid sentences and [precedence](https://en.wikibooks.org/wiki/Introduction_to_Programming_Languages/Precedence_and_Associativity)
    - Lex (lexical syntactic analysis): generates a program that will tokenize your code 
    - Yacc (yet another compiler compiler, for the semantics): generates a progarm that adds semantics to your parse

And, if you want to massively scale your program

- Bit the bullet, do the deep dive
- Welcome to LLVM

## LLVM


Illustrates many of the complexities of compilation.

Simplifies them. Abstracts them away to numerous separate modules... which means now you can add your own.

At the front end of LLVM you can have C, Scala, Perl, Lua, and many other high level languages. At the backend, you have the natives code that run directly on the machine.

At the center is your intermediate code representation. If every high level languages can be represented in this LLVM IR format, then analysis tools based on this IR can be easily reused - that is the basic rational.

![](https://i.stack.imgur.com/9xGDe.png)

Useful for many things (including just in time compilation) including compile-time  transformations
and of code. It also consists of a number of tools serving distinct usages. e.g.

- llvm-prof is a profiling tool that allows you to do profiling of execution in order to identify program hotspots. 
- Opt is an optimization tool that offers various optimization passes (dead code elimination for instance).

 Importantly LLVM provides you with the libraries, to write your own **Passes**. For instance if you require to add a range check on certain arguments that are passed into certain functions of a Program, writing a simple LLVM Pass would suffice.



## Why LLVM?

Although gcc is the most common suite of compilers , it is not built to be re-usable ie. it is difficult to take components from gcc and use it to build your own application . LLVM addresses this issue well by building a set of " modular and reusable compiler and toolchain technologies" which anyone could use to build compilers and language oriented software.

For example of why that is useful, consider the wonderful luajit just-in-time-compiler for lua which can result in great [performance gains](http://luajit.org/performance_x86.html).  


- In computing, just-in-time (JIT) compilation (also dynamic translation or run-time compilations)[1] is a way of executing computer code that involves compilation during execution of a program – at run time – rather than prior to execution. Most often, this consists of source code or more commonly bytecode translation to machine code, which is then executed directly. A system implementing a JIT compiler typically continuously analyses the code being executed and identifies parts of the code where the speedup gained from compilation or recompilation would outweigh the overhead of compiling that code.

The original author of LuaJist moved on to other things and now we have a great optimizer for Lua code up to 5.1 but but supportive of Lua 5.3. 

Is it better to use tools known to more people? [Some people](https://github.com/gligneul/FastLua) [think so](https://github.com/dibyendumajumdar/ravi).

- Reality check: currently, LuaJit is pragmatically the best. 
- But as Lua evolves for 5.4, 6, 7 .. what then?

## Examples

No, I do not understand all the [passes available in LLVM](https://llvm.org/docs/Passes.html)
but they divide into:

- analysis (thing, attributed grammars... looking at the  code and reporting extra things); e.g.
    - [sanity check](https://llvm.org/docs/Passes.html#lint-statically-lint-checks-llvm-ir)
    - some [safety checks](https://llvm.org/docs/Passes.html#stack-safety-stack-safety-analysis)
- transformations (think optimization's); e.g
    - [Remove redunancies](https://llvm.org/docs/Passes.html#instcombine-combine-redundant-instructions)   
    - [Canonical's induction variables](https://llvm.org/docs/Passes.html#indvars-canonicalize-induction-variables)
    - [Unroll and jam the loops](https://llvm.org/docs/Passes.html#loop-unroll-and-jam-unroll-and-jam-loops)
          - I spent much time trying to understand this one. Talked to a lot of people. In the end, the best they could tell me  is that this one makes other optimizations awesome, for reasons I did not understand.
          - Meta-lesson: optiization is black art.  Tread carefully!
    - remove [dead code](https://llvm.org/docs/Passes.html#dce-dead-code-elimination); i.e. things we cannot reach
    - simple [constraint propergation](https://llvm.org/docs/Passes.html#constprop-simple-constant-propagation)
    - [inlining](https://llvm.org/docs/Passes.html#inline-function-integration-inlining) of simple functions
    - [unswitching loops](https://llvm.org/docs/Passes.html#loop-unswitch-unswitch-loops)
    - [anonymous code](https://llvm.org/docs/Passes.html#strip-strip-all-symbols-from-a-module)
    - [tail call elimination](https://llvm.org/docs/Passes.html#tailcallelim-tail-call-elimination)
- utilities (reports); e.g.
    - [view the call graph](https://llvm.org/docs/Passes.html#tailcallelim-tail-call-elimination)
    - [checking for dumb things](https://llvm.org/docs/Passes.html#verify-module-verifier) (and yes, this could also
         have been an _analysis_ pass...).

The point here is that if you want more than the above, LLVM lets you [write your own](http://llvm.org/docs/WritingAnLLVMPass.html).

## Warnings

Good news: 

- if you do all these, you get faster code!

Bad news: 

- ever tried debugging optimized code? Shudder.
- and if optimizations are platform dependent then different semantics on different platforms (Shudder).

Premature optimization is the root of all evil -- Donald Knuth

In Donald Knuth's paper "StructuredProgrammingWithGoToStatements", he wrote: "Programmers waste enormous amounts of time thinking about, or worrying about, the speed of noncritical parts of their programs, and these attempts at efficiency actually have a strong negative impact when debugging and maintenance are considered. We should forget about small efficiencies, say about 97% of the time: premature optimization is the root of all evil. Yet we should not pass up our opportunities in that critical 3%."

So Premature Optimization can be defined (in less loaded terms) as optimizing before we know that we need to.

Step1: write "it"  clean

Step2: get experience with "it"

Step3: then optimize "it"
