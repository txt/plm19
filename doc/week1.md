
[home](http://tiny.cc/plm19) |
[copyright](https://github.com/txt/plm19/blob/master/license.md) &copy;2019, timm&commat;ieee.org
<br>
<a href="http://tiny.cc/plm19"><img width=900 src="https://raw.githubusercontent.com/txt/plm19/master/etc/img/banner.png"></a>
<br>
[syllabus](https://github.com/txt/plm19/blob/master/doc/syllabus.md) |
[src](https://github.com/txt/plm19/tree/master/src) |
[submit](http://tiny.cc/plm19give) |
[chat](https://plm19.slack.com/)

# Review, week1

## Words

- shebang
- syntax
- semantics


## Pipe and Filter

<img width=600 align=right src="https://www.cs.uic.edu/~jbell/CourseNotes/OO_SoftwareEngineering/images/Chapter6/Fig6-25.jpg">

- **Intent**  Divide and conqueor

- **Probems**: 
    Need to apply a series of ordered but independent computations is required, perhaps as a series of time-step operations, on ordered data.

- **Structure**: Pipes (that move things) and filters (that change things).

- **Example**:  Bell Labs. 1970s. Hundreds of Ph.D. comptuer scientists with no job descriptions all looking for ways 
   to work together (this is the world that gave us UNIX). They broke all their work up into tiny tools (filters)
   which could be wired together in many and magical magnficent ways.


- **Rules of thumb**:
   - K.I.S.S. Unix piles are (mostly) one input, one output (i.e. "|" not "Y" or "T"); makes
    then **simple** to use
   - Use a common  format for all i/o (e.g text files, comma-seperated-values, json)
   - Each filter should do one thing well and one thing only.
   - Useful for multi-tasking (operating system can run each filter seperately).
   - Don't use when 2 filters need to tightly co-ordinate with each other
   - Don't use when very complex state has to be transferred between the filters 
   - Can be slower to run than "one big pile of code" since so much i/o between filters
   - Can be easier to maintain than "one big pile of code" since easier to upgrade each bit in isolation 
   - Don't use for GUI applications where users want to hunt and peck all over different parts of the pipe (since
     pipes are fast methods for going here to there).

## Precendence-Driven parser
<img align=right width=400 src="https://students.cs.byu.edu/~cs235ta/labs/images/expressions.png">
Simple way to specify the syntax of a simple language.

Pre-processor for interpreter.

Core cocentps:

- parse tree
- operator
- operand
- predence
- predence clash
- associativity
- xfx, fx, xf
- yfx, xfy, fy, yf
- prefix, index, postfix

## Interpreter


<img align=right width=500 src="https://upload.wikimedia.org/wikipedia/commons/thumb/b/bc/Interpreter_UML_class_diagram.svg/536px-Interpreter_UML_class_diagram.svg.png">

- **Intent** Seperate what you want to say from underlying
  execution device
   - You want some platfrom independent way
  to express solutions. 
   - Or you want to think one way
   (e.g. pattern matching, lambda bodies, compartmental models, agents, objects, classes, messages) and the underlying
   machine does not support that (e.g. it only knows
   about stacks and registers). 
   - Or you want to batch
   up many of the underlying services into some
   convenient, more succinct, summary language
- **Problem** 
   - How can a grammar for a simple language be defined 
     so that sentences in the language can be interpreted? 

   - **Examples**
  - Homework 1a
  - any interpretered language (Python, Runy, Prolog...)
- **Stucture**
   - Grammaer
   - Terminal (has 0 rewrites)
   - Non-terminals (has N rewrites)
   - Abstract syntax tree (AST) : which part
     of the grammer is needed to describe 
     a particular set of sentences.
- **Names**
    - Interpreter
    - Abstract machine
- **Discussion**
    - Interpreters are one of the most stunning successes
      of computer science. Fast, easy to understand,
      cross platform tools enabled the open source
      revolution and, in directly, the move to cloud
      compute (you can't shuffle things round cloud CPU
      farms unless everything runs the same on all 
      platforms).
    - Studying interepreters, and more importantly,
      writing your own, is the best way to understand a 
      a langguage.
- **Rules of Thumb** <img align=right width=600 src="https://cdncontribute.geeksforgeeks.org/wp-content/uploads/compilerDesign.jpg">
    - Intepret into a virtual machine, then build an execution tool for the virtual machine (e.g. Java ==> JVM)
         - e.g. Smalltalk ==> bytecodes, 
         - e.g. JAVA ==? JVM
         - e.g. Lisp ==> lambda bodies
          - Useful for cross platfrom stuff
    - To build a compiler, interpret into machine code then cache that intepretation
         - So the next time you want to do something, no wait for the interpreter
    - Seperate out _lexical analyzer_ (strings to tokens) from _syntactical analyzer_ (tokens to
      parse trees) from semantic analyzer (how to run the trees) from _optimizer_ from _target
      code generation_
    - Simple, uniform semantics makes interpretation easier
    - Added layers of interpretation slows down the program
      - So yes, [JAVA is slower than C](https://benchmarksgame-team.pages.debian.net/benchmarksgame/which-programs-are-fast.html) 
    - CPUs are faster these days so why not interpret?
    - For many applciations, network latency is what drives runtime speed so adding an interepred language doesn't slow
      down anything very much.
    - Maintainability and usability can be more important than mere speed
    - Sometimes, the clarity of the higher level
      interpreted system can make it optimize better
- **See also**
    - Macros
    - Lambda calculus (where "computation" means  rewriting sub-trees)
    - Unification (where "computation" means  
      matching sub0trees)
    - _Composite_ pattern provides a way to represent a part-whole hierarchy 
as a tree (composite) object structure.
    - _Builder_ pattern provides a way to create the elements of an object structure.
     _Iterator_ pattern provides a way to traverse the elements of an object structure.
    - _Visitor_ pattern provides a way to define new operations for the elements of an object structure.
    - _Interpreter_ pattern represents a sentence in a simple language as a tree (composite) object structure (abstract syntax tree).


