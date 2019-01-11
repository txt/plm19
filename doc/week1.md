
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
- parse tree
- pipe and filter
- operator
- operand
- predence
- predence clash
- associativity
- xfx, fx, xf
- yfx, xfy, fy, yf
- prefix, index, postfix

## Abstractions

### Parse tree


Constraints:

Very small amount of primary memory, typically orders of magnitude smaller than the data that needs to be processed/generated. (The example sets the limit to 1024 cells)

No labels -- i.e. no variable names or tagged memory addresses. All we have is memory that is addressable with numbers.

Possible names:

Good old times
Early 50s style
NOTE: this example program takes a good 5-10 minutes to run over Pride and Prejudice

### Interpreter

- **Intent** Seperate what you want to say from underlying
  execution device
- **Problem** 
   - You want some platfrom independent way
  to express solutions. 
   - Or you want to think one way
   (e.g. Objects, classes, messages) and the underlying
   machine does not support that (e.g. it only knows
   about stacks and registers). 
   - Or you want to batch
   up many of the underlying services into some
   convenient, more succinct, summary language
- **Examples**
  - Homework 1a
  - any interpretered language (JAVA, Lisp, smalltalk, Prolog, Awk,...)
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
- **Rules of Thumb**
    - Simple, uniform semantics is key (think lambda!).
    - Added layers of interpretation slows down the program
      - So yes, [JAVA is slower than C](https://benchmarksgame-team.pages.debian.net/benchmarksgame/which-programs-are-fast.html) 
    - Maintainability and usability can be more important
      than mere speed
    - And CPUs are faster these days
    - and, sometimes, the clarity of the higher level
      interpreted system can make it optimize better
- **See also**
    - Grammers
    - Macros
    - Virtual machone
    - Lambda bodies
    - Unification
