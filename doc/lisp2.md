

<a href="http://tiny.cc/plm19"><img width=1000 src="https://raw.githubusercontent.com/txt/plm19/master/etc/img/banner.png"></a><br>
&nbsp;<a href="http://tiny.cc/plm19">home</a> |
<a href="https://github.com/txt/plm19/blob/master/doc/syllabus.md">syllabus</a> |
<a href="https://github.com/txt/plm19/tree/master/src">src</a> |
<a href="http://tiny.cc/plm19give">submit</a> |
<a href="https://plm19.slack.com/">chat</a> |
<a href="https://github.com/txt/plm19/blob/master/LICENSE.md">&copy;2019</a> 
by <a href="http://menzies.us">Tim Menzies</a>


# LISP Macros


## What is a Macro and Why Should I Care?

A macro (short for "macroinstruction")
is a program called at runtime to write other programs.

They are used to expand shorthand into longhand.

    ;; example in Clojure
    (defmacro on-error [default-value code]
      `(try ~code (catch Exception ~'e ~default-value)))
    
    (on-error 0 (+ nil nil))               ;; would normally throw NullPointerException
    => 0                                   ;l; but we get the default value
    
There are simple lexical
text-substitution macro
languages like the "C" pre-processor or ye olde M4.
Note that these simple macro systems
have no access to the semantics
of the underlying language.

      # example in M4
      define(`ALPHA', `abcdefghijklmnopqrstuvwxyz')
      define(`ALPHA_UPR', `ABCDEFGHIJKLMNOPQRSTUVWXYZ')
      define(`ROT13', `nopqrstuvwxyzabcdefghijklm')
    
      translit(`abc ebg13', ALPHA, ALPHA_UPR)
      # -> ABC EBG13
      # -> ABC EBG13
      
      translit(`abc ebg13', ALPHA, ROT13)
      # -> nop rot13
      define(`eng',`engineering')
      substr(`engineer',0,3)           # -> eng -> engineering
      translit(`rat', ALPHA, ROT13)    # -> eng -> engineering
    
Then there are somewhat clean macro
languages that offer a declarative view of
the semantics, like the Moustache library
available in Python, Ruby, Java, JavaScript,
Lua, etc etc
    
    # example is moustache
    {
      "beatles": [
        { "firstName": "John", "lastName": "Lennon" },
        { "firstName": "Paul", "lastName": "McCartney" },
        { "firstName": "George", "lastName": "Harrison" },
        { "firstName": "Ringo", "lastName": "Starr" }
      ],
      "name": function () {
        return this.firstName + " " + this.lastName;
      }
    }
    
Template:
    
    {{#beatles}}
    * {{name}}
    {{/beatles}}

Note that the first line is actually a condition.
If `#beatles` returns a zero count, nothing
is generated.

Output:

    * John Lennon
    * Paul McCartney
    * George Harrison
    * Ringo Starr

I've written whole web sites that are just Mustache expansions.

Most Macro languages offer only a tiny subset
of a full language. A notable exception
is the `defmacro`
operator of LISP
that
offer the whole power of the underlying language
as part of the macro system. `defmacros` are
LISP functions that return a list which
the compiler then compiles as "real lisp".

This is particularly nice since

- LISP manipluates lists (LISP= list processing)
- LISP
  programs are lists (in fact, in ye olde times, a LISP
  function was jsut a list starting with `lambda`.
- LISP macros rewrite lists to add in the required
  details.


For decades, LISP was the king of macros. Now, finally,
other languages have caught on. So 
[CLOJURE](https://www.slideshare.net/pcalcado/lisp-macros-in-20-minutes-featuring-clojure-presentation),
[ELIXR](https://elixir-lang.org/getting-started/meta/macros.html),
Prolog, Dylan, Scala, Nemerle, Rust, 
Julia, etc, etc have macro systems that are
(nearly) as powerful as LISP. So macros flourish!


But the more powerful the macro language,
the more skill required to use
them wisely.
Beginners have a lot of trouble with 
macros. 
For example, here is an example in the GNU C pre-processor that
goes terrible wrong due to "variable capture"

    #define LOG(msg) ({ \
        int state = get_log_state(); \
        if (state > 0) { \
            printf("log(%d): %s\n", state, msg); \
        } \
    })
    
Here’s a simple use case that goes terribly wrong:

    const char *state = "reticulating splines";
    LOG(state)
    
This expands to

    const char *state = "reticulating splines";
    {
        int state = get_log_state();
        if (state > 0) {
            printf("log(%d): %s\n", state, state);
        }
    }
    
Note the double call the `state` in the last line. To avoid variable
capture, we need "hygenic macros" (see below) that are 
guaranteed not to cause the accidental capture of identifiers.

But experienced programmers
use them, a lot. 
For the absolute best book on macros in LISP, see the amazing Let
Over Lambda (http://letoverlambda.com/) book by Doug Hoyte.  Absolutely
not for beginners.

Note that there is much more to writing macros than shown below. For
more details, see http://www.gigamonkeys.com/book/macros-defining-your-own.html

## Macros in Julia

What is Julia? Think "next generation Python". 
[Fast as heck](https://julialang.org/#high-performance-jit-compiler). 
Scales.  A language to watch.

From [Julua](https://julialang.org/):

- Julia is a high-level, high-performance dynamic programming
  language for numerical computing. It provides a sophisticated
  compiler, distributed parallel execution, numerical accuracy, and
  an extensive mathematical function library. Julia’s Base library,
  largely written in Julia itself, also integrates mature, best-of-breed
  open source C and Fortran libraries for linear algebra, random
  number generation, signal processing, and string processing.

But right now, our focus is on one part of Julia- its
type system.
Julia has a nice type system, with some limits. E.g. default
Julia does not let you define types with default variables.

So here's a macro that genertes a `type` and `function $(name)`.

    # example in Julia
    # define types and a constructor that drops in the
    # right default values
    # e.g. @def emp age=0 salary=10000
    macro has(typename, pairs...)
        name = esc(symbol(string(typename,0))) # hygiene
        x    = esc(symbol("x"))                # hygiene 
        ones = [  x.args[1]  for x in pairs ]
        twos = [  x.args[2]  for x in pairs ]
        sets = [ :($x.$y=$y) for y in ones  ]
        :(type $(typename)
             $(ones...)
          end;
          function $(name)(; $(pairs...) )
            $x = $(typename)($(twos...))
            $(sets...)
            $x
          end)
    end

Example

    @has aa bb=1 cc=10+1

First, we get the results of the macro

     begin
        type aa # /Users/timm/gits/timm/15/jl/one.jl, line 18:
            bb
            cc
        end
        function aa0() # /Users/timm/gits/timm/15/jl/one.jl, line 21:
            aa(1,10 + 1)
        end
     end

Julia programs are organized
around multiple dispatch, which allows built-in and user-defined
functions to be overloaded for different combinations of argument
types.

    someFun(x::Any) = println(1000000)
    someFun(x::aa)  = println(x.bb)

So if this code rules, we can use `aa0`  to build a type that
auto-assigns some fields to something of type `aa`, and
initialize its fields to `1` and `10+1`.

    x    = aa0()
    x.bb = 200

    someFun(22)
    someFun(x)

Running results:

    1000000
    200
    


## LISP Macros: the Details

### LISP TICK vs BACK TICKS

Back ticks create a toggle mode within which things are not evaluated
unless they are proceeded by a comma.

This allows for the simple definition of nested list structures.

````lisp
(let ((a 1)
      (b 2)
      (c '(10 20 30 40)))
   (print '(a a b b))          ; ==> (A A B B)
   (print `(a ,a b ,b))        ; ==> (A 1 B 2)
   (print `(a ,a b ,b c ,c))   ; ==> (a 1 b 2 c (10 20 30 40))
   (print `(a ,a b ,b c ,@c))) ; ==> (a 1 b 2 c 10 20 30 40)
````

(Note: so back tick is a DSL for specifying nested lists).

For example, the following tells LISP to convert all calls to

````lisp
(time-it 10 (run-this-long-function))
````

with code that runs some slow function ten times, then returns the
mean time times across those ten calls.

````lisp
(defmacro time-it (n &body body)
  "Run 'body' 'n' times."
  (let ((n1 (gensym))
        (i  (gensym))
        (t1 (gensym)))
    `(let ((,n1 ,n)
           (,t1 (get-internal-run-time)))
       (dotimes (,i ,n1) ,@body)
       (float (/ (- (get-internal-run-time) ,t1)
                 (* ,n1 internal-time-units-per-second))))))
````

Pretty neat, heh? Less typing for you, more auto-generated code.

To explain the above, we need to know about _macros_

### What are Macros ?

Macros in Lisp provide a very powerful and flexible method of extending
Lisp syntax.

Macros are programs to write programs. They are called when a
program is first loaded from file into LISP. They expand succinct
expressions into longer, executable, forms.  Since the expansion
happens only once at load time, they have zero runtime cost after that.

Lisp functions take Lisp values as input and return Lisp values.
They are executed at run-time.

Lisp macros take Lisp code as input, and return Lisp code. They are
executed at compiler pre-processor time, just like in C. The resultant
code gets executed at run-time.

### Basic Idea

Macros take unevaluated Lisp code and return a Lisp form. This form
should be code that calculates the proper value. Example:

```lisp
(defmacro Square-1 (X)
    `(* ,X ,X))
````

That is, wherever LISP reads    `(Square-1 XXX)`, 
it replaces it with `(* XXX XXX)`.

The resultant code is what the compiler sees.

### Traps For Beginners

1. Trying to evaluate arguments at compile time
2. Evaluating arguments too many times
3. Variable name capture (not being hygienic).

#### Compile Time Eval Error

Macros are expanded at compiler pre-processor time. So this is an error:

````lisp
(defmacro Square-2 (X)
    (* X X))
````

This would indeed work for `(Square-2 4)`, but would crash for
`(Square-2 X)`, since `X` is probably a variable whose value is not known
until run-time.

### Evaluating arguments too many times

````lisp
(defmacro Square-1 (X) `(* ,X ,X))
````

This looks OK on first blush. However, try macroexpand-1'ing a form,
and you notice that it evaluates its arguments twice:

````lisp
(macroexpand-1 '(Square-1 (Foo 2)))
==> (* (Foo 2) (Foo 2))
````

`Foo` gets called _twice_, but it should only be called once.
Inefficient.

Also, returns the wrong value if Foo does not always return the same
value.

````lisp
(Square-1 (incf X))
(Square-1 (random 10))
````

So, to fix this, we eval the argument once, cache the result, and
use it many times.

````lisp
(defmacro Square-3 (X)
    `(let ((Temp ,X))
     (* Temp Temp)))
````

How does that look?

````lisp
(macroexpand-1 '(Square-3 (Foo 2)))
==> (let ((Temp (Foo 2)))
             (* Temp Temp))
````

Which is nearly what we want.... except for variable name clashes.

### Variable name capture (not being hygienic)

Square-3 is perfectly safe, but consider instead the following macro,
which takes two numbers and squares the sum of them:

````lisp
(defmacro Square-Sum-1 (X Y)
    `(let* ((First ,X)
            (Second ,Y)
        (Sum (+ First Second)))
      (* Sum Sum)) )
````

This looks pretty good, even after macroexpansion:

````lisp
(macroexpand-1 '(Square-Sum-1 3 4))

==> (LET* ((FIRST 3)
           (SECOND 4)
           (SUM (+ FIRST SECOND)))
      (* SUM SUM))
````

which seems ok. BUT the local variables we chose would conflict with
existing local variable names if a variable named First already
existed.  E.g.

````lisp
(macroexpand-1 '(Square-Sum-1 1 First))
==> (LET* ((FIRST 1)
           (SECOND FIRST)
           (SUM (+ FIRST SECOND)))
      (* SUM SUM))
````

The problem here is that `(SECOND FIRST)` gets the value of the new
local variable `FIRST`, not the one you passed in. Thus

````lisp
(let ((First 9)) (Square-Sum-1 1 First))
````

returns 4, not 100!

Solution: need to create a variable name inside the macro that
cannot exist anywhere else in the code. Now the macro is "hygienic".

````lisp
(defmacro Square-Sum (X Y)
    (let ((First (gensym "FIRST-"))
          (Second (gensym "SECOND-"))
          (Sum (gensym "SUM-")))
      '(let* ((,First ,X)
              (,Second ,Y)
              (,Sum (+ ,First ,Second)))
           (* ,Sum ,Sum))
    ))
````

Now

````lisp
(macroexpand-1 '(Square-Sum 1 First))
==> (LET* ((#:FIRST-590 1)
           (#:SECOND-591 FIRST)
           (#:SUM-592 (+ #:FIRST-590 #:SECOND-591)))
       (* #:SUM-592 #:SUM-592))
````

This expansion has no dependence on any local variable names in the
macro definition itself, and since the generated ones are guaranteed to
be unique, is safe from name collisions.

Just to complete the picture, there is the "real" definition of
square:

````lisp
(defmacro square (x)
    (let ((temp (gensym)))
    `(let ((,temp ,x))
````

## Other Examples

### While

````lisp
(defmacro while (test &body body)
  "implements 'while' (which is not standard in LISP)"
  `(do ()
       ((not ,test))
     ,@body))
````

Expansion:

````lisp
(macroexpand-1 '(while (> (decf n) 0) 
                  (print n)))

(DO NIL 
    ((NOT (> (DECF N) 0))) 
       (PRINT N))
````

### Until (defined using While)

````lisp
(defmacro until (test &body body)
  "implements 'until' (which is not standard in LISP)"
  `(while (not ,test)
     ,@body))
````

Expansion:

````lisp
(macroexpand-1 '(until (> (decf n) 0) 
                  (print n)))

(WHILE (NOT (> (DECF N) 0)) 
       (PRINT N)) 
````


### Nested Slot Access

Example of a recurisve macros. Very slick.

Fixes a problem in LISP, not chains of "`.`" to handle nested
object attributes:

````lisp
(defmacro ? (obj first-slot &rest more-slots)
  "From https://goo.gl/dqnmvH:"
  (if (null more-slots)
      `(slot-value ,obj ',first-slot)
      `(? (slot-value ,obj ',first-slot) ,@more-slots)))
````

Expansion

````lisp
(macroexpand '(? obj a b c d))

(SLOT-VALUE 
  (SLOT-VALUE 
    (SLOT-VALUE 
      (SLOT-VALUE OBJ 'A) 
      'B) 
    'C) 
  'D)
````

### Looping

Loop 0..9, return n:

````lisp
(macroexpand '(dotimes (i 10 n) (Incf n i)))

(BLOCK NIL
  (LET ((I 0))
    (DECLARE (TYPE UNSIGNED-BYTE I))
    (TAGBODY
      (GO #:G620)
     #:G619
      (TAGBODY (INCF N I))
      (PSETQ I (1+ I))
     #:G620
      (UNLESS (>= I 10) (GO #:G619))
      (RETURN-FROM NIL (PROGN N)))))
````

Aren't you glad that you've never seen this before and you'll never
have to see it again?

### Anaphoric Macros


This one is really famous. Can you guess what it does?

````lisp
(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))
````

From Graham's _On LISP_ book:

In natural language, an anaphor is an expression which refers back
in the conversation. The most common anaphor in English is probably
"it," as in "Get the wrench and put it on the table." Anaphora are a
great convenience in everyday language-imagine trying to get along
without them-but they don't appear much in programming languages. For
the most part, this is good. Anaphoric expressions are often genuinely
ambiguous, and present-day programming languages are not designed to
handle ambiguity.

However, it is possible to introduce a very limited form of anaphora
into Lisp programs without causing ambiguity. An anaphor, it turns out,
is a lot like a captured symbol. We can use anaphora in programs by
designating certain symbols to serve as pronouns, and then writing
macros intentionally to capture these symbols.

Example use:

````lisp
(aif (big-long-calculation)
     (foo it))
````

When you use an aif, the symbol _it_ is bound to the result returned
by the test clause. This can be reused without repeating some long
expensive test.

### Timing Execution

Now we can explain the _time-it_ code that started this lecture.

````lisp
(defmacro time-it (n &body body)
  "Run 'body' 'n' times."
  (let ((n1 (gensym)) ; hygiene
        (i  (gensym)) ; hygiene
        (t1 (gensym))) ; hygiene
    `(let ((,n1 ,n)
           (,t1 (get-internal-run-time)))
       (dotimes (,i ,n1) ,@body)
       (float (/ (- (get-internal-run-time) ,t1)
                 (* ,n1 internal-time-units-per-second))))))
````

Compile-time evaluation is avoided by returning a  list that defines
a let environment inside of which we can run or code.

Extra evaluations are avoided by computing _n_ once and caching that
result in _n1_.

And the gensyms avoid variable name clashes.
