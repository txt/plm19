
[home](http://tiny.cc/plm19) |
[copyright](https://github.com/txt/plm19/blob/master/license.md) &copy;2019, timm&commat;ieee.org
<br>
<a href="http://tiny.cc/plm19"><img width=900 src="https://raw.githubusercontent.com/txt/plm19/master/etc/img/banner.png"></a>
<br>
[syllabus](https://github.com/txt/plm19/blob/master/doc/syllabus.md) |
[src](https://github.com/txt/plm19/tree/master/src) |
[submit](http://tiny.cc/plm19give) |
[chat](https://plm19.slack.com/)

# LISP: One ring to rule them all.

Give me less than dozen  primitives and I can give you a language more
powerful that Fortran, Cobol,... many modern languages

[Paul Graham, origins of lisp](http://lib.store.yahoo.net/lib/paulgraham/jmc.ps)

- "In 1960, John McCarthy published a remarkable paper in which he did for programming something like what Euclid did for geometry. He showed how, given a handful of simple operators and a notation for functions, you can build a whole programming language. He called this language Lisp, for "List Processing," because one of his key ideas was to use a simple data structure called a list for both code and data."
- "It's worth understanding what McCarthy discovered, not just as a landmark in the history of computers, but as a model for what programming is tending to become in our own time. It seems to me that there have been two really clean, consistent models of programming so far: the C model and the Lisp model. These two seem points of high ground, with swampy lowlands between them. As computers have grown more powerful, the new languages being developed have been moving steadily toward the Lisp model. A popular recipe for new programming languages in the past 20 years has been to take the C model of computing and add to it, piecemeal, parts taken from the Lisp model, like runtime typing and garbage collection."

Let me show you the power of LISP....

![](https://xkcd.com/297/)


## The Primitives Of Lisp

1. Expressions: atom or (zero or more expresions)
2. `(quote x)` returns x
3. `(atom x)` returns `t` (true) if `x` is an atom or empty list
4. `(eq x y)` returns `t` if these are the same atom or both the empty list
5. Make a list using `(cons x y)` returns a list whose head 
   is `x` and whose tail is `y`. <br>
   <img wdith=500 src="http://xahlee.info/comp/i/Guy_Steele_parallel_programing_lisp_cons.png">
6. Inspect a list: `(car x)` first item in a list
7. Inspect a list: `(cdr x)` other items in a list
8. Dont run all code: `(cond (p1 e1) (p2 e2) (p3 e3))`
   run the first `e` with a true `p`.

## Lambda Bodies

(Named after the lambda calculus of Alfonso Church.).

LISP  functions as a list whose:

- first item is the atom `lambda`, 
- second item is a list of `parameter` atoms
- remaining items is an `expression`.

```
((lambda (x)􏰐(cons x '(b)) 'a) ==> (a b)
􏰐
((lambda (x y)􏰏(cons x (cdr y))) 'z '(a b c))􏰐==> (z b c)
```
Which also means I can pass in a lambda to a lambda

```
((lambda 􏰏(f) (f 􏰋􏰏'(b c)))  ; <=== lambda body
 '(lambda (x) (cons 'a x)))  ; <=== 1st arg = lambda
) 
===>
(a b c)
```
􏰐
Lambdas process variables in a `environment` which is, conceptually, a list of cons symbol values

```
'((a 1) (b 1) ...)
```

When we call a lambda body:

- the new variables in the parameter
list are appended **to the left** of the envrionment list.
- When we refer to variables inside a lambda body, we look them up
**from the left**

Now in case you blinked and missed it, you got a full-fledged
programming language. 

## Example1: Functions:
Consider functions:

```
(defun hello (who)
  (format t "hello ~a.~%" who))
```
Internally, we can implement these as a lambda body 
that is stored in the named memory location `hello`.

## Example2: Recursive Functions:

Consider recursive function calls:

- When we call a function recursively, then the variables
for **this level** are most left and the variables for the
calling levels of recursion are on the right
- This means that when we look up a value ina  function,
  we do not mix up the settings from this level with those
  from the outer levels.

## Example3:  Higher-Order Functions

Lambda bodies are just data so I can carry them around a list just like
any other variable.

```
(defun for (start stop f)
  (when (< start stop)       ; 
        (funcall f start)
        (countdown (+ 1 start) stop f)))
```
(If use `when` since standard `if` assumes 3rd arg is the `else` part
while `when` just uses all the parts as `then`.)
```
(for 0 5 #'print)

0
1
2
3
4
```
## Example4: Local Variables

For yet another example, 
the `let` form of LISP defines some local variables; e.g.

```
(let ((x 1)
      (y 2))
   (let ((x 3))
     (print (+ x y))) ==> prints + 3 2 ==> 5
   (print (+ x y))) ==> print + 1 2 ==> 3
```

Internally, we can implement these lets at lambda bodies
such that the method look up inside the let bodies does the right things.
For example:

```
(let ((var1 value1) 
      (var2 value2) 
      ...) 
  body)
```
becomes
```
((lambda (var1 var2 ...) body) value1 value2 ...)
```
## Example5: OO

For yet another another example, consider object-oriented
programming. We can define objects as lists of lambda bodies
(in the following, the `case` function of `new-account`
returns one of the lambda bodies).

```
(defun new-account (name &optional (balance 0.00)
                    (interest-rate .06))
  "Create a new account that knows the following messages:"
  (lambda (message)
      (case message
        (withdraw (lambda (amt)
                      (if (<= amt balance)
                          (decf balance amt)
                          'insufficient-funds)))
        (deposit  (lambda (amt) (incf balance amt)))
        (balance  (lambda () balance))
        (name     (lambda () name))
        (interest (lambda ()
                      (incf balance
                            (* interest-rate balance)))))))
```
In the following

-  `(funcall f m)` ==> f(m) in e.g.  Python. We use this to get the 
   lambda body.
-  `(apply g (x y ...))` ==> g(x, y, ...) in e.g. Python. We use this
   to send the args to the lambda body.

```
(defun send (object message &rest args)
  (apply (funcall object message) args))
````

This lets us defined some convenience functions:
```
(defun withdraw (object &rest args)
  (apply (send object 'withdraw) args))
```
## Macros

Now we add a macro facility such that high-level constructs unwind into
lower-level constructs at load time. The following code is my
protest against the Common Lisp Object System (which is so verbose).
I prefer

```
(isa stuff       ;class
     magic       ;superclass
     (a 1 )      ;slots with defaults
     (b 2) 
     (_cache "tim")))
```
The equivalent in CLOS is the following, which I'm
not going to even to explain to you cause I hate it so much.

```
(defclass stuff (magic)
 ((a :initarg :a :initform 1 :accessor stuff-a)
  (b :initarg :b :initform 2 :accessor stuff-b)
  (_cache :initarg :_cache :initform "tim" :accessor
   stuff-_cache))) 
```
I could make the specification of my classes easier with a
`macro` that turns (e.g.) `(a 1)` into 
 
        (a :initarg :a :initform 1 :accessor stuff-a)

Easily done:

```
(defmacro uses  (slot x form)
    `(,slot
	:initarg  ,(intern (symbol-name slot) "KEYWORD")
	:initform ,form
	:accessor ,(intern (format nil "~a-~a" x slot))))
```
Note the backtick notation. This creates a "toggle environment"
where "x" evalautes to to the symbol "x" but ",x" evaluates
to whatever value "x" points to.

Here's the whole thing where  the `uses` 
macro is now a sub-routine inisde
`isa` (why? cause I don't like polluting the global name space).

```
(defmacro isa (x parent &rest slots)
  "simpler creator for claseses. see example in thingeg.lisp"
  (labels ((uses  
	     (slot x form)
	     `(,slot
		:initarg  ,(intern (symbol-name slot) "KEYWORD")
		:initform ,form
		:accessor ,(intern (format nil "~a-~a" x slot)))))
    `(progn
       (defun ,x  (&rest inits)
	 (apply #'make-instance (cons ',x inits)))
       (defclass ,x (,parent)
	 ,(loop for (slot form) in slots collect (uses slot x form))))))

(macroexpand '(isa stuff magic (a 1 ) (b 2) (_cache "tim")))

(PROGN

  (DEFUN STUFF (&REST INITS)
	 (APPLY #'MAKE-INSTANCE (CONS 'STUFF INITS)))

  (DEFCLASS STUFF (MAGIC)
	    ((A :INITARG :A :INITFORM 1 :ACCESSOR STUFF-A)
	     (B :INITARG :B :INITFORM 2 :ACCESSOR STUFF-B)
	     (_CACHE :INITARG :_CACHE :INITFORM "tim" :ACCESSOR
		     STUFF-_CACHE)))) ;
```

## Design Patterns (e.g. Visitor)

For yet another another example, consider the `visitor` design pattern
which, in OO languages, needs all these new classes. Not so in LISP

```
(defmethod visit ((x t) f)
  (funcall f x ))

(defmethod visit ((x cons) f)
  (if (funp x)
    (funcall f (format nil "(~a)" (second x)))
    (mapcar (lambda(y) (visit y f)) x)))

(defmethod visit ((x magic) f)
  (mapcar (lambda(slot)
	      (visit (slot-value x slot) f))
	  (slots x)))

(visit `(1 2 #'_has "apples" ,(stuff :b 20)) #'print)
```

This outputs:

```
1
2
"(_HAS)"
"apples"
1
20
```
The above uses two magic functions which I include for
completeness but need not bother us too much:

```
(defun funp(x)
   (and (consp x)
         (eql 2 (length x))
         (eql 'function (car x))))

(defmethod slots ((x magic) &optional reveal-all)
  (labels 
    ((hide (y)
	   (and (not reveal-all)
		(and (symbolp y) 
		     (equal (elt (symbol-name y) 0) #\_))))
     (slots1 (y) ; what are the slots of a class?
	     #+clisp (class-slots (class-of y))
	     #+sbcl  (sb-mop:class-slots (class-of y)))
     (name (y) ; what is a slot's name?
	   #+clisp (slot-definition-name y)
	   #+sbcl  (sb-mop:slot-definition-name y)))
    (remove-if #'hide 
	       (mapcar #'name 
		       (slots1 x)))))
```

## Have you drunk the Koolaid?


![](https://imgs.xkcd.com/comics/lisp.jpg)

https://www.youtube.com/watch?v=HM1Zb3xmvMc

## Resources on LISP

The core simplicity of LISP is easier to see it in Norvig's work:

- http://norvig.com/lispy.html
    - LISP in Python
    - Source code: https://github.com/norvig/pytudes/blob/master/py/lis.py

## Clojure: Lisp on the JVM

Java is everywhere, which means that Java Virtual Machines are everywhere. Creating a version of Lisp that runs on the JVM made it possible to run Lisp anywhere. That was the primary motivation for creating Clojure, and a great reason to learn the language.

Another benefit is that Clojure provides access, via the JVM, to countless tools and third-party libraries written in Java. This gives Clojure a development ecosystem that is more powerful than those previously available to any Lisp dialect.

Btw, Clojure tells us what LISP overlooked. It is recommended
to start Clojure projects using the `lien` package manager. If you do that,
the first thing that happens is that `lien` builds you a directory
structure and populates it with some manifest files:

```
$ lein new app my-stuff

Generating a project called my-stuff based on the 'app' template.

$ cd my-stuff
$ find .
.
./.gitignore
./doc
./doc/intro.md
./LICENSE
./project.clj
./README.md
./resources
./src
./src/my_stuff
./src/my_stuff/core.clj
./test
./test/my_stuff
./test/my_stuff/core_test.clj
```
That is, the first thing you do in Clojure is to make your code
sharable with the international programming community. And that's
the final primitive needed to complete LISP- people to code with.
 