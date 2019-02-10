


<a href="http://tiny.cc/plm19"><img width=1000 src="https://raw.githubusercontent.com/txt/plm19/master/etc/img/banner.png"></a><br>
&nbsp;<a href="http://tiny.cc/plm19">home</a> |
<a href="https://github.com/txt/plm19/blob/master/doc/syllabus.md">syllabus</a> |
<a href="https://github.com/txt/plm19/tree/master/src">src</a> |
<a href="http://tiny.cc/plm19give">submit</a> |
<a href="https://plm19.slack.com/">chat</a> |
<a href="https://github.com/txt/plm19/blob/master/LICENSE.md">&copy;2019</a> 
by <a href="http://menzies.us">Tim Menzies</a>


# Prolog (ish) in LISP

[Prolog-in-LISP](../src/lisp/prolog1c.lisp) is a LISP program that
lets us better understand our prior work on Prolog while learning
more about LISP.

The system is in two parts:

1. A knowledge-base;
2. An inference engine.

## Knowledge-Base

_Facts_ are
list with  a predicate followed by zero or more arguments; e.g.

    (parent donald nancy)

tells us that _donald_'s parent is _nancy_.  _Rules_ tell us what we can infer:

    (<- head body)

where _head_ is the "consequence" and "body" is the "antesecendat".

_Variables_ are symbols beginning with question marks; e.g. `?x`:

```lisp
(defun var? (x)
  (and (symbolp x) 
       (eql (char (symbol-name x) 0) #\?)))
```

For example, 

    (<- (child ?x ?y) (parent ?y ?x))

says that:

- if y is the parent of x, then x is the child of y

or more precisely, that we can prove any fact of the form 
_(child x v)_ by proving _(parent y x)_. Bodies
can contain
complex expressions (and, or, not) e.g.

    (<- (father ?x ?y) (and (parent ?x ?y) (male ?x)))

Bodies can mention facts or rules (recursively); e.g. 

    (<- (daughter ?x ?y) (and (child ?x ?y) (female ?x)))

Proving something means chaining back through rules until we terminte at facts.
Each step of that requires the _unification_ of two lists
(which might have variables).
This means search for _bindings_ to variables that makes the two lists equal.
For example, to _unify_ two lists:

    (p ?x ?y c ?x) 
    (p a b c a)

then use `?x = a` and `?y = b`, and the lists

    (p ?x b ?y a) 
    (p ?y b c a)

unify  when `?x = ?y = c`. 

## Interpreting the Knowledge

### Unify

The function `unify` takes two tries
and returns nil (if unification is imposible) or an association list containing
the required bindings.

```lisp
(defun unify (x y &optional binds)
  (cond 
    ((eql x y)        (values binds t))
    ((assoc x binds)  (unify (known x binds) y binds))
    ((assoc y binds)  (unify x (known y binds) binds))
    ((var? x)         (values (cons (cons x y) binds) t))
    ((var? y)         (values (cons (cons y x) binds) t))
    (t
      (when (and (consp x) (consp y))
        (multiple-value-bind (b2 yes) 
          (unify (car x) (car y) binds)
          (and yes (unify (cdr x) (cdr y) b2)))))))
```

where the `known` function looks up bindings within the `binds` association list.

There are six cases within `(unify x y)`:

1. If x and y are eql they unify. Else:
2. If x has a binding, then try unifying that binding to y. Else:
3. If y has a binding, then try unifying that binding to x. Else:
4. If x is a variable (without a binding), then it can unify to anything at all,
   so we just create a new binding to y. Else:
5. If y is a variable (without a binding), then it can unify to anything at all,
   so we just create a new binding to x. Else:
6. Else, if x and y are list then if their car unifies, try unifying the cdrs.

Here are some examples of _unify_ in action:


     > (unify '(p  a  b c  a) 
              '(p ?x ?y c ?x)) 
     ==> ((?Y . B) (?X . A))
     T
     
     > (unify '(p ?x b ?y a) 
              '(p ?y b  c a))
     ((?Y . C) (?X . ?Y))
     T
     
     > (unify '(a b c) 
              '(a a a)) 
     NIL

Since not all successful unifications generate any bindings, `unify`, like `gethash`, 
returns a second value to show that the unify succeeded:


     > (unify ' (p ?x) »(p ?x)) 
     NIL
     T

Now we can make our little Prolog work.  For example, if we have
just the fact

     (parent donald nancy)

and we ask the program to prove "(parent ?x ?y)" it should return
something like

    (((?x . donald) (?y . nancy)))

which says that there is exactly one way for the expression to be
true: if ?x is donald and ?y is nancy.

## Defining Knowledge

The following macro defines a macro for writing rules and storing them
into the `*rules*` hash table (indexed by the predicated in the head... so
predicates can't be variables):

```lisp
(defvar *rules* (make-hash-table))

(defmacro <- (con &optional ant)
  `(length
     (push (cons (cdr ',con) ',ant)
           (gethash (car ',con) *rules*))))
```

This macro defines both facts and rules:

+  Facts are  rules with a head but no body. I.e.
   to prove this head, there is nothing else to do (so nothing is needed for
   the body).

For example

     > (<- (parent donald nancy)) ; this is a fact
     1
      
     > (<- (child ?x ?y) (parent ?y ?x)) ; this is a rule
     1

Note that "<-" returns a number (using "length") just to stop a high-level
form returning a tediously long list.

## Answering Queries

The `prove` function peeks at the expr and calls different sub-provers
for conjuctnion, disjuction, negation and basic unifications.

```lisp
(defun prove (expr &optional binds)
  (case (car expr)
    (and  (ands        (reverse (cdr expr))   binds))
    (or   (ors         (cdr  expr)            binds))
    (not  (negation    (cadr expr)            binds))
    (t    (prove1      (car  expr) (cdr expr) binds))))

(defun prove1 (pred args binds)
  (mapcan 
    (lambda (r)
        (multiple-value-bind (b2 yes) 
          (unify args (car r) 
                 binds)
          (when yes
            (if (cdr r)  
              (prove (cdr r) b2) 
              (list b2)))))
    (mapcar #'renames
            (gethash pred *rules*))))

(defun renames (r)
  (sublis (mapcar (lambda (v) (cons v (gensym "?")))
                  (has-vars r))
          r))
```

For each head that unifies, it calls "prove" on the body, with the
new bindings generated by the unification.  The lists of bindings
returned by each call are then collected by mapcan and returned:

     > (prove-simple 'parent (donald nancy) nil) 
     (NIL)

     > (prove-simple 'child ' (?x ?y) nil )
     (((#:?6 . NANCY) (#:?5 . DONALD) 
       (?Y   . #:?5)  (?X   . #:?6)))

Both of the return values above indicate that there is one way to
prove what we asked about. (A failed proof would return nil.) The
first example generated one empty set of bindings, and the second
generated one set of bindings in which ?x and ?y were (indirectly)
bound to nancy and donald.

Incidentally, we see here a good example of the point made on page
23. Because our program is written in a functional style, we can
test each function interactively.

What about those gensyms in the second return value? If we are going
to use rules containing variables, we need to avoid the possibility
of two rules accidentally containing the same variable. If we define
two rules as follows

     (<- (child ?x ?y) (parent ?y ?x))
     (<- (daughter ?y ?x) (and (child ?y ?x) (female ?y)))
     
then we mean that for any x and y, x is the child of y if y is the
parent of x, and for any x and y, y is the daughter of x if y is
the child of x and female. The relationship of the variables within
each rule is significant, but the fact that the two rules happen
to use the same variables is entirely coincidental.

If we used these rules as written, they would not work that way.
If we tried to prove that a was b's daughter, unifying against the
head of the second rule would leave ?y bound to a and ?x to b. We
could not then unify the head of the first rule with these bindings:

     > (unify '(child ?y ?x) '(child ?x ?y)
     '((?y . a) (?x . b)))
     NIL

To ensure that the variables in a rule imply only something about
the relations of arguments within that rule, we replace all the
variables in a rule with gensyms. This is the purpose of the function
change-vars. A gensym could not possibly turn up as a variable in
another rule. But because rules can be recursive, we also have to
guard against the possibility of a rule clashing with itself, so
change-vars has to be called not just when a rule is defined, but
each time it is used.

```lisp
(defun ands (goals binds)
  (if (null goals)
      (list binds)
      (mapcan (lambda (b)
                  (prove (car goals) b))
              (ands (cdr goals) binds))))

(defun ors(goals binds)
  (mapcan (lambda (c) (prove c binds))
          goals))

(defun negation (goal binds)
  (unless (prove goal binds)
    (list binds)))
```

```lisp
(defmacro query (question &body body)
  (let ((binds (gensym)))
    `(dolist (,binds (prove ',question))
       (let ,(mapcar (lambda (v)
                         `(,v (known ',v ,binds)))
         (has-vars question))
   (declare (ignorable ,@(has-vars question)))
   ,@body))))
```

Now all that remains is to define the functions that prove complex
expres- sions. These are shown in Figure 15.4. Handling an or or
not expression is particularly simple. In the former case we collect
all the bindings returned by each of the expressions within the or.
In the latter case, we return the current bindings iff the expression
within the not yields none.

The function p r o v e - a n d is only a little more complicated.
It works like a filter, proving the first expression for each set
of bindings that can be established for the remaining expressions.
This would cause the expressions within the and to be considered
in reverse order, except that the call to prove-and within prove
reverses them to compensate.

Now we have a working program, but it's not very user-friendly.
It's a nuisance to have to decipher the lists of bindings returned
by prove—and

     (with-answer (p ?x ?y) (f ?x ?y))

is macroexpanded into:

     (dolist (#:gl (prove (P ?x ?y)))
         (let ((?x (binding '?x #:gD) 
                   (?y (binding ' #:gl)))
        (f ?x ?y)))


they only get longer as the rules get more complex. Figure 15.5
contains a macro that will make our program more pleasant to use:
a with-answer expression will take a query (not evaluated) and a
body of expressions, and will evaluate its body once for each set
of bindings generated by the query, with each pattern variable bound
to the value it has in the bindings.

     > (with-answer (parent ?x ?y)
          (format t "~A is the parent of ~A.~°/0" ?x ?y))
     DONALD is the parent of NANCY. 
     NIL

This macro does the work of deciphering the bindings for us, and
gives us a convenient way of using prove in programs. Figure 15.6
shows what an expansion looks like, and Figure 15.7 shows some
examples of it in use.

It may seem as if the code we've written in this chapter is simply
the natural way to implement such a program. In fact it is grossly
inefficient. What we've done here, essentially, is to write an
interpreter. We could implement the same program as a compiler.

Here is a sketch of how it would be done. The basic idea would be
to pack the whole program into the macros <- and with-answer, and
make them do at macro-expansion time most of the work the program
now does at run-time. (The germ of this idea is visible in avg, on
page 170.) Instead of representing rules as lists, we would represent
them as functions, and instead of having functions like prove and
prove-and to interpret expressions at run-time, we would have
corresponding functions to transform expressions into code. The
expressions are available at the time a rule is defined. Why wait
until it


If we do a (clrhash *rules*) and then define the following rules and facts,

(<- (parent donald nancy))
(<- (parent donald debbie))
(<- (male donald))
(<- (father ?x ?y) (and (parent ?x ?y) (male ?x))) j (<- (= ?x ?x))
(<- (sibling ?x ?y) (and (parent ?z ?x) (parent ?z ?y)
(not (= ?x ?we will be able to make inferences like the following:


> (with-answer (father ?x ?y)

(format t "~A is the father of ~A.~°/. ?x ?y))
DONALD is the father of DEBBIE. DONALD is the father of NANCY. NIL

> (with-answer (sibling ?x ?y)
(format t "~A is the sibling of ~k.~V ?x ?y» DEBBIE is the sibling of NANCY.
NANCY is the sibling of DEBBIE.
! NIL

The program in use.  is used in order to analyze them? The same
goes for with-answer, which would call the same functions as <- to
generate its expansion.

This sounds like it would be a lot more complicated than the program
we wrote in this chapter, but in fact it would probably only be
about two or three times as long. Readers who would like to learn
about such techniques should see On Lisp or Paradigms of Artificial
Intelligence Programming, which contain several examples of programs
written in this style.

