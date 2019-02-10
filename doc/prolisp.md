


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
new bindings generated by the unification.  For example:

     > (prove1 'parent (donald nancy) nil) 
     (NIL)

     > (prove-simple 'child ' (?x ?y) nil )
     (((#:?6 . NANCY) (#:?5 . DONALD) 
       (?Y   . #:?5)  (?X   . #:?6)))

Both of the return values above indicate that there is one way to
prove what we asked about. (A failed proof would return nil.) The
first example generated one empty set of bindings, and the second
generated one set of bindings in which ?x and ?y were (indirectly)
bound to nancy and donald.

If we are going
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
"renames". A gensym could not possibly turn up as a variable in
another rule. But because rules can be recursive, we also have to
guard against the possibility of a rule clashing with itself, so
change-vars has to be called not just when a rule is defined, but
each time it is used.

```lisp
(defun renames (r)
  (sublis (mapcar (lambda (v) (cons v (gensym "?")))
                  (has-vars r))
          r))
```

The sub-provers that handle conjunction/ disjunction/ negation
are shown below. 

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

Note that:

- The disjunction function returns lists of differnt bindings.
- the "negation" function returns nil if the recursive proof fails.

Finally, we need a top-level driver:

```lisp
(defmacro query (question &body body)
  (let ((binds (gensym)))
    `(dolist (,binds (prove ',question))
       (let ,(mapcar (lambda (v)
                         `(,v (known ',v ,binds)))
             (has-vars question))
         ,@body))))
```
(where "has-vars"  returns the variables in the "question").

For example, this:

     (query  (p ?x ?y) (f ?x ?y))

macroexpands into:

     (dolist (#:gl (prove (P ?x ?y)))
         (let ((?x (binding '?x #:gD) 
                   (?y (binding ' #:gl)))
        (f ?x ?y)))

A "query"  expression will take a query (not evaluated) and a
body of expressions, and will evaluate its body once for each set
of bindings generated by the query, with each pattern variable bound
to the value it has in the bindings.

     > (with-answer (parent ?x ?y)
          (format t "~A is the parent of ~A.~°/0" ?x ?y))
     DONALD is the parent of NANCY. 
     NIL



