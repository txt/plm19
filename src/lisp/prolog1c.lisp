;  vim: set filetype=lisp tabstop=2 shiftwidth=2 expandtab : 

#|

Help, my Prolog is broken. At the end of this file is a lisp
function called (test1) that runs when this file loads. e.g.

     clisp prolog1c.lisp

This should print out

      DONALD is the father of DEBBIE
      DONALD is the father of NANCY
      DEBBIE is the sibling of NANCY.
      NANCY is the sibling of DEBBIE.
      [1]
      ?x in chain1 matches to 1.
      ?x in chain2 matches to 1.
      ?x in chain4 matches to 1.

Sadly, it is does not cause some things are broken. Please fix!

First, some initial questions:

11111111111111111111111111111111111111111111111111
Part 1 is worth 1 mark. 0.5 marks for getting 7/10 
of the following right. 1 mark for getting 10/10

1. Write a file prolog1c.txt that answers the following questions.

1a. In LISP what is an association list?

1b. What does the function `assoc` do:

      (assoc 'r '((a . b) (c . d) 
                  (r . x) (s . y) 
                  (r . z))) 

1c. What does the LISP 
[mapcan](http://jtra.cz/stuff/lisp/sclr/mapcan.html)
function do?  Gove am example of its use.

1d. Give a small example of using LISP hash tables to (1) crete a
hash table then (2) write something into that hash table then (3)
read that value back.

1e What does the LISP "sublis" function do? Give
an example.

1f. In Prolog, what is the role of the bindings variable "binds".

1g. There seems to be a missing function. The examples shown below
use an `(= ?x ?x)` rule but there is no support code anywhere else
for `=`. So how does `(= ?x ?x)` work?

1h. What does "(gensym "?")" do?

1i. The following rules illustrates the need for variable renaming.
Why is such renaming required? What could go wrong (with the 
?x and ?y bindings) as our Prolog does inference over these two 
rules.

     (<- (child ?x ?y) (parent ?y ?x))
     (<- (daughter ?y ?x) (and (child ?y ?x) (female ?y)))

1j. (HARD) The code for "prove" that handles conjunctions seem wrong.  Why
does the "and" clause in "prove" use "reverse"? Write a comment in
the "ands" function that explains why we needed that reverse.

22222222222222222222222222222222222222222222222222222
Part 2 is worth 1 mark

2a. The "(known x bindings)" function is missing. This is a function
that accepts a symbol "a" and list of dotted pairs.  While "a" can
be found in the car of any list then we set "a", to the cdr of that
list.  Once that stops recursing, we return the binding "a".
Otherwise, we return nil.  For example:

  (KNOWN '?X
    '((#:?3044 . DEBBIE) (#:?3045 . DONALD) 
      (?Y . #:?3044) (?X . #:?3045))) ==> DONALD

  (KNOWN '?Y
    '((#:?3044 . DEBBIE) (#:?3045 . DONALD) 
      (?Y . #:?3044) (?X . #:?3045))) ==> DEBBIE

  (KNOWN '?X
     '((#:?3066 . 1) (#:?3063 . 1) (#:?3065 . 1) 
       (#:?3064 . 1) (#:?3061 . #:?3064)
       (#:?3062 . 1) (?X . #:?3061))) ==> 1

2b. Another missing function is "(has-vars lst)" that
recursively explores "lst" looking for any symbol that starts
with "?" (see the "varp" function, below). Please implement:

   (HAS-VARS '(AND (PARENT ?X ?Y) (MALE ?X))) ==> (?Y ?X)

(Note that the order of the symbols in the output list does
not matter. But there can be **no** repeats).

33333333333333333333333333333333333333333333333333333
Part 3 is worth 1 mark

3a. The code "(do (show ?c))" crashes. Fix it such that "(do (show
?c))" prints the current binding to ?c, followed be a new line.
Hint: add an extra case into "prove".

3b. The prove function is missing anything that handles numeric
comparisons. So tests like (> ?c x) crashes. Please add code to
handle the maths functions ">=,>,<,<=".
Hint: this should be easy, once you've done (3).

3c. Please fix  definition of sibling such that a person cannot be
their own siblings. So the following output is wrong:

     DEBBIE is the sibling of DEBBIE.
     NANCY is the sibling of NANCY.

Hint: this a logic error, not an interpreter error. So you only
need to fix something inside `data0`.

|#

(defvar *rules* (make-hash-table))

(defmacro <- (con &optional ant)
  `(length
     (push (cons (cdr ',con) ',ant)
           (gethash (car ',con) *rules*))))

(defun data0 ()
  (clrhash *rules*)
  (<- (= ?x ?x))
  (<- (parent donald nancy))
  (<- (parent donald debbie))
  (<- (male donald))
  (<- (chain1 ?a ?b)
         (and (= ?a ?b)
              (= ?b ?c)
              (do (show ?c))
              (= ?c 1)))
  (<- (chain2 ?a ?b)
         (and (= ?a ?b)
              (= ?b ?c)
              (>  ?c 0.3)))
  (<- (chain3 ?a ?b)
         (and (= ?a ?b)
              (= ?b ?c)
              (> ?c 3)))
  (<- (chain4 ?a ?b)
         (and (= ?a ?b)
              (= ?b ?c)
              (not (> ?c 3))
              (= ?c 1)))
  (<- (father ?x ?y) 
      (and 
        (parent ?x ?y) 
        (male ?x)))
  (<- (sibling ?x ?y) 
      (and (parent ?z ?x)
           (parent ?z ?y))))


;--------- --------- --------- --------- --------- --------- ---------
(defun test1 ()
  (data0)
  (query 
    (father ?x ?y)
    (format t "~A is the father of ~A~%" ?x ?y))
  (query 
    (sibling ?x ?y)
    (format t "~A is the sibling of ~A.~%" ?x ?y))
  (query 
    (chain1 ?x 1)
    (format t "?x in chain1 matches to ~A.~%" ?x))
  (query 
    (chain2 ?x 1)
    (format t "?x in chain2 matches to ~A.~%" ?x))
  (query 
    (chain3 ?x 1)
    (format t "?x in chain3 matches to ~A.~%" ?x))
  (query 
    (chain4 ?x 1)
    (format t "?x in chain4 matches to ~A.~%" ?x))
)
    
;--------- --------- --------- --------- --------- --------- ---------
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

(defun var? (x)
  (and (symbolp x) 
       (eql (char (symbol-name x) 0) #\?)))

;; does no occur check cause crash?
;--------- --------- --------- --------- --------- --------- ---------
(defmacro query (question &body body)
  (let ((binds (gensym)))
    `(dolist (,binds (prove ',question))
       (let ,(mapcar (lambda (v)
                         `(,v (known ',v ,binds)))
         (has-vars question))
   ,@body))))

(defun prove (expr &optional binds)
  (case (car expr)
    (and  (ands        (reverse (cdr expr))   binds))
    (or   (ors         (cdr  expr)            binds))
    (not  (negation    (cadr expr)            binds))
    (do   (evals       (cadr expr)            binds))
    (t    (prove1      (car  expr) (cdr expr) binds))))

;--------- --------- --------- --------- --------- --------- ---------
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

(defun evals (expr binds)
  " turns e.g. (print (list ?a ?b)) into
    (let ((?a x) ; where x is computed from (known ?a binds)
          (?b y)); where y is computed from (known ?b binds)
      (print ?a ?b))"
  (labels 
    ((local-vars ()
        (mapcar 
          (lambda (x) 
                 `(,x ',(known x binds))) 
             (has-vars expr))))
    (eval `(let ,(local-vars) 
              ,expr))
    (list binds)))

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

;--------- --------- --------- --------- --------- --------- ---------

(defun renames (r)
  (sublis (mapcar (lambda (v) (cons v (gensym "?")))
                  (has-vars r))
          r))


(test1)
