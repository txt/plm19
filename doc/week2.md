
[home](http://tiny.cc/plm19) |
[copyright](https://github.com/txt/plm19/blob/master/license.md) &copy;2019, timm&commat;ieee.org
<br>
<a href="http://tiny.cc/plm19"><img width=900 src="https://raw.githubusercontent.com/txt/plm19/master/etc/img/banner.png"></a>
<br>
[syllabus](https://github.com/txt/plm19/blob/master/doc/syllabus.md) |
[src](https://github.com/txt/plm19/tree/master/src) |
[submit](http://tiny.cc/plm19give) |
[chat](https://plm19.slack.com/)

# Review, week2

## Things to know

### Prolog:

- immutable variables (once a=1 then it cannot be 2)
- backtracking 
- connection of Prolog to SQL

### Parsing:

- pre- post- in- order traversal
- prefix, infix, postix
- how, as a human, to convert infix to (e.g.) postfix
      - write down parse tree
      - perform a  (say) post-order traversal, writing down the notes you visit
- how, as a computer, to convert infix to somethign you can evalaute
      - shunting yard (infix --> postfix)
      - postfix --> tree
      - recursively evaluate tree


### Grammers:

- rewrite rules:
- thigns that can be rewritten are non-terminals.
- things that can't are terminals.


e.g. grammars in OCAML syntax. What is it describing?

```
type paragraph =
      Normal of par_text
    | Pre of string * string option
    | Heading of int * par_text
    | Quote of paragraph list
    | Ulist of paragraph list * paragraph list list
    | Olist of paragraph list * paragraph list list
  
  and par_text = text list
  
  and text =
      Text of string
    | Emph of string
    | Bold of string
    | Struck of par_text
    | Code of string
    | Link of href
    | Anchor of string
    | Image of img_ref
  
  and href = { href_target : string; href_desc : string; }
  
  and img_ref = { img_src : string; img_alt : string; }
```

Grammer s can be used to code precences, without explicit numbers. e.g in the following, "/" is "inside "+"
  Also, "/" is left associative.

```
expr --> term,
expr -->  expr, [ - ], term.
term --> const.
term --> term, [/], const.
```



## Macros

_**Intent**_ : transform inout text into a differen before using templates.

_**Problem**_ : 

- you want to think about the problem one way, but the program wants you to do it another.
- you keep doing the sam darn thing, over and over again

_Structure_ : Some input form to  be expanded (supplied by the end programmer) and some tempate used
to do the expansion (supplied by the language author).
Macros bring syntactic abstraction, which is a wonderful thing. It helps
make your code clearer, by describing your intent without getting bogged
in implementation details (indeed abstracting those details away). It
helps make your code more concise and more readable, by eliminating
both redundancy and irrelevant details. 

_**Examples**_ 

e.g. Macro exapnsion in C (usieful for simple in-lining of one-line functions).

e.g. in Prolog, the "-->" rules are macroed as follows.
Recall that this is used to do "pipes" in Prolog where the output of one thing becomes the input to another.

```
expr --> expr,[-], term.

% internally is automatically converted to...

expr(A, D) :-
        expr(A, B),
        B=[-|C],
        term(C, D).
```


e.g. in LISP:.

```
(macroexpand-1 '(dolist (x lst) (print x)))

(DO* ((#:LIST-3234 LST (CDR #:LIST-3234)) 
      (X NIL)) 
   ((ENDP #:LIST-3234) NIL)
    (DECLARE (LIST #:LIST-3234)) 
    (SETQ X (CAR #:LIST-3234)) 
    (PRINT X)) 
```

Of course  `do*` is itself a macro that expands into some gotos:

```
(BLOCK NIL
 (LET* ((#:LIST-3230 LST) 
        (X NIL)) 
   (DECLARE (LIST #:LIST-3230))
   (TAGBODY #:LOOP-3231 
      (IF (ENDP #:LIST-3230) (GO #:END-3232))
      (SETQ X (CAR #:LIST-3230)) 
      (PRINT X) 
      (SETQ #:LIST-3230 (CDR #:LIST-3230))
      (GO #:LOOP-3231) 
      #:END-3232 (RETURN-FROM NIL (PROGN NIL))))) 
```

**Rules of thumb**

- Simple macros good for simple things.
- Very quickly get messy for larger things.
- Syntactic only, cannot access semantics of the underlying language.
- Google says Use macros when appropriate, which is often. 
- Google says Define macros when appropriate, which is seldom.
- But it comes at a cost to the
reader, which is learning a new syntactic concept for each macro. And
so it should not be abused.

Note: older languages used macros extensively. Modern languages, not so much (exceptions: elixr, julia).

##  Production Systems
 

Rule of Representation
 
 Developers should choose to make data more complicated rather than the procedural logic of theprogram when faced with the choice, because it is easier for humans to understand complex data compared with complex logic. This rule aims to make programs more readable for any developer working on the project, which allows the program to be maintained.

simplue unifrm sematics: tools support = easier user trainign easier

some claim fits better with himan metnal models. but...

See also
compartmental models
state machines
decsion table

match 80%

select : confect resolution recency, prioirt, specificity

act
