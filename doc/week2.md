
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

_**Rules of thumb**_

- Simple macros good for simple things.
- Very quickly get messy for larger things.
- Syntactic only, cannot access semantics of the underlying language.
- Google says Use macros when appropriate, which is often. 
- Google says Define macros when appropriate, which is seldom.
- But it comes at a cost to the
reader, which is learning a new syntactic concept for each macro. And
so it should not be abused.

Note: older languages used macros extensively. Modern languages, not so much. But maybe the tibe is turn (very new languages have macros, again, elixr, julia).

##  Production Systems

_**Intent**_ Seperate the logic from the program. 

_**Problem**_ : High level knowlege, encoded into (e.g.) "C" becomes
convulated, verbose, hard to maintain.

_**Structure**_: 

- A working memroy of assertions
- An explicit high-level rule-based notation (recall proj1a)
  that matches to the working memory and, maybe, does updates to that memory.
- A seperate inference engine that does 
     - Match (find the N rules with satisfied conditions)
     - Select (finds the best rule to use)
     - Act (applies that rule)
     - And repeat till no more rules found.

(Reminds you of macros? But here we are _committing_ to the match-select-act semantics).

Another way to say the above is "look before you leap". THink what you _could_ do before
deciing what you _should_ do.

Note that _select_ is also called "conflect resolution"; i.e. how to handle the case
where multiple rules  want to change the working memory in different ways.
Standard conflict resolution opertors:

- Recency: facor rules that match to the most recent changes to working memory
- Precedence: give each rule an explicity precedence and select hogher precendence ones
   over lower precedence ones.
- Specificity: if tow rules can fir, use the msot complex one.
- Roll your own



_**Examples**_

So many business logic languages.

See your proj1a rules.

_**Rules of thumb**_

- dozens to 100s of rules are good: maintainable knowledge
- 1000s of rules are bad: maintainence nightmare
- Think rule groups: objects containing small lists of rules that are about
  just theor content
      - So find some way to divide very big rule sets into doznes of small to medium rule sets.
- 80% of the time (or more) in a production system spent in match
      - Solution 1: clever cross comialtion of rules into sets of related tests
      - Solution 2: rule groups.
- Simple uniform semantics makes rules a candidate for an end-user language
  (something they can read and understand and critique, and even write).
- Simple uniform semantics makes rules an excellent candidate for tool support
     - e,g, writing "checkers" to find sillt errors.
- Simple uniform semantics makes rules hard to express certain procedural constructs

_**See also**_

- compartmental models
- state machines
- decision tables