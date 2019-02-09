


<a href="http://tiny.cc/plm19"><img width=1000 src="https://raw.githubusercontent.com/txt/plm19/master/etc/img/banner.png"></a><br>
&nbsp;<a href="http://tiny.cc/plm19">home</a> |
<a href="https://github.com/txt/plm19/blob/master/doc/syllabus.md">syllabus</a> |
<a href="https://github.com/txt/plm19/tree/master/src">src</a> |
<a href="http://tiny.cc/plm19give">submit</a> |
<a href="https://plm19.slack.com/">chat</a> |
<a href="https://github.com/txt/plm19/blob/master/LICENSE.md">&copy;2019</a> 
by <a href="http://menzies.us">Tim Menzies</a>


# Review, week2

## Things to know

### Prolog:

- immutable variables (once a=1 then it cannot be 2)
- backtracking 
- connection of Prolog to SQL

### Parsing:

- pre- post- in- order traversal
- prefix, infix, postfix
- how, as a human, to convert infix to (e.g.) postfix
      - write down parse tree
      - perform a  (say) post-order traversal, writing down the notes you visit
- how, as a computer, to convert infix to something you can evaluate
      - shunting yard (infix --> postfix)
      - postfix --> tree
      - recursively evaluate tree


### Grammars:

- rewrite rules:
- things that can be rewritten are non-terminals.
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

Grammar s can be used to code precedence's, without explicit numbers. e.g in the following, "/" is "inside "+"
  Also, "/" is left associative.

```
expr --> term,
expr -->  expr, [ - ], term.
term --> const.
term --> term, [/], const.
```



## Macros

_**Intent**_ : transform input text into a different before using templates.

_**Problem**_ : 

- you want to think about the problem one way, but the program wants you to do it another.
- you keep doing the same darn thing, over and over again

_Structure_ : Some input form to  be expanded (supplied by the end programmer) and some template used
to do the expansion (supplied by the language author).
Macros bring syntactic abstraction, which is a wonderful thing. It helps
make your code clearer, by describing your intent without getting bogged
in implementation details (indeed abstracting those details away). It
helps make your code more concise and more readable, by eliminating
both redundancy and irrelevant details. 

_**Examples**_ 

e.g. Macro expansion in C (useful for simple in-lining of one-line functions).

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

Note: older languages used macros extensively. Modern languages, not so much. But maybe the tide is turn (very new languages have macros, again, elixr, julia).

##  Production Systems

_**Intent**_ Separate the logic from the program. 

_**Problem**_ : High level knowledge, encoded into (e.g.) "C" becomes
convoluted, verbose, hard to maintain.

_**Structure**_: 

- A working memory of assertions
- An explicit high-level rule-based notation (recall proj1a)
  that matches to the working memory and, maybe, does updates to that memory.
- A separate inference engine that does 
     - Match (find the N rules with satisfied conditions)
     - Select (finds the best rule to use)
     - Act (applies that rule)
     - And repeat till no more rules found.

(Reminds you of macros? But here we are _committing_ to the match-select-act semantics).

Another way to say the above is "look before you leap". THink what you _could_ do before
deciding what you _should_ do.

Note that _select_ is also called "conflict resolution"; i.e. how to handle the case
where multiple rules  want to change the working memory in different ways.
Standard conflict resolution operators:

- Recentcy: favor rules that match to the most recent changes to working memory
- Precedence: give each rule an explicitly precedence and select higher precedence ones
   over lower precedence ones.
- Specificity: if tow rules can fir, use the most complex one.
- Roll your own



_**Examples**_

So many business logic languages.

See your proj1a rules.

_**Rules of thumb**_

- dozens to 100s of rules are good: maintainable knowledge
- 1000s of rules are bad: maintenance nightmare
- Think rule groups: objects containing small lists of rules that are about
  just their content
      - So find some way to divide very big rule sets into dozens of small to medium rule sets.
- 80% of the time (or more) in a production system spent in match
      - Solution 1: clever cross compilation of rules into sets of related tests
      - Solution 2: rule groups.
- Simple uniform semantics makes rules a candidate for an end-user language
  (something they can read and understand and critique, and even write).
- Simple uniform semantics makes rules an excellent candidate for tool support
     - e,g, writing "checkers" to find sill errors.
- Simple uniform semantics makes rules hard to express certain procedural constructs

_**See also**_

- compartmental models
- state machines
- decision tables
