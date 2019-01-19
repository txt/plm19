<a href="http://tiny.cc/plm19">home</a> ::
<a href="https://github.com/txt/plm19/blob/master/doc/syllabus.md">syllabus</a> ::
<a href="https://github.com/txt/plm19/tree/master/src">src</a> ::
<a href="http://tiny.cc/plm19give">submit</a> ::
<a href="https://plm19.slack.com/">chat</a> ::
<a href="https://github.com/txt/plm19/blob/master/license.md">&copy</a> 2019, <a href="http://menzies.us">timm</a>
<br>
<a href="http://tiny.cc/plm19"><img width=900 src="https://raw.githubusercontent.com/txt/plm19/master/etc/img/banner.png"></a>


# Logic Programming II

## Pipes in Prolog

In Python

```prolog
first( sort( weight( words )))

```
In unix

```prolog
cat words | weight | sort | first
```

In Prolog:

```prolog

longest --> weight, sort, first.       

weight  -->  maplist(long).                                                           

long(Word, N / Word) :- atom_length(Word,M), N is -1*M.               

first([ _/X | _ ], X).                                              

:- longest([names,age,shoesize], X)
X = shoesize
```

## How does this work?

`X --> Y` is a _macro_ that adds the magic I/) variables:

```prolog
:- listing(longest).

longest(A, D) :-
        weight(A, B),
        sort(B, C),
        first(C, D).
```

`maplist` is a _higher-order functions_  that _iterates_  over a list, collecting the result:

```prolog
:- listing(weight).
weight(A, B) :-
        maplist(long, A, B).
```

Other details:

- `is` is the Prolog maths predicate
- `atom_length` is a built-in
- In Prolog, don't write a program. Instead, draw it (see `first`).

## Prolog Bits and Pieces

Clauses have a head, neck and body.

A rule has a head and body.

```prolog
longest(A, D) :-
        weight(A, B),
        sort(B, C),
        first(C, D).
```

A head is true if Prolog can prove its body.

`true` is proved by definition.

A fact only has `true` in the body so Prolog lets us write it as shorthand
(no neck or body).

```prolog
fact(emp,tim,      [job=cleaner,mother=president]).
fact(emp,jane,     [job=president]).
fact(job,cleaner,  [salary=10000]).
fact(job,professor,[salary=30000]).
```

A normal rule has `:-` as a nect. A grammar rule has `-->` as a neck.

```prolog
sentence --> subject, verb, object.

subject --> modifier, noun.

object --> modifier, noun.

modifier --> [the].

noun --> [cat].
noun --> [mouse].
noun --> [polar, bear].

verb --> [chases].
verb --> [eats].
```

Note we call `cat, mouse, ...` etc _terminals_ sicne they cannot be rewritten
and `sentence, subject` etc _nonterminals_ (since they can be rewritten).

Which of course looks like this internally:

```prolog
:- listing(sentence).
sentence(A, D) :-
        subject(A, B),
        verb(B, C),
        object(C, D).

?- listing(noun).
noun([cat|A], A).
noun([mouse|A], A).
noun([polar, bear|A], A).
```

which lets us generate sentences:

```prolog
:- sentence(X, []).
X = [the, cat, chases, the, cat] ;
X = [the, cat, chases, the, mouse] ;
X = [the, cat, chases, the, polar, bear] ;
X = [the, cat, eats, the, cat] ;
X = [the, cat, eats, the, mouse] ;
X = [the, cat, eats, the, polar, bear] ;
X = [the, mouse, chases, the, cat] ;
X = [the, mouse, chases, the, mouse] ;
X = [the, mouse, chases, the, polar, bear] ;
X = [the, mouse, eats, the, cat] ;
X = [the, mouse, eats, the, mouse] ;
X = [the, mouse, eats, the, polar, bear] ;
X = [the, polar, bear, chases, the, cat] ;
X = [the, polar, bear, chases, the, mouse] ;
X = [the, polar, bear, chases, the, polar, bear] ;
X = [the, polar, bear, eats, the, cat] ;
X = [the, polar, bear, eats, the, mouse] ;
X = [the, polar, bear, eats, the, polar, bear].
```
