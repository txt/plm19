# Logic Programming I

The language Prolog is the Fortran of logic programming: it was the
first of its kind, it was much criticized, and it is still widely used.

George Santayana: "Those who do not remember the past are condemned to
repeat it."

Tim Menzies: "Those who do not study Prolog are condemned to repeat it."


## Prolog: Yet Another Paradigm

In LISP, everything is a _lambda body_ and computation is
evaluation within the environment of the current lambda.

In Smalltalk, everything is an _object_ (even _Object_)
and computation is message sends between objects.

In OCAML, everything is a _type_ and computation is a process
of converting between types.

In Prolog, everything is a _predicate_ and computation
is a process of recursively 
matching to predicates (possibly, with side-effects).

## heading Predicates

While functions map inputs to outputs...


- predicates define constraints on variables.
- So functions _run_ but predicates _match_.

And while functions return some output, predicates only return yes or no. If "yes", then as a side effect, they may add bindings to variables.
For example, in the following, 

- upper case things are the variables.
- _:- xx_ things are _queries_;
- _xx :- yy_ things are _rules_;
- _xx._ things are _facts_.


```
#!/usr/bin/env swipl
% vim: set filetype=prolog: ts=2 sw=2 sts=2 expandtab:cindent:formatoptions+=cro %

% defining grand dad
grandfather(X,Y) :- parent(X,Z),father(Z,Y).

parent(X,Y) :- mother(X,Y).
parent(X,Y) :- father(X,Y).

father(arthur,buck).
father(al,buck).
father(amber,buck).
father(anne,boris).
father(barbara,charles).
father(betty,cuthbert).
father(boris,charles).
father(buck,calvin).
father(charles,burt).

mother(al,barbara).
mother(anne,betty).
mother(arthur,barbara).
mother(amber,barbara).
mother(boris,carla).
mother(barbara,carla).
mother(betty,cora).

:- grandfather(al, Gdad),     print(grandad(Gdad)),nl.
:- grandfather(Gson,charles), print(grandson(Gson)),nl.
:- grandfather(Gson,Gdad),    father(Gdad,burt), print(greatgrandfather(burt,Gson)),nl.
:- halt.
```

Note that in the above, the predicates have no reserved inputs or outputs. Everything is constraints and these
can be specified in any order.


##  Terms

Predicates are build from _terms_:

Terms are are either _variable_ or 
_nonvar_. 
Nonvars are either _compound_ or 
_atomic_ (and these divide into _atoms_ or _numbers_).


     Term
        Variable
        NonVar
           Compound 
           Atomic
              Atom
              Number
     

Variables are empty slots- boxes, waiting to be filled. Note that once filled they cannot be changed; i.e. variable
are immutable.

Terms have an _functor_ and an 
_arity_. E.g. the following term has a functor/arity of emp/3:


	emp(tim,30,male).


Terms contain terms (recursively), one for each arity; e.g.

	emp(name(tim), age(30), sex(male))

Here:

- Argument three is a term of arity one with a functor of "sex": sex(male);
- Recursively, that term has an arity of one:
     - whose first argument is a term with arity zero: male.

Terms with an arity of zero are _atoms_. 
For example, in the above, the atoms are:

	emp
	name
	tim
	age
	sex
	male

The other atomic terms are numbers (integers or floats).

## Predicates = Trees of Terms 

Terms have arity (number of arguments)

emp(tim, 23, male)

    emp/3  ; arity = 3
        tim
        23
        male

2+3

    (+)/2 ; arity = 2
        2
        3


(note : some unary and binary functors can be written in a
prefix or infix notation. e.g. `2+31 is shorthand
for `+(2,3)`.)

10-10*3


    -/2 ; arity = 2
        10
            * / 2 ; arity = 2
                10
                3

- x

    -/0 ; artiy=0
        x

age(Dob,Age) :- now(Now), Age is Now - Dob.

    :-/2
            age/2
                A
                B
            ,/2
                    now/1
                        C
                    is/2
                        B
                            -/2
                                C
                                A

:- now(Now), print(Now))


    :-/1
            ,/2
                    now/1
                        A
                    print/1
                        B


## Variable Scope

The scope of variables is one clause; there is no such thing as a global variable or nested scope within blocks.



## Backtracking

When Prolog tries to answer a query, it goes through a matching process. It tries to match the predicates on the right-hand side of the rule from left to right, binding variables when it finds a match. If a match fails, it may be because of the bindings in matches to the left. In that case, the interpreter backtracks - it returns to the nearest previous binding and tries to find a different match. If that fails, it backtracks some more.

Now we can use the builtin trace predicate to see what happens as one of our rules is matched.


    ?- spy(grandfather).
    % Spy point on grandfather/2
    true.
    
    [debug]  ?-  grandfather(al,X).
       Call: (7) grandfather(al, _G313) ? creep
       Call: (8) parent(al, _L175) ? creep
       Call: (9) father(al, _L175) ? creep
       Exit: (9) father(al, buck) ? creep
       Exit: (8) parent(al, buck) ? creep
       Call: (8) father(buck, _G313) ? creep
       Exit: (8) father(buck, calvin) ? creep
       Exit: (7) grandfather(al, calvin) ? creep
    X = calvin ;
       Redo: (8) parent(al, _L175) ? creep
       Call: (9) mother(al, _L175) ? creep
       Exit: (9) mother(al, barbara) ? creep
       Exit: (8) parent(al, barbara) ? creep
       Call: (8) father(barbara, _G313) ? creep
       Exit: (8) father(barbara, charles) ? creep
       Exit: (7) grandfather(al, charles) ? creep
    X = charles.
    
    [debug]  ?- 
    
Note that rules in the database are tried in the order they appear in the database.

We can make Prolog print out all the answers to a question by deliberately making it fail, using the fail predicate.


    ?- grandfather(X,Y),write('X='),write(X),write(', Y='),write(Y),nl,fail.
    X=arthur, Y=calvin
    X=al, Y=calvin
    X=amber, Y=calvin
    X=anne, Y=charles
    X=al, Y=charles
    X=anne, Y=cuthbert
    X=arthur, Y=charles
    X=amber, Y=charles
    fail
    ?- 

## Negation as Failure

It is important to realize that in a goal `?-not(g)`, if g has variables,
and some instance of these variables lead to satisfaction of `g`, then
`?-not(g)` fails. For example, consider the following `bachelor` program:

    bachelor(P) :- male(P), not(married(P)). 
    
    male(henry). 
    male(tom). 
    
    married(tom).

Then

    ?- bachelor(henry). 
    yes 
    ?- bachelor(tom). 
    no 
    ?- bachelor(Who). 
    Who= henry ; 
    no 
    ?- not(married(Who)). 
    no. 
    
The first three responses are correct and as expected.  

The answer to
the fourth query might have been unexpected at first (like... no one is married, at all?).
 
But consider that
the goal `?-not(married(Who))`  fails because for the 
variable binding
`Who=tom, married(Who)` succeeds, and so the negative 
goal fails. Thus,
negative goals `?-not(g)s`_ with variables cannot be 
expected to produce
bindings of the variables for which the goal g fails.


## List Processing

Prolog lists have heads and tails (think "car" and "cdr").

	[a, b, c]

is really nested terms

    . / 2
        a
            . / 2
                b
                    . / 2
                        c
                        []

So we can write a list member predicate as follows:

    member(A, [A|_]).
    member(A, [_|B]) :- member(A, B).
    
So, of course,...

	?- member(a,[c,b,a]).
	true

But Prolog can backtrack to find all repeated members of a list:

    ?-    member(a,[c,a,b,a]).
    true ;
    true 
    
Also, we can find all members of a list:

    ?- member(X,[a,b,c]).
    X = a;
    X = b;
    X = c
    

Here's a predicated to print a list of terms. Like
all recursion it has at least two parts:

	prints([]).    % termination
	prints([H|T]) :- 
	   print(H), nl,  % handle one thing
	   prints(T).     % recurse to handle the rest
	

Also, Prolog is a relational language. Variables are not inputs and outputs but concepts woven together by constraints. Also, we only need to partially specify the I/O to query those constraints:

    ?-  member(name=What, [age=20,shoesize=12,name=tim]).
    What = tim .
    
(If this is causing stress, just chant to yourself: in Prolog, we don't code; rather, we draw the shape of the solution.)

To use that list stuff for a little database, we have to be able to update fields.

	switch([],_,[]).
	switch([X=_Old | T],  X=New, [X=New|T]).
	switch([Y=Old  | T0], X=New, [Y=Old|T]) :-
	   X \= Y,
	   switch(T0,X=New,T).

        ?- switch([a=1,b=2], b=3,Out).
	Out = [a=1,b=3].

### Fun with Append

Lists can get really funky. Here's the standard definition of how to append two lists:

    append([], A, A).
    append([A|B], C, [A|D]) :-
            append(B, C, D).
    
Which works in the expected way:

	?- append([a,b,c],[d,e,f],Out).
	Out = [a, b, c, d, e, f].

But any of these arguments can be left unspecified to achieve neat results:

    % find the third thing in a list
    ?- append([ _, _, Third], _, [alice,john,mike,tim,veronica,wendy]).
    Third = mike
    
    % find the list before "tim"
    ?- append(Before, [tim | _], [alice,john,mike,tim,veronica,wendy]).
    Before = [alice, john, mike]
    
    % find the list after "tim"
    ?- append(_, [tim | After], [alice,john,mike,tim,veronica,wendy]).
    After = [veronica, wendy]
    
You can even write "member" using "append":

	member1(X,L) :- append(_,[X|_],L).
	
	?- member1(a,[b,c,a]).
	true .

	?- member1(X,[b,c,a]).
    X = b ;
    X = c ;
    X = a ;
    fail.


