#!/usr/bin/env swipl
% vim: set filetype=prolog: ts=2 sw=2 sts=2 expandtab:cindent:formatoptions+=cro %

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PROLOG TUTOR
% ============
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TASK
% Write human-readable rules to manage these facts.

:- dynamic fact/3. % can be retracted/asserted at runtime
fact(emp,tim,      [job=cleaner,mother=president]).
fact(emp,jane,     [job=president]).
fact(job,cleaner,  [salary=10000]).
fact(job,professor,[salary=30000]).

% for example, let me show you how i got this job
%
% rule nepotism 
% if   
%      emp = E had job=J had mother in [president,chancellor] and
%      job = J had salary =< 10000 and
%      job = J2 had salary > 20000
% then
%      emp =E has job = J2.
% %

% To achieve this task, take 6 small steps
% If it works, you should be able to load the code with zero
% load-time errors and all the testN predicates working without
% getting "failed(X)" errors

main:- tests, testNepotism.
% Expected output
%
% $ ./onea
% 
%   passed(emp=tim had job=cleaner)
% 
%   passed(emp=tim had job=cleaner had mother in [president,chancellor])
% 
%   passed(emp=tim had job=cleaner had mother in [president,chancellor]and job=cleaner had salary=<10000)
%   :- dynamic fact/3.
% 
%   fact(emp, tim, [job=cleaner, mother=president]).
%   fact(emp, jane, [job=president]).
%   fact(job, cleaner, [salary=10000]).
%   fact(job, professor, [salary=30000]).
% 
% 
%   :- dynamic fact/3.
% 
%   fact(emp, tim, [job=professor, mother=president]).
%   fact(emp, jane, [job=president]).
%   fact(job, cleaner, [salary=10000]).
%   fact(job, professor, [salary=30000]).
% $
% 
% 
% 1111111111111111111111111111111111111111111111111111111

% Define operators such that the following rules does NOT generate an
% error when prolog loads it.

% Hints: don't go above 999 (bad things happen).
% Hints: if you stay above 700 then the standard arithmetics can be inside 
% Hints: for trivial little prefix, ostfix markers, use 1

:- op(902,xfx,  if).
:- op(901,xfx,  then).

rule1 if a then b.

rule 2 if a then b.

rule 3 if a and b then c.

rule 4 if a and b and c then d.

rule 5 if a or b and c and d then e.

rule 6 if a or b and c and d or e then f.

rule 7 if a or b and c and d or not e then f.

rule 8 if a or b and c and d or not e then f.

rule 9 if a or not (b and c and d) or e then f.

rule 10 if emp had job=_ then b.

% the the left of the first had, we match on functor and id
% "=" is inside "had"
rule 11 if emp = _ had job =_ then b.


% "X had Y" is inside "or"
rule 12 if a or emp = _ had job=_ then b.

% "X had Y" is inside "amd"
rule 13 if a or b and emp = _ had job=_ then c.

% there can be multiple "had" tests
rule 14 if a or b and emp = _ had job=_ had mother = _ then c.

% "in" is a keyword for set membership
rule 15 if emp = _ had job=_ had mother in [president,chancellor]  then c.

% inside a had, tests are "=", "\=", ">", "<", ">=", "=<"
rule 16 if  
     emp = _ had job=J had mother in [president,chancellor] and
     job = J had salary =< 10000 
then c.

% "has" does updates.
% but this time, "=" means "change that field"  (and  "in,<,>,\=" etc are
% not supported"

rule nepotism 
if   
     emp = E had job=J had mother in [president,chancellor] and
     job = J had salary =< 10000 and
     job = J2 had salary > 20000
then
     emp =E has job = J2.

% 222222222222222222222222222222222222222222222222222222222222

% Get rule conditon testing on solo facts to work.
% hint: implement right associative "had". see has(X and Y)
% for an example on how that is done

tests :- forall(test(N), test1(N)).

test1(X) :-
  nl,rule X if Condition then _,
  had(Condition),
  print(passed(Condition)), nl,!.
test1(X) :-
  print(failed(X)),nl.

test(11).

% 3333333333333333333333333333333333333333333333333333333333

% Add testing for set membership to they ssytem
% Hint: extend the "b4" clauses

test(15).

% 4444444444444444444444444444444444444444444444444444444444

% Add numeric testing.
% Hint: extend the "b4" clauses

test(16).

% 5555555555555555555555555555555555555555555555555555555555

% get the nepotism rule to fire.
% Hint, if all the above works, then this should work fine.

testNepotism :-
  listing(fact),
  nl,
  rule(nepotism) if Condition then Action,
  had(Condition),
  has(Action),
  listing(fact).

% 666666666666666666666666666666666666666666666666666666666
% Add "or" and "not" to "had" .
% Write rules and "test" facts to show that you can handle
% "or" and "not"

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% you should use the following code.
% WARNING: the code for "had" is incomplete

had(X and Y)      :- had(X), had(Y).
had(X =Id had Y)  :- fact(X,Id,Fs),  b4(Y,Fs).

b4(X had Y,Fs)    :- b4(X,Fs), b4(Y,Fs).
b4(X =  Y, Fs)    :- member(X=Y,Fs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This is a complete implementation of "has"

has(X and Y)      :- has(X), has(Y).
has(X =Id has Y)  :- 
   retract(fact(X,Id,Old)), 
   now(Y,Old,New), 
   asserta(fact(X,Id,New)). 

now(X has Y, Old,New) :-  now(X,Old,Tmp), now(Y,Tmp,New).
now(X = Y,  Old, New) :-  switch(Old,X=Y,New).

switch([],_,[]).
switch([X=_Old | T],  X=New, [X=New|T]).
switch([Y=Old  | T0], X=New, [Y=Old|T]) :-
   X \= Y,
   switch(T0,X=New,T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% To run this tutorial first isntall prolog
%
%	sudo apt-get install swi-prolog
% 
% Next, split the screen with an editor on one side and a shell on the other.
%
% Next, make these files executable
%
%       chmod +x onea
%       chmod +x errors
%
% Then run the file in the shell, find errors, fix then in the editor, re-run
%
%	./onea
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ieally, complete this assignment with code that has no load errors.
% For an example of load errors, run ./errors

% You will see
%
% Warning: /home/cabox/gits/txt/plm19/src/pl/errors:6:
%             Singleton variables: [X]
% ERROR: /home/cabox/gits/txt/plm19/src/pl/errors:12:7: Syntax error: Operator expected
% ERROR: /home/cabox/gits/txt/plm19/src/pl/errors:17:7: Syntax error: Operator expected
%
% Also, sometimes Prolog will catch a one letter typo in your predicates.
% IF that happens you will see:

% Warning: The predicates below are not defined. If these are defined
% Warning: at runtime using assert/1, use :- dynamic Name/Arity.
% Warning:
% Warning: has/1, which is referenced by
% Warning:        /home/cabox/gits/txt/plm19/src/pl/onea:231:2: 1-st clause of think/0
% 


% using the REPL (kill the halt)

% load [onea]
% make
% spy
% curse of assert, retract
% curse of the knowledge base

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BTW, ypu will need this table.
% current_op reports current definitions.

prints([]).    % termination
prints([H|T]) :- 
   print(H), nl,  % handle one thing
   prints(T).     % recurse to handle the rest


% setof collects, and sorts, results.
allOps :- setof([P,A,X],current_op(P,A,X),L), prints(L).

%:- allOps.
%[1,fx,$]
%[100,yfx,'.']
%[200,fy,+]
%[200,fy,-]
%[200,fy,@]
%[200,fy,\]
%[200,xfx,**]
%[200,xfy,^]
%[250,yfx,?]
%[400,yfx,*]
%[400,yfx,/]
%[400,yfx,//]
%[400,yfx,<<]
%[400,yfx,>>]
%[400,yfx,div]
%[400,yfx,mod]
%[400,yfx,rdiv]
%[400,yfx,rem]
%[400,yfx,xor]
%[500,yfx,+]
%[500,yfx,-]
%[500,yfx,/\]
%[500,yfx,\/]
%[600,xfy,:]
%[700,xfx,:<]
%[700,xfx,<]
%[700,xfx,=]
%[700,xfx,=..]
%[700,xfx,=:=]
%[700,xfx,=<]
%[700,xfx,==]
%[700,xfx,=@=]
%[700,xfx,=\=]
%[700,xfx,>]
%[700,xfx,>:<]
%[700,xfx,>=]
%[700,xfx,@<]
%[700,xfx,@=<]
%[700,xfx,@>]
%[700,xfx,@>=]
%[700,xfx,\=]
%[700,xfx,\==]
%[700,xfx,\=@=]
%[700,xfx,as]
%[700,xfx,is]
%[900,fy,\+]
%[990,xfx,:=]
%[1000,xfy,',']
%[1050,xfy,*->]
%[1050,xfy,->]
%[1100,xfy,;]
%[1105,xfy,'|']
%[1150,fx,discontiguous]
%[1150,fx,dynamic]
%[1150,fx,initialization]
%[1150,fx,meta_predicate]
%[1150,fx,module_transparent]
%[1150,fx,multifile]
%[1150,fx,public]
%[1150,fx,thread_initialization]
%[1150,fx,thread_local]
%[1150,fx,volatile]
%[1200,fx,:-]
%[1200,fx,?-]
%[1200,xfx,-->]
%[1200,xfx,:-]

:- main.
%:- halt.
