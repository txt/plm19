
eg(1) :- print(1)

egs :- forall(clause(eg(What),Code),
              (format("\n%--- ~a -----\n",[Code]), Code)).

:- print(1)
:- eq(1).
:- egs, halt.
% vim: set filetype=prolog: ts=2 sw=2 sts=2 expandtab:cindent:formatoptions+=cro %
