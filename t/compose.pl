:- use_module(library(func)).
:- use_module(library(tap)).

two :-
    F = succ of plus(2),
    call(F, 4, Y),
    Y = 7.

multiple :-
    call(succ of succ of succ, 0, 3).

idempotent :-
    F = plus(3) of plus(1),
    G = plus(3) of plus(1),
    G = F,
    call(F, 1, X),
    call(G, 1, X).

evaluable_functions :-
    Plus3 = succ of _+1 of plus(1),
    call(Plus3, 0, 3).
