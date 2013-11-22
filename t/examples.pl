:- use_module(library(func)).

% list indexing function
:- multifile func:compile_function/4.
func:compile_function(List, Index, Elem, nth0(Index, List, Elem)) :-
    is_list(List).

% factorial predicate to use as a function
fact(0,1) :- !.
fact(N,F) :-
    N > 0,
    F is N * (fact of _-1 $ N).


:- use_module(library(tap)).

'hello world' :-
    X = atom_codes $ hello,
    X = [0'h, 0'e, 0'l, 0'l, 0'o].

plus3 :-
    Plus3 = succ of _+1 of plus(1),
    call(Plus3, 1, X),
    X = 4.

'compose apply' :-
    X = succ of plus(40) $ 1,
    X =:= 42.

indexing :-
    N = 2,
    Atom = [zero,one,two,three] $ N,
    Atom = two.

factorial :-
    fact(7,Factorial),
    Factorial =:= 5040.

'negative factorial'(fail) :-
    fact(-3, _).
