#!/usr/bin/env swipl -q -t main -s
:- use_module(func, [op(675,xfy,$),op(650,xfy,of)]).

main :-
    format('~s world~n', [atom_codes $ hello]),
    Plus3 = succ of _+1 of plus(1),
    call(Plus3, 1, 4),
    format('The answer: ~p~n', [succ of plus(40) $ 1]).

fact(0,1) :- !.
fact(N,F) :-
    N > 0,
    F is N * (fact of _-1 $ N).
