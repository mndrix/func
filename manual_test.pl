#!/usr/bin/env swipl -q -t main -s
:- use_module(func).

:- multifile func:compile_function/4.
func:compile_function(List, Index, Elem, nth0(Index, List, Elem)) :-
    is_list(List).

main :-
    format('~s world~n', [atom_codes $ hello]),
    Plus3 = succ of _+1 of plus(1),
    call(Plus3, 1, 4),
    format('The answer: ~p~n', [succ of plus(40) $ 1]),
    N = 2,
    format('Brought to you by the number ~w~n', [zero,one,two,three] $ N).


fact(0,1) :- !.
fact(N,F) :-
    N > 0,
    F is N * (fact of _-1 $ N).
