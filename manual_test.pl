#!/usr/bin/env swipl -q -t main -s
:- use_module(func).
:- nb_setval(tap_test_count, 0).

:- multifile func:compile_function/4.
func:compile_function(List, Index, Elem, nth0(Index, List, Elem)) :-
    is_list(List).

main :-
    writeln('TAP version 13'),
    writeln('1..5'),
    ok(hello_world),
    ok(plus3),
    ok(compose_apply),
    ok(indexing),
    ok(factorial).

ok(Test) :-
    ( call(Test) ->
        test_result(ok, Test)
    ; % otherwise ->
        test_result('not ok', Test)
    ).

test_result(Status, Test) :-
    nb_getval(tap_test_count, N0),
    succ(N0, N),
    format('~w ~w - ~w~n', [Status, N, Test]),
    nb_setval(tap_test_count, N).

hello_world :-
    X = atom_codes $ hello,
    X = "hello".

plus3 :-
    Plus3 = succ of _+1 of plus(1),
    call(Plus3, 1, X),
    X = 4.

compose_apply :-
    X = succ of plus(40) $ 1,
    X =:= 42.

indexing :-
    N = 2,
    Atom = [zero,one,two,three] $ N,
    Atom = two.

factorial :-
    fact(7,Factorial),
    Factorial =:= 5040.

fact(0,1) :- !.
fact(N,F) :-
    N > 0,
    F is N * (fact of _-1 $ N).
