#!/usr/bin/env swipl -q -t main -s
:- use_module(func, [op(675,xfy,$),op(650,xfy,of)]).

main :-
    Plus3 = succ of succ of plus(1),
    call(Plus3, 1, 4),

    format('The answer: ~p~n', [succ of plus(40) $ 1]).
