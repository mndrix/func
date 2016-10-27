
:- module(foo, []).

foo(a).

bar(X) :-
    X = foo(~).
