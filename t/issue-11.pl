:- use_module(library(func)).
:- use_module(library(func/issue_11), [count_unique/2]).
:- use_module(library(tap)).

composition :-
    count_unique([a,b,c,a,c],N),
    N == 3.
