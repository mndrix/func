:- module(func_issue_11, [count_unique/2]).
:- use_module(library(func)).

count_unique(L,N) :-
    F = len of unique,
    N = F $ L.

len(X,N) :-
    length(X,N).

unique(XX,X) :-
    sort(XX,X).
