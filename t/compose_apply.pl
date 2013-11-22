:- use_module(library(func)).
:- use_module(library(tap)).

typical :-
    format(string(A), '~s world', [reverse of atom_codes $ ih]),
    A = "hi world".

numeric :-
    10 = plus(1) of plus(3) of succ $ 5.

interpolation :-
    F = 'hello ~w' of downcase_atom,
    call(F, 'WoRLd', Msg),
    Msg = 'hello world'.
