:- use_module(library(func)).
:- use_module(library(tap)).

single :-
    X = succ $ 41,
    X = 42.

multiple :-
    X = plus(5) $ succ $ 10,
    16 = X.

arithmetic :-
    8 =:= 2*(succ $ 3).

evaluable_functions :-
    X = _+1 $ 0,
    X =:= 1.

interpolation :-
    One = 'hello ~w' $ world,
    One = 'hello world',
    Two = '1 ~d ~d 4' $ [2, 3],
    Two = '1 2 3 4'.
