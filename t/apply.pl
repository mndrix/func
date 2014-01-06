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
    One == 'hello world',
    Two = '1 ~d ~d 4' $ [2, 3],
    Two == '1 2 3 4',
    Three = "~w quotes" $ double,
    Three == "double quotes",
    Four = `back~w` $ ticks,
    Four == `backticks`.

tilde :-
    X is 2 + atom_length(foo,~),
    X =:= 5,
    atom_number(A, ~ is 3 + X),
    A == '8'.

dicts :-
    John = person{ name: "John", age: 27 },
    "John" == John $ name,
    27 =:= John $ age.

'dict missing a key'(fail) :-
    Point = point{x:2, y:3},
    writeln(Point $ z).  % looking up z fails
