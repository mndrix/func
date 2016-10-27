:- use_module(library(func)).

:- prolog_load_context(directory, Dir),
   asserta(user:file_search_path(here, Dir)).

my_mod:foo([1,2,3]).

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

tilde_with_module :-
    length(my_mod:foo(~), N),
    N == 3.

% see issue 16
tilde_with_unqual_module :- 
    use_module(here(resources/foo)), %% see dummy file resources/foo.pl
    foo:bar(a). 

dicts :-
    John = person{ name: "John", age: 27 },
    "John" == John $ name,
    27 =:= John $ age.

'dict missing a key'(fail) :-
    Point = point{x:2, y:3},
    writeln(Point $ z).  % looking up z fails
