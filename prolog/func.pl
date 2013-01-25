:- module(func, [ op(800, xfy, $)
                , op(750, xfy, of)
                ]).
:- use_module(library(list_util), [xfy_list/3]).
:- use_module(library(function_expansion)).


% for the cross-referencer. removed during macro expansion
:- meta_predicate $(2,+).
$(_,_).

% Y is the result of applying function F to argument X
user:function_expansion($(F,X), Y, Apply) :-  % basic functions
    (   functor(F, 'of', 2)
    ->  user:function_expansion(F, of_dcg(Id), true),
        Functor = of_dcg,
        Args = [Id]
    ;   F =.. [Functor|Args]
    ),
    append(Args, [X, Y], NewArgs),
    Apply =.. [Functor|NewArgs].

% for the cross-referencer.  removed during macro expansion
:- meta_predicate of(2,2).
of(_,_).

user:function_expansion(Term, of_dcg(Id), true) :-
    functor(Term, 'of', 2),
    format('considering ~p~n', [Term]),
    xfy_list('of', Term, Funcs),
    xfy_list(',', DcgBody, reverse $ Funcs),
    term_hash(Funcs, Id),
    format('expanding of_dcg(~p) --> ~p~n', [Id, DcgBody]),
    dcg_translate_rule((of_dcg(Id) --> DcgBody), Rule),
    writeln('  asserting'),
    func:assertz(Rule).


normal :-
    plus(10, 30, A),
    succ(A, B),
    succ(B, C),
    writeln(C).

applied :-
    writeln(succ $ succ $ plus(10) $ 30).

composed :-
    writeln(succ of succ of plus(10) $ 30).
