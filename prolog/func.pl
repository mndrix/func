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
    (   function_composition_term(F)
    ->  user:function_expansion(F, Functor, true),
        Args = []
    ;   F =.. [Functor|Args]
    ),
    append(Args, [X, Y], NewArgs),
    Apply =.. [Functor|NewArgs].

% for the cross-referencer.  removed during macro expansion
:- meta_predicate of(2,2).
of(_,_).

% True if the argument is a function composition term
function_composition_term(of(_,_)).

% Converts a function composition term into a list of functions to compose
functions_to_compose(Term, Funcs) :-
    functor(Term, Op, 2),
    Op = 'of',
    xfy_list(Op, Term, Funcs).

user:function_expansion(Term, Functor, true) :-
    functions_to_compose(Term, Funcs),
    xfy_list(',', DcgBody, reverse $ Funcs),
    format(atom(Functor), 'composed_function_~d', [term_hash $ Funcs]),
    (   func:current_predicate(Functor/2)
    ->  true  % predicate implementing this composition already exists
    ;   dcg_translate_rule((Functor --> DcgBody), Rule),
        func:assert(Rule),
        func:compile_predicates([Functor/2])
    ).


normal :-
    plus(10, 30, A),
    succ(A, B),
    succ(B, C),
    writeln(C).

applied :-
    writeln(succ $ succ $ plus(10) $ 30).

composed :-
    writeln(succ of succ of plus(10) $ 30).
