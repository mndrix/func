:- module(func, [ op(675, xfy, $)
                , op(650, xfy, of)
                ]).
:- use_module(library(list_util), [xfy_list/3]).
:- use_module(library(function_expansion)).
:- use_module(library(arithmetic)).


% for the cross-referencer. removed during macro expansion
:- meta_predicate $(2,+).
$(_,_).

user:function_expansion($(F,X), Y, Y is F) :-  % single var arithmetic functions
    arithmetic:evaluable(F),
    !,
    term_variables(F, [X]).
user:function_expansion($(F,X), Y, func:Apply) :-  % composed functions
    func:function_composition_term(F),
    function_expansion(F, func:Functor, true),
    Apply =.. [Functor,X,Y].
user:function_expansion($(F,X), Y, Apply) :-  % basic functions
    \+ func:function_composition_term(F),
    F =.. [Functor|Args],
    append(Args, [X, Y], NewArgs),
    Apply =.. [Functor|NewArgs].


% for the cross-referencer.  removed during macro expansion
:- meta_predicate of(2,2).
of(_,_).

% Converts an evaluable arithmetic expression of one variable
% into a predicate of two variables.  For all else, F=P.
% Identical expressions are compiled only once.
to_predicate(F,P) :-
    arithmetic:evaluable(F),
    !,
    term_variables(F, [X]),
    variant_sha1(F, Hash),  % term_hash/2 supports only ground terms
    format(atom(P), 'arithmetic_function_~p', [Hash]),
    (   current_predicate(P/2)
    ->  true  % predicate already exists
    ;   Head =.. [P,X,Y],
        assert((Head :- Y is F)),
        compile_predicates([P/2])
    ).
to_predicate(F,F).


% True if the argument is a function composition term
function_composition_term(of(_,_)).

% Converts a function composition term into a list of functions to compose
functions_to_compose(Term, Funcs) :-
    functor(Term, Op, 2),
    Op = 'of',
    xfy_list(Op, Term, Raw),
    maplist(to_predicate, Raw, Funcs).

user:function_expansion(Term, func:Functor, true) :-
    functions_to_compose(Term, Funcs),
    xfy_list(',', DcgBody, reverse $ Funcs),
    format(atom(Functor), 'composed_function_~d', [term_hash $ Funcs]),
    (   func:current_predicate(Functor/2)
    ->  true  % predicate implementing this composition already exists
    ;   dcg_translate_rule((Functor --> DcgBody), Rule),
        func:assert(Rule),
        func:compile_predicates([Functor/2])
    ).

:- begin_tests(apply).
test(single) :-
    X = succ $ 41,
    X = 42.
test(multiple) :-
    X = plus(5) $ succ $ 10,
    16 = X.
test(arithmetic) :-
    8 =:= 2*(succ $ 3).
test(evaluable_functions) :-
    X = _+1 $ 0,
    X =:= 1.
:- end_tests(apply).

:- begin_tests(compose).
test(two) :-
    F = succ of plus(2),
    call(F, 4, Y),
    Y = 7.
test(multiple) :-
    call(succ of succ of succ, 0, 3).
test(idempotent) :-
    F = plus(3) of plus(1),
    G = plus(3) of plus(1),
    G = F,
    call(F, 1, X),
    call(G, 1, X).
test(evaluable_functions) :-
    Plus3 = succ of _+1 of plus(1),
    call(Plus3, 0, 3).
:- end_tests(compose).

:- begin_tests(compose_apply).
test(typical) :-
    format(codes(A), '~s world', [reverse of atom_codes $ ih]),
    A = "hi world".
test(numeric) :-
    10 = plus(1) of plus(3) of succ $ 5.
:- end_tests(compose_apply).
