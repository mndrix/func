:- module(func, [ op(675, xfy, $)
                , op(650, xfy, of)
                , '$'/2
                , 'of'/2
                , compile_function/4
                ]).
:- use_module(library(list_util), [xfy_list/3]).
:- use_module(library(function_expansion)).
:- use_module(library(arithmetic)).
:- use_module(library(error)).

%%  compile_function(+Term, -In, -Out, -Goal) is semidet.
%
%   True if Term represents a function from In to Out
%   implemented by calling Goal.  This multifile hook is
%   called by $/2 and of/2 to convert a term into a goal.
%   It's used during compile time for macro expansion.
%   It's used during run time to handle functions which aren't
%   known at compile time.
%   When called as a hook, Term is guaranteed to be =nonvar=.

:- multifile compile_function/4.
compile_function(Var, _, _, _) :-
    % variables storing functions must be evaluated at run time
    % and can't be compiled, a priori, into a goal
    var(Var),
    !,
    fail.
compile_function(Expr, In, Out, Out is Expr) :-
    % arithmetic expression of one variable are simply evaluated
    arithmetic:evaluable(Expr),
    term_variables(Expr, [In]).
compile_function(F, In, Out, func:Goal) :-
    % composed functions
    function_composition_term(F),
    user:function_expansion(F, func:Functor, true),
    Goal =.. [Functor,In,Out].
compile_function(F, In, Out, Goal) :-
    % string interpolation via format templates
    format_template(F),
    ( atom(F) ->
        Goal = format(atom(Out), F, In)
    ; error:has_type(codes, F) ->
        Goal = format(codes(Out), F, In)
    ; fail  % to be explicit
    ).


% True if Format is a template string suitable for format/3.
% The current check is very naive and should be improved.
format_template(Format) :-
    atom(Format), !,
    atom_codes(Format, Codes),
    format_template(Codes).
format_template(Format) :-
    error:has_type(codes, Format),
    memberchk(0'~, Format).  % ' fix syntax highlighting

% for the cross-referencer and PlDoc. removed during macro expansion

%%	$(+Function, +Argument) is det.
%
%	Apply Function to an Argument.  A Function is any predicate
%	whose final argument generates output and whose penultimate argument
%	accepts input.
%
%	This is realized by expanding function application to chained
%	predicate calls at compile time.  Function application itself can
%	be chained.
%
%	==
%	Reversed = reverse $ sort $ [c,d,b].
%	==
:- meta_predicate $(2,+).
$(_,_).

user:function_expansion($(F,X), Y, Goal) :-
    ( compile_function(F, X, Y, Goal) ->
        true
    ; var(F) -> Goal =      % defer until run time
        ( compile_function(F, X, Y, P) ->
            call(P)
        ; call(F, X, Y)
        )
    ; Goal = call(F, X, Y)
    ).


% for the cross-referencer and PlDoc.  removed during macro expansion

%%	of(+F, +G) is det.
%
%	Creates a new function by composing F and G.  The functions are
%	composed at compile time to create a new, compiled predicate which
%	behaves like a function.  Function composition can be chained.
%	Composed functions can also be applied with $/2.
%
%	==
%	Reversed = reverse of sort $ [c,d,b].
%	==
:- meta_predicate of(2,2).
of(_,_).

% True if the argument is a function composition term
function_composition_term(of(_,_)).

% Converts a function composition term into a list of functions to compose
functions_to_compose(Term, Funcs) :-
    functor(Term, Op, 2),
    Op = 'of',
    xfy_list(Op, Term, Funcs).

% Thread a state variable through a list of functions.  This is similar
% to a DCG expansion, but much simpler.
thread_state([], [], Out, Out).
thread_state([F|Funcs], [Goal|Goals], In, Out) :-
    ( compile_function(F, In, Tmp, Goal) ->
        true
    ; var(F) ->
        instantiation_error(F)
    ; F =.. [Functor|Args],
      append(Args, [In, Tmp], NewArgs),
      Goal =.. [Functor|NewArgs]
    ),
    thread_state(Funcs, Goals, Tmp, Out).

user:function_expansion(Term, func:Functor, true) :-
    functions_to_compose(Term, Funcs),
    format(atom(Functor), 'composed_function_~w', [variant_sha1 $ Funcs]),
    (   func:current_predicate(Functor/2)
    ->  true  % predicate implementing this composition already exists
    ;   thread_state(reverse $ Funcs, Threaded, In, Out),
        xfy_list(',', Body, Threaded),
        Head =.. [Functor, In, Out],
        func:assert(Head :- Body),
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
test(interpolation) :-
    One = 'hello ~w' $ world,
    One = 'hello world',
    Two = '1 ~d ~d 4' $ [2, 3],
    Two = '1 2 3 4'.
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
test(interpolation) :-
    F = 'hello ~w' of downcase_atom,
    call(F, 'WoRLd', Msg),
    Msg = 'hello world'.
:- end_tests(compose_apply).
