:- module(tap, [ run_test//2
               , tap_header/1
               ]).
:- use_module(library(list_util)).

tap_header(TestCount) :-
    format('TAP version 13~n'),
    format('1..~d~n', [TestCount]).

run_test(ok, Test, Count0, Count) :-
    ( call(Test) ->
        test_result(ok, Test, Count0, Count)
    ; % otherwise ->
        test_result('not ok', Test, Count0, Count)
    ).
run_test(fails, Test, Count0, Count) :-
    ( call(Test) ->
        test_result('not ok', Test, Count0, Count)
    ; % otherwise ->
        test_result(ok, Test, Count0, Count)
    ).

test_result(Status, Test, N0, N) :-
    succ(N0, N),
    Test =.. [Name|_Options],
    format('~w ~w - ~w~n', [Status, N0, Name]).

test_expectation([], ok, []).
test_expectation([fails|Options], fails, Options) :- !.
test_expectation([todo|Options], todo, Options) :- !.
test_expectation([_|Options], Type) :-
    test_expectation(Options, Type).

% Thread a state variable through a list of predicates.  This is similar
% to a DCG expansion, but much simpler.
thread_state([], [], Out, Out).
thread_state([P0|Preds0], [P|Preds], In, Out) :-
    P0 =.. [Functor|Args],
    append(Args, [In, Tmp], NewArgs),
    P =.. [Functor|NewArgs],
    thread_state(Preds0, Preds, Tmp, Out).

user_wants_tap :-
    prolog_load_context(module, user),
    predicate_property(user:tap_header(_), imported_from(tap)).

:- dynamic test_case/3, user:main/0.
user:term_expansion((Head:-_), _) :-
    % collect test cases as the predicates are defined
    user_wants_tap,
    Head =.. [_|Options0],
    test_expectation(Options0, Expect, Options),
    tap:assertz(test_case(Head, Expect, Options)),
    fail.
user:term_expansion(end_of_file, _) :-
    % build main and tap_body
    user_wants_tap,
    findall(run_test(Expect,Head), tap:test_case(Head,Expect,_), Tests0),
    length(Tests0, TestCount),
    thread_state(Tests0, Tests, 1, _),
    list_util:xfy_list(',', Body, [tap_header(TestCount)|Tests]),
    user:assertz((main :- Body)),

    % undo all database side effects
    tap:retractall(test_case(_,_)),
    fail.
