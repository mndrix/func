# Synopsis

    :- use_module(library(func)).
    main :-
        % create a Plus3 function by composing three others
        Plus3 = succ of _+1 of plus(1),
        call(Plus3, 1, 4),
        format('~s world~n', [atom_codes $ hello]).

# Description

This module allows one to apply ($/2) and compose (of/2) terms as if
they were functions.  One often uses predicates as these functions,
but one can define function behavior for arbitrary terms.  See
"What is a function" and "Defining functions" below.

Why? Prolog predicates are more powerful than functions, but sometimes
the syntax is awkward or requires meaningless effort from the
developer (generating and maintaining intermediate variable names and
goals).  Using library(func) often results in more succinct, clearer
code.  For example, the use of atom_codes/2 in the Synopsis above.

At compile time, library(func) converts function application and
composition into standard predicate calls.  There should be no
performance penalty and one can still use nondeterminism.

# What is a function?

For our purposes, a function is any term which can be converted into a
predicate call that accepts input in a single variable and produces
output by binding a single variable.  The following sections describe
terms which library(func) can natively treat as functions.  See
further below for instructions on defining function behavior for
additional terms.

## Predicates as functions

Any predicate whose final argument can be viewed as an "output" and
whose penultimate argument can be viewed as "input" can be used,
without modification, as a function.

For example, succ/2 can be seen as accepting an input as the first
argument and producing an output, in the second argument, that's one
greater.  Similarly, the term `plus(3)` can be seen as a predicate
which takes an integer input and generates an integer output that's
three larger.

Because Prolog predicates often follow a convention of having "inputs"
before "outputs", many predicates can be applied and composed as
functions as is.  This includes length/2, reverse/2, maplist/3,
append/3, etc.

## Dicts

An SWI-Prolog 7 dictionary is considered a function from its keys to its values.  Applying the function to a non-existent key fails.

    ?- writeln(words{1:one, 2:two, 3:three} $ 2).
    two
    ?- writeln(words{1:one, 2:two, 3:three} $ 4).
    false.

This is similar to SWI Prolog's dot notation but doesn't throw an exception for missing keys.  Dicts as functions can be composed and applied just like other functions.


## Arithmetic expressions of one variable

Any arithmetic expression of a single variable can be applied and
composed as a function.  For example, `2*_+3` is the function which
multiplies a number by two and then adds three.  Similarly,
`sqrt(X)*X + X/2` is a function even though it uses the input in
three different places.

## format/2 templates

A format string acceptable as the first argument to format/2 can be
used as a function.  It generates an atom, list of codes or string
as output.
The template's type determines the output's type.  This offers a
powerful string interpolation syntax visually similar to Python's.

In this next example, `X` might hold any of the values `codes`,
`chars`, `number` or  `length`.

    call('atom_~w' $ X, Atom, Term)

One might also use this interpolation syntax to build a file path:

    Path = "/home/~w/src/~w/.git/config" $ [User, Project]

## Tilde Terms

A compound term with a single `~` argument is considered a function which takes no input values and produces an output at the `~` position.  For example,

    atom(atom_string(~,"hello world")).

produces code that's equivalent to

    atom_string(X,"hello world"),
    atom(X).

This can be conveniently employed with arithmetic expressions.

    length(List, ~ is X + Y).

Because tilde terms take no inputs, they can't be used with $/2 or of/2.

# Defining functions

Any term can behave as a function by defining additional clauses for
the multifile hook func:compile_function/4.  See the full
documentation for greater detail.  In this example, we'll define a
list term as a function from a 0-based index to the corresponding
element of that list.

    :- multifile func:compile_function/4.
    func:compile_function(List, Index, Elem, nth0(Index, List, Elem)) :-
        is_list(List).

We might use it to convert small integers into English words:

    N = 2,
    format('The number word is ~w~n', [zero,one,two,three] $ N).

One might imagine similar definitions for assoc lists, binary trees,
hash tables, etc.

# Changes in this Version

  * Support strings as format/2 template functions

# Installation

Using SWI-Prolog 6.3 or later:


    $ swipl
    1 ?- pack_install(func).


Source code available and pull requests accepted on GitHub:
https://github.com/mndrix/func


@author Michael Hendricks <michael@ndrix.org>
@license BSD
