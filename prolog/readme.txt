---+ Name

=func= - Function application and composition

---+ Synopsis

==
:- use_module(func, [op(675,xfy,$), op(650,xfy,of)]).
main :-
    Plus3 = succ of _+1 of plus(1),
    call(Plus3, 1, 4),
    format('~s world~n', [atom_codes $ hello]).
==

---+ Description

This module allows you to apply ($/2) and compose (of/2) predicates as
if they were functions.  For these purposes, a function is any
predicate whose final argument can be an "output" and whose
penultimate argument can be an "input".

Experimentally, any arithmetic expression of a single argument
(such as =|2*_+3|=) can also be treated as a function.

Repository available on GitHub: https://github.com/mndrix/func

---+ Installation

Using SWI-Prolog 6.3 or later:

==
    $ swipl
    1 ?- pack_install(function_expansion).
==

@author Michael Hendricks <michael@ndrix.org>
@license BSD
