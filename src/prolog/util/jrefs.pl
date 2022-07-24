:- module(jrefs,[
    assert_jref/1,
    
    lapper_jref/1,
    logger_jref/1
]).

:- dynamic logger_jref/1.
:- dynamic lapper_jref/1.

assert_jref(Term) :-
    functor(Term, Name, 1),
    atom_concat(_, '_jref', Name),
    retractall(Term),
    assertz(Term).