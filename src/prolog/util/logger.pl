:- module(logger, [info/1, error/1, assert_logger/1]).

:- dynamic logger/1.


convert_thing(Thing, Thing) :- 
    atom(Thing), !.
convert_thing(Thing, JRefString) :- 
    string(Thing),
    string_to_atom(Thing, AtomString),
    jpl_new('java.lang.String', [AtomString], JRefString), !.
convert_thing(Thing, Atom) :- 
    term_to_atom(Thing, Atom), !.


info(Thing) :- log('INFO', Thing).
error(Thing) :- log('SEVERE', Thing).


log(Level, Thing) :-
    logger(JRefLogger),
    convert_thing(Thing, ConvertedThing),
    jpl_get('java.util.logging.Level', Level, JRefLevel),
    jpl_call(JRefLogger, log, [JRefLevel, ConvertedThing], _).


assert_logger(JRefLogger) :-
    retractall(logger(_)),
    asserta(logger(JRefLogger)).