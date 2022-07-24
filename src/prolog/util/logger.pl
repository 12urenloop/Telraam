:- module(logger, [info/1, error/1]).

:- use_module(jrefs).


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
    (logger_jref(JRefLogger) ->
        true
        ;
        lapper_jref(JRefLapper),
        jpl_get(JRefLapper, 'logger', JRefLogger),
        assert_jref(logger_jref(JRefLogger))
    ),
    convert_thing(Thing, ConvertedThing),
    jpl_get('java.util.logging.Level', Level, JRefLevel),
    jpl_call(JRefLogger, log, [JRefLevel, ConvertedThing], _).