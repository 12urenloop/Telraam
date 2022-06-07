:- module(init, [init/1]).

:- use_module(logger).


init(JRefLogger) :-
    assert_logger(JRefLogger),
    jpl_pl_lib_version(JPLVersion),
    current_prolog_flag(version_data, swi(Ma, Mi, Pa, _)),
    format(string(PrologVersion), "~d.~d.~d", [Ma, Mi, Pa]),
    format(
        string(Banner), 
        "\n\n==> Successfully Initialized Prolog Lapper\n\n==> JPL: ~s\n==> Prolog: ~s\n", 
        [JPLVersion, PrologVersion]
    ),
    info(Banner).
