:- module(run,[run/0]).


:- use_module('util/logger').
:- use_module('util/jrefs').

run :-
    info("Running The Prolog Lapper"),
    info("Fetching Detections"),

    lapper_jref(JRefLapper),
    jpl_call(JRefLapper, 'getDetections', [], DetectionsString),
    term_string(Detections, DetectionsString),
    length(Detections, DetectionsLength),
    info(DetectionsLength),

    info("Done Fetching Detections"),
    info("Fetching Baton Switchovers"),

    lapper_jref(JRefLapper),
    jpl_call(JRefLapper, 'getBatonSwitchovers', [], BatonSwitchoversString),
    term_string(BatonSwitchovers, BatonSwitchoversString),
    length(BatonSwitchovers, BatonSwitchoversLength),
    info(BatonSwitchoversLength),

    info("Done Fetching Baton Switchovers"). 