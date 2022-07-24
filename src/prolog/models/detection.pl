:- module(detection, [assert_detections/1, detection/2]).

:- use_module(atom_to_list).
:- use_module('../util/jrefs').

:- dynamic detection/2.

assert_detections(Detections) :-
    atom_to_list(Detections, DetectionsList),
    retractall(detection/2),
    maplist([detection(V1, V2)]>>assertz(detection(V1, V2)), DetectionsList).