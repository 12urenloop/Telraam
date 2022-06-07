:- module(detection, [assert_baton_switchovers/1, baton_switchover/2]).

:- use_module(library(record)).

:- use_module(atom_to_list).

:- record baton_switchover(
    id:                 integer,
    team_id:            integer,
    previous_baton_id:  integer,
    new_baton_id:       integer,
    timestamp:          string
).

assert_baton_switchovers(BatonSwitchovers) :-
    atom_to_list(BatonSwitchovers, BatonSwitchoversList),
    retractall(baton_switchover/2),
    maplist([baton_switchover(V1, V2, V3, V4, V5)]>>assertz(baton_switchover(V1, V2, V3, V4, V5)), BatonSwitchoversList).