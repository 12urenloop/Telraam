create table batonswitchover
(
    id serial not null,
    teamId integer not null constraint batonswitchover_team_id_fk references team,
    previousBatonId integer not null constraint batonswitchover_previous_baton_id_fk references baton,
    newBatonId integer not null constraint batonswitchover_new_baton_id_fk references baton,
    timestamp timestamp not null
);