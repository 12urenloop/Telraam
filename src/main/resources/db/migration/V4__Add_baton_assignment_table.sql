create table baton_assignment
(
    id        serial    not null
        constraint baton_assignment_pk
            primary key,
    baton_id  integer   not null
        constraint baton_assignment_baton_id_fk
            references baton,
    team_id   integer   not null
        constraint baton_assignment_team_id_fk
            references team,
    timestamp timestamp not null
        default now()
);