create table lap_source
(
    id   serial       not null
        constraint lap_source_pk
            primary key,
    name varchar(255) not null unique
);

alter table lap
    add lap_source_id integer not null;

alter table lap
    add
    constraint lap_lap_source_id_fk
    foreign key (lap_source_id) REFERENCES lap_source(id);

-- We always need sources for the internal lappers
-- ID must be the same as in the code
insert into lap_source(name) VALUES ('simple-lapper');

-- We always need manual count
insert into lap_source(name) VALUES ('manual-count');
