alter table beacon
    add distance_from_start integer not null;

insert into lap_source(name) VALUES ('viterbi-lapper');

