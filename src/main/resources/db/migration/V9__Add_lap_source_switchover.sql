create table lapsourceswitchover
(
    id serial not null,
    newLapSource integer not null constraint lapsourceswitchover_lap_source_id_fk references lap_source,
    timestamp timestamp not null
);