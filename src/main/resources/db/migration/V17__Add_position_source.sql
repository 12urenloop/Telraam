create table position_source
(
    id serial not null
        constraint position_source_pk primary key,
    name varchar(255) not null unique
);