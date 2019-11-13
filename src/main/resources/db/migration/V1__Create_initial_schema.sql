create table beacon
(
	id serial not null
		constraint beacon_pk
			primary key,
	name varchar(255)
);

create table baton
(
	id serial not null
		constraint baton_pk
			primary key,
	name varchar(255) not null
);

create table detection
(
	id serial not null
		constraint detection_pk
			primary key,
	beacon_id integer
		constraint detection_beacon_id_fk
			references beacon,
	baton_id integer
		constraint detection_baton_id_fk
			references baton,
	timestamp timestamp
);

create table team
(
	id serial not null
		constraint team_pk
			primary key,
	name varchar(255),
	baton_id integer
		constraint team_baton_id_fk
			references baton
);

create table lap
(
	id serial not null
		constraint lap_pk
			primary key,
	team_id integer
		constraint lap_team_id_fk
			references team,
	timestamp timestamp
);

