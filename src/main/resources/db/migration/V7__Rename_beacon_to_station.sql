alter table beacon rename to station;

ALTER TABLE detection RENAME COLUMN beacon_id TO station_id;