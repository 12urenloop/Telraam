--Delete all existing records.
DELETE FROM baton;

ALTER TABLE baton ADD mac VARCHAR NOT NULL UNIQUE;
