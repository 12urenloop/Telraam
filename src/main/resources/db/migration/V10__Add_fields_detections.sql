DELETE FROM detection;

ALTER TABLE detection ADD rssi INTEGER NOT NULL;
ALTER TABLE detection ADD battery FLOAT NOT NULL;
ALTER TABLE detection ADD uptime_ms bigint NOT NULL;
ALTER TABLE detection ADD remote_id INTEGER NOT NULL;