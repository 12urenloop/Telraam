ALTER TABLE team DROP COLUMN baton_id;

CREATE VIEW team_baton_ids (team_id, baton_id)
AS
SELECT bs.teamid, newbatonid
FROM batonswitchover bs
         INNER JOIN (SELECT teamid, MAX(timestamp) AS max_timestamp
                     FROM batonswitchover
                     GROUP BY teamid) latest ON bs.teamid = latest.teamid AND bs.timestamp = latest.max_timestamp
