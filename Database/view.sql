CREATE OR REPLACE VIEW gameview AS
SELECT *, winner(regulartimegoals1, overtimegoals1, NULL, regulartimegoals2, overtimegoals2, NULL, TRUE) AS winner,
winner(regulartimegoals1, overtimegoals1, penaltygoals1, regulartimegoals2, overtimegoals2, penaltygoals2, kogame) AS kowinner FROM game;

CREATE OR REPLACE VIEW tipview AS
SELECT t.*, p.firstname || ' ' || p.name as name, p.nationality, p.expertstatus, p.artificial, 
winner(goals1, NULL, NULL, goals2, NULL, NULL, TRUE) AS winner 
FROM tip t JOIN player p on t.username = p.username;

CREATE OR REPLACE view tipgame AS
SELECT t.gameid, g.tournamentid, t.username, t.points, g.kogame FROM tip t JOIN game g ON (t.gameid = g.gameid AND t.tournamentid = g.tournamentid);

CREATE OR REPLACE VIEW userstat AS
SELECT x.tournamentid, x.username, name, firstname, nationality, expertstatus, artificial, 
       nulltozero(grouppoints) AS grouppoints, nulltozero(groupgames) as groupgames, nulltozero(evalgroupgames) as evalgroupgames, 
       nulltozero(kopoints) AS kopoints, nulltozero(kogames) AS kogames, nulltozero(evalkogames) AS evalkogames
       FROM (SELECT tournamentid, username, name, firstname, nationality, expertstatus, artificial FROM player p, tournament t) AS x
       LEFT OUTER JOIN (SELECT username, tournamentid, sum(points) AS grouppoints, count(*) AS groupgames, count(points) AS evalgroupgames 
         FROM tipgame WHERE NOT kogame GROUP BY username, tournamentid) AS t1 ON x.username = t1.username AND x.tournamentid = t1.tournamentid
       LEFT OUTER JOIN (SELECT username, tournamentid, sum(points) AS kopoints, count(*) AS kogames, count(points) AS evalkogames 
         FROM tipgame WHERE kogame GROUP BY username, tournamentid) AS t2 ON x.username = t2.username AND x.tournamentid = t2.tournamentid;

CREATE OR REPLACE VIEW groupphasetable AS
SELECT l.tournamentid, l.team, fifaranking, initialgroup, played, won, draw, loss, goalsfor, goalsagainst, points FROM 
  (SELECT tournamentid, team1 as team from game union select tournamentid, team2 as team from game) AS l 
  INNER JOIN team t ON t.team = l.team
  JOIN getGamePoints(t.team, l.tournamentid) p ON l.team = p.team;

CREATE OR REPLACE VIEW nationstat AS
SELECT tournamentid, avg(points) AS avgpoints, sum(points) AS sumpoints, count(points) AS tipgames,
       count(distinct t.username) AS players, nationality
       FROM tipview t GROUP BY nationality, tournamentid;

CREATE OR REPLACE VIEW teampointstat AS
SELECT tournamentid, team, sum(points) AS sumpoints, sum(num) AS numtips, count(distinct gameid) AS games, sum(points)::REAL/sum(num)::REAL AS avgpoints FROM (
SELECT t1.tournamentid, team1 AS team, points, 1 AS num, t1.gameid FROM tipgame t1 JOIN game g1 ON g1.gameid = t1.gameid AND g1.tournamentid = t1.tournamentid WHERE points IS NOT NULL
UNION ALL
SELECT t2.tournamentid, team2 AS team, points, 1 AS num, t2.gameid FROM tipgame t2 JOIN game g2 ON g2.gameid = t2.gameid AND g2.tournamentid = t2.tournamentid WHERE points IS NOT NULL) AS teampoint
GROUP BY team, tournamentid;
