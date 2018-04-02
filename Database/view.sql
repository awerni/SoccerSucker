CREATE OR REPLACE VIEW gameview AS
SELECT *, winner(regulartimegoals1, overtimegoals1, NULL, regulartimegoals2, overtimegoals2, NULL, TRUE) AS winner,
winner(regulartimegoals1, overtimegoals1, penaltygoals1, regulartimegoals2, overtimegoals2, penaltygoals2, kogame) AS kowinner FROM game;

CREATE OR REPLACE VIEW tipview AS
SELECT *, winner(goals1, NULL, NULL, goals2, NULL, NULL, TRUE) AS winner FROM tip;

CREATE OR REPLACE view tipgame AS
SELECT t.gameid, t.username, t.points, g.kogame FROM tip t JOIN game g ON (t.gameid = g.gameid);

CREATE OR REPLACE VIEW userstat AS
SELECT p.username, name, firstname, nationality, 
       nulltozero(grouppoints) AS grouppoints, nulltozero(groupgames) as groupgames, nulltozero(evalgroupgames) as evalgroupgames, 
       nulltozero(kopoints) AS kopoints, nulltozero(kogames) AS kogames, nulltozero(evalkogames) AS evalkogames
       FROM player p
       LEFT OUTER JOIN (SELECT username, sum(points) AS grouppoints, count(*) AS groupgames, count(points) AS evalgroupgames 
         FROM tipgame WHERE NOT kogame GROUP BY username) AS t1 ON p.username = t1.username
       LEFT OUTER JOIN (SELECT username, sum(points) AS kopoints, count(*) AS kogames, count(points) AS evalkogames 
         FROM tipgame WHERE kogame GROUP BY username) AS t2 ON p.username = t2.username;

CREATE OR REPLACE VIEW groupphasetable AS
SELECT t.team, fifaranking, initialgroup, played, won, draw, loss, goalsfor, goalsagainst, points 
       FROM team t JOIN getGamePoints(t.team) p ON t.team = p.team;

CREATE OR REPLACE VIEW nationstat AS
SELECT avg(points) AS avgpoints, sum(points) AS sumpoints, count(points) AS tipgames,
       count(distinct t.username) AS players, nationality
       FROM tipview t JOIN player p ON p.username = t.username GROUP BY nationality;

CREATE OR REPLACE VIEW teampointstat AS
SELECT team, sum(points) AS sumpoints, sum(num) AS numtips, count(distinct gameid) AS games, sum(points)::REAL/sum(num)::REAL AS avgpoints FROM (
SELECT team1 AS team, points, 1 AS num, t1.gameid FROM tipgame t1 JOIN game g1 ON g1.gameid = t1.gameid WHERE points IS NOT NULL
UNION ALL
SELECT team2 AS team, points, 1 AS num, t2.gameid FROM tipgame t2 JOIN game g2 ON g2.gameid = t2.gameid WHERE points IS NOT NULL) AS teampoint
GROUP BY team;
