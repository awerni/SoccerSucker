CREATE OR REPLACE FUNCTION getTipPoints(theTournament INT2, theGameID INT2, theUser TEXT) RETURNS INT2 AS $$
DECLARE
  points INT2;
  rsGame record;
  rsTip record;
BEGIN
  SELECT INTO rsGame COALESCE(overtimegoals1, regulartimegoals1) as gameendgoals1, 
                     COALESCE(overtimegoals2, regulartimegoals2) as gameendgoals2, 
                     kogame, winner, kowinner FROM gameview WHERE gameid = theGameID AND tournamentid = theTournament;
  IF NOT found OR (rsGame.winner IS NULL) THEN
    RETURN(NULL);
  END IF;

  SELECT INTO rsTip goals1, goals2, winner, kowinner FROM tipview WHERE gameid = theGameID AND tournamentid = theTournament AND username = theUser;

  IF (rsTip.goals1 IS NULL OR rsTip.goals2 IS NULL) THEN
    RETURN(NULL);
  END IF;

  IF (rsGame.winner = rsTip.winner) THEN
    points = 3;
  ELSE
    points = 0;
  END IF;

  IF (rsGame.gameendgoals1 - rsGame.gameendgoals2 = rsTip.goals1 - rsTip.goals2) THEN
    points := points + 1;
  END IF;

  IF (rsGame.gameendgoals1 = rsTip.goals1 AND rsGame.gameendgoals2 = rsTip.goals2) THEN
    points := points + 1;
  END IF;

  IF (rsGame.kogame) THEN
    IF (rsGame.kowinner = rsTip.kowinner) THEN
      points := points + 1;
    END IF;
    points := points * 2;
  END IF;

  RETURN points;
END;
$$ LANGUAGE plpgsql;
-----------------------------------------------------
DROP FUNCTION getGamePoints(TEXT) CASCADE;
DROP TYPE teamResult CASCADE;
CREATE TYPE teamResult AS (
  tournamentid INT2,
  team TEXT,
  played INT2,
  won INT2,
  draw INT2,
  loss INT2,
  goalsfor INT2,
  goalsagainst INT2,
  points INT2
);

CREATE OR REPLACE FUNCTION getGamePoints(theTeam TEXT, theTournament INT2) RETURNS teamResult AS $$
DECLARE
  tr teamResult;
  rsGame record;
BEGIN
  tr.tournamentid := theTournament;
  tr.team := theTeam;
  tr.played := 0;
  tr.won := 0; 
  tr.draw := 0;
  tr.loss := 0;
  tr.goalsfor := 0;
  tr.goalsagainst := 0;
  tr.points = 0;

  FOR rsGame IN SELECT team1, team2, regulartimegoals1, regulartimegoals2, winner FROM gameview 
    WHERE tournamentid = theTournament AND NOT kogame AND (team1 = theTeam OR team2 = theTeam) AND regulartimegoals1 IS NOT NULL AND regulartimegoals2 IS NOT NULL
  LOOP
    tr.played := tr.played + 1;
    IF (rsGame.winner = '1' AND rsGame.team1 = theTeam) OR (rsGame.winner = '2' AND rsGame.team2 = theTeam) THEN 
      tr.won := tr.won + 1;
      tr.points := tr.points + 3;
    END IF;
    IF rsGame.winner = 'X' THEN
      tr.draw := tr.draw + 1;
      tr.points := tr.points + 1;
    END IF;
    
    IF rsGame.team1 = theTeam THEN
      tr.goalsfor := tr.goalsfor + rsGame.regulartimegoals1;
      tr.goalsagainst := tr.goalsagainst + rsGame.regulartimegoals2;
    ELSE 
      tr.goalsfor := tr.goalsfor + rsGame.regulartimegoals2;
      tr.goalsagainst := tr.goalsagainst + rsGame.regulartimegoals1;
    END IF;
  END LOOP;
 
  tr.loss := tr.played - tr.won - tr.draw;

  RETURN tr;

END;
$$ LANGUAGE plpgsql;

-----------------------------------------------------

CREATE OR REPLACE FUNCTION winner(regulartimegoals1 INT2, overtimegoals1 INT2, penaltygoals1 INT2, regulartimegoals2 INT2, overtimegoals2 INT2, penaltygoals2 INT2, kogame BOOL) RETURNS CHAR as $$
DECLARE
  goals1 INT2;
  goals2 INT2;
BEGIN
  IF (NOT kogame) THEN RETURN(NULL); END IF;

  IF (regulartimegoals1 IS NULL) OR (regulartimegoals2 IS NULL) THEN RETURN(NULL); END IF;
  goals1 := COALESCE(penaltygoals1, overtimegoals1, regulartimegoals1);
  goals2 := COALESCE(penaltygoals2, overtimegoals2, regulartimegoals2);

  IF (goals1 > goals2) THEN RETURN('1'); END IF;
  IF (goals1 = goals2) THEN RETURN('X'); END IF;
  RETURN('2');
END;
$$ LANGUAGE plpgsql;

-----------------------------------------------------

CREATE OR REPLACE FUNCTION place_tip(mytournament INT2, mygame INT2, myuser TEXT, tipgoals1 INT2, tipgoals2 INT2, kowin CHAR(1)) RETURNS BOOL AS $$
DECLARE
  rec record;
  gameover bool;
BEGIN
  SELECT starttime AT TIME ZONE 'Europe/Paris' < now() AT TIME ZONE 'Europe/Paris' INTO gameover FROM game WHERE gameid = mygame AND tournamentid = mytournament;
  IF gameover THEN
    RAISE EXCEPTION 'game % started already. You cannot place tips anymore', mygame;
  END IF;

  SELECT INTO rec goals1, goals2, kowinner FROM tip WHERE gameid = mygame AND tournamentid = mytournament AND username = myuser;
  IF found THEN
    IF rec.goals1 = tipgoals1 AND rec.goals2 = tipgoals2 AND ((rec.kowinner IS NULL AND kowin IS NULL) OR (rec.kowinner = kowin)) THEN
      RETURN FALSE;
    END IF;
  END IF;

  LOOP
    UPDATE tip SET tiptime = now() AT TIME ZONE 'Europe/Paris', goals1 = tipgoals1, goals2 = tipgoals2, kowinner = kowin 
      WHERE gameid = mygame AND tournamentid = mytournament AND username = myuser;
    IF found THEN
      RETURN TRUE;
    END IF;
    BEGIN
      INSERT INTO tip (gameid, tournamentid, username, tiptime, goals1, goals2, kowinner) 
        VALUES (mygame, mytournament, myuser, now() AT TIME ZONE 'Europe/Paris', tipgoals1, tipgoals2, kowin);
      RETURN TRUE;
    EXCEPTION WHEN unique_violation THEN
    -- do nothing, and loop to try the UPDATE again
    END;
  END LOOP;
END;
$$
LANGUAGE plpgsql;

-----------------------------------------------------
CREATE OR REPLACE FUNCTION NULLtoZero(n INT8) RETURNS INT8 AS $$
DECLARE
  mysum INT8;
BEGIN
  mysum := 0;
  IF n IS NOT NULL THEN
    mysum := mysum + n;
  END IF;
  RETURN (mysum);
END;
$$
LANGUAGE plpgsql;

-----------------------------------------------------
CREATE OR REPLACE FUNCTION betterTeam(points1 INT2, goalsfor1 INT2, goalsagainst1 INT2, points2 INT2, goalsfor2 INT2, goalsagainst2 INT2) RETURNS BOOL AS $$
BEGIN
  IF points1 < points2 THEN
    RETURN(TRUE);
  END IF;
  IF points1 > points2 THEN
    RETURN(FALSE);
  END IF;

  IF (goalsfor1 - goalsagainst1) < (goalsfor2 - goalsagainst2) THEN
    RETURN(TRUE);
  END IF;
  IF (goalsfor1 - goalsagainst1) > (goalsfor2 - goalsagainst2) THEN
    RETURN(FALSE);
  END IF;

  IF goalsfor1 < goalsfor2 THEN
    RETURN(TRUE);
  END IF;
  IF goalsfor1 > goalsfor2 THEN
    RETURN(FALSE);
  END IF;

  RETURN(FALSE);
END;
$$
LANGUAGE plpgsql;

-----------------------------------------------------
CREATE OR REPLACE FUNCTION gameTime(t timestamp with time zone) RETURNS text AS $$
DECLARE
  n timestamp;
BEGIN
  SELECT INTO n now() AT TIME ZONE 'Europe/Paris';
  IF t < n THEN
    return('past');
  END IF;
  IF (t - interval '3 days') < n THEN
    return('soon');
  END IF;
  return('later');
END;
$$
LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION updateMrAverage(theTournament INT2, theGame INT2) RETURNS BOOL AS $$
DECLARE
  rsAvg record;
  rsWinner record;
  winner INT2;
  isko BOOL;
BEGIN
  DELETE FROM TIP WHERE gameid = theGame AND tournamentid = theTournament AND username = 'average';

  SELECT INTO rsAvg round(avg(goals1)) AS goals1, 
                    round(avg(goals2)) AS goals2 
    FROM tip t JOIN player p on t.username = p.username WHERE not artificial AND gameid = theGame AND tournamentid = theTournament;

  IF (rsAvg.goals1 IS NULL OR rsAvg.goals2 IS NULL) THEN
    return(FALSE);
  END IF;

  SELECT INTO isko kogame FROM game WHERE gameid = theGame AND tournamentid = theTournament;

  IF isko THEN
    IF (rsAvg.goals1 = rsAvg.goals2) THEN
      SELECT INTO rsWinner count(*) AS freq, kowinner FROM tip t JOIN player p ON t.username = p.username
        WHERE gameid = theGame AND tournamentid = theTournament AND NOT artificial 
        GROUP BY kowinner ORDER BY freq DESC LIMIT 1;
      winner := rsWinner.kowinner;
    ELSE
      IF (rsAvg.goals1 > rsAvg.goals2) THEN
        winner := '1';
      ELSE
        winner := '2';
      END IF;
    END IF;
  END IF;

  INSERT INTO tip (goals1, goals2, kowinner, gameid, tournamentid, username, tiptime) 
    VALUES (rsAvg.goals1, rsAvg.goals2, winner, theGame, theTournament, 'average', now() AT TIME ZONE 'Europe/Paris');
  RETURN(TRUE);
END;
$$
LANGUAGE plpgsql;

---------------------------------

CREATE OR REPLACE FUNCTION updateExpert(theTournament INT2, theGameID INT2, expertclass INT2) RETURNS BOOL AS $$
DECLARE
  curr_user TEXT;
  rsAvg record;
  rsWinner record;
  winner INT2;
  isko BOOL;
BEGIN
  SELECT INTO curr_user username FROM player WHERE artificial and expertstatus = expertclass;
  DELETE FROM TIP WHERE gameid = theGameID AND tournamentid = theTournament AND username = curr_user;

  SELECT INTO rsAvg round(avg(goals1)) AS goals1,
                    round(avg(goals2)) AS goals2
    FROM tip t JOIN player p on t.username = p.username WHERE not artificial AND expertstatus = expertclass 
    AND gameid = theGameID AND tournamentid = theTournament;

  IF (rsAvg.goals1 IS NULL OR rsAvg.goals2 IS NULL) THEN
    return(FALSE);
  END IF;

  SELECT INTO isko kogame FROM game WHERE gameid = theGameID;

  IF isko THEN
    IF (rsAvg.goals1 = rsAvg.goals2) THEN
      SELECT INTO rsWinner count(*) AS freq, kowinner FROM tip t JOIN player p ON p.username = t.username
        WHERE gameid = theGameID AND tournamentid = theTournament AND NOT artificial and expertstatus = expertclass 
        GROUP BY kowinner ORDER BY freq DESC LIMIT 1;
      winner := rsWinner.kowinner;
    ELSE
      IF (rsAvg.goals1 > rsAvg.goals2) THEN
        winner := '1';
      ELSE
        winner := '2';
      END IF;
    END IF;
  END IF;

  INSERT INTO tip (goals1, goals2, kowinner, gameid, tournamentid, username, tiptime)
    VALUES (rsAvg.goals1, rsAvg.goals2, winner, theGameID, theTournament, curr_user, now() AT TIME ZONE 'Europe/Paris');
  RETURN(TRUE);
END;
$$
LANGUAGE plpgsql;

---------------------------------
CREATE OR REPLACE FUNCTION updateRational(theTournament INT2, theGameID INT2) RETURNS BOOL AS $$
DECLARE
  rsRank record;
  goal1 INT2;
  goal2 INT2;
  winner INT2;
  isko BOOL;
BEGIN
  DELETE FROM TIP WHERE gameid = theGameID AND username = 'rational';

  SELECT INTO rsRank (SELECT fifaranking FROM team WHERE team = team1) AS rank1, 
                     (SELECT fifaranking FROM team WHERE team = team2) AS rank2 FROM game g 
                     WHERE gameid = theGameID AND tournamentid = theTournament;

  IF (rsRank.rank1 IS NULL OR rsRank.rank2 IS NULL) THEN
    return(FALSE);
  END IF;

  goal1 := 0;
  goal2 := 0;
  IF (rsRank.rank1 < rsRank.rank2) THEN
    goal1 := round((rsRank.rank2 - rsRank.rank1)/10);
  ELSE
    goal2 := round((rsRank.rank1 - rsRank.rank2)/10);
  END IF; 

  SELECT INTO isko kogame FROM game WHERE gameid = theGameID;

  IF isko THEN
    IF (goal1 = goal2) THEN
      IF (rsRank.rank1 < rsRank.rank2) THEN
        winner := '1';
      ELSE
        winner := '2';
      END IF;
    END IF;
  END IF;

  INSERT INTO tip (goals1, goals2, kowinner, gameid, tournamentid, username, tiptime)
    VALUES (goal1, goal2, winner, theGameID, theTournament, 'rational', now() AT TIME ZONE 'Europe/Paris');
  RETURN(TRUE);
END;
$$
LANGUAGE plpgsql;

---------------------------------
DROP FUNCTION usertimestat(interval);
DROP TYPE usertimestat;
CREATE TYPE usertimestat AS (
  rank INT8,
  username TEXT,
  tournamentid INT2,
  name TEXT, 
  firstname TEXT,
  nationality TEXT, 
  points INT8, 
  evalgames INT8,
  avgpoints REAL
);

CREATE OR REPLACE FUNCTION usertimestat(interval) RETURNS SETOF usertimestat AS $$ 
SELECT rank() OVER (ORDER BY points DESC), *, points::REAL/evalgames::REAL as avgpoints FROM 
              (SELECT t.username, t.tournamentid, name, firstname, nationality, sum(points) AS points, count(points) AS evalgames 
              FROM tip t JOIN player p ON p.username = t.username WHERE gameid IN 
                (SELECT gameid FROM game WHERE starttime AT TIME ZONE 'Europe/Paris'> now() AT TIME ZONE 'Europe/Paris' - $1 AND starttime AT TIME ZONE 'Europe/Paris'< now() AT TIME ZONE 'Europe/Paris')
              GROUP BY t.username, t.tournamentid, name, firstname, nationality ORDER BY points DESC) AS r;
$$
LANGUAGE sql;

---------------------------------
