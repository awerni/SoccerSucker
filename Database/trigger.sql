DROP TRIGGER checkTip ON tip;
CREATE OR REPLACE FUNCTION checkTip() RETURNS "trigger" AS $$
DECLARE
  rsGame record;
  tipChanged BOOL;
  playerArtificial BOOL;
BEGIN
  SELECT INTO rsGame kogame, starttime < now() as started
    FROM game WHERE gameid = NEW.gameid AND tournamentid = NEW.tournamentid;

  tipChanged := (NEW.goals1 IS DISTINCT FROM OLD.goals1 OR NEW.goals2 IS DISTINCT FROM OLD.goals2 OR NEW.kowinner IS DISTINCT FROM OLD.kowinner);
  playerArtificial := (SELECT artificial FROM player WHERE username = NEW.username);

  IF rsGame.started AND tipChanged AND NOT playerArtificial THEN
    --RETURN OLD;
    RAISE EXCEPTION 'Cannot change tip after game has started';
  END IF;

  IF tipChanged OR TG_OP = 'INSERT' THEN
    SELECT INTO NEW.tiptime now();
  END IF;

  IF rsGame.kogame THEN
    IF (NEW.goals1 > NEW.goals2) THEN
      NEW.kowinner := '1';
      RETURN NEW;
    END IF;
    IF (NEW.goals1 < NEW.goals2) THEN
      NEW.kowinner := '2';
      RETURN NEW;
    END IF;

    IF (NEW.kowinner IS NULL) THEN
      RETURN OLD;
    ELSE
      IF (NEW.kowinner = '1' OR NEW.kowinner = '2') THEN
        RETURN NEW;
      ELSE
        RETURN OLD;
      END IF;
    END IF;
  ELSE
    NEW.kowinner := NULL;
    RETURN NEW;
  END IF; 
END;
$$ LANGUAGE 'plpgsql';

CREATE TRIGGER checkTip
  BEFORE INSERT OR UPDATE
  ON tip
  FOR EACH ROW
    EXECUTE PROCEDURE checkTip();
------------------------
--DROP TRIGGER setAverage ON tip;
--CREATE OR REPLACE FUNCTION setAverage() RETURNS "trigger" AS $$
--BEGIN
--  PERFORM updateMrAverage(NEW.gameid);
--  RETURN NEW;
--END;
--$$ LANGUAGE 'plpgsql';

--CREATE TRIGGER setAverage
--  AFTER INSERT OR UPDATE
--  ON tip
--  FOR EACH ROW
--    EXECUTE PROCEDURE setAverage();

----------------------
DROP TRIGGER addPoints ON game;
CREATE OR REPLACE FUNCTION addPoints() RETURNS "trigger" AS $$
BEGIN
  PERFORM updateMrAverage(NEW.tournamentid, NEW.gameid);
  PERFORM updateExpert(NEW.tournamentid, NEW.gameid, 1::INT2);
  PERFORM updateExpert(NEW.tournamentid, NEW.gameid, 2::INT2);
  PERFORM updateExpert(NEW.tournamentid, NEW.gameid, 3::INT2);
  PERFORM updateRational(NEW.tournamentid,NEW.gameid);
  UPDATE tip SET points = getTipPoints(tournamentid, gameid, username) WHERE gameid = NEW.gameid AND tournamentid = NEW.tournamentid;
  RETURN NEW;
END;
$$ LANGUAGE 'plpgsql';

CREATE TRIGGER addPoints
  AFTER UPDATE
  ON game
  FOR EACH ROW
    EXECUTE PROCEDURE addPoints();
