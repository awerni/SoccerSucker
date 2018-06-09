DROP TRIGGER checkTip ON tip;
CREATE OR REPLACE FUNCTION checkTip() RETURNS "trigger" AS $$
DECLARE
  rsGame record;
  isKogame BOOL;
BEGIN
  SELECT INTO rsGame kogame, starttime < now() AT TIME ZONE 'Europe/Paris' as started FROM game WHERE gameid = NEW.gameid;

  IF rsGame.started THEN
    RETURN OLD;
  END IF;

  SELECT INTO NEW.tiptime now() AT TIME ZONE 'Europe/Paris'; 

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
  PERFORM updateMrAverage(NEW.gameid);
  PERFORM updateExpert(NEW.gameid, 1::INT2);
  PERFORM updateExpert(NEW.gameid, 2::INT2);
  PERFORM updateExpert(NEW.gameid, 3::INT2);
  UPDATE tip SET points = getTipPoints(gameid, username) WHERE gameid = NEW.gameid;
  RETURN NEW;
END;
$$ LANGUAGE 'plpgsql';

CREATE TRIGGER addPoints
  AFTER UPDATE
  ON game
  FOR EACH ROW
    EXECUTE PROCEDURE addPoints();
