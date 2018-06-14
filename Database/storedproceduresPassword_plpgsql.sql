CREATE OR REPLACE FUNCTION checkLogin(user1 TEXT, pass TEXT) RETURNS INT2 AS $$
DECLARE
  ret BOOL;
BEGIN
  SELECT INTO ret g.password = md5($2) FROM gameuser g WHERE username = $1;
  IF ret THEN
    RETURN 1;
  END IF;
  RETURN 0;
END;
$$ LANGUAGE plpgsql;
