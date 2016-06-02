CREATE OR REPLACE FUNCTION checkLogin(user1 TEXT, pass TEXT) RETURNS INT2 AS $$
  var ret = 0;
  if (user1 == pass) ret = 1;
  return ret ;
$$ LANGUAGE plv8;
