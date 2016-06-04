CREATE OR REPLACE FUNCTION checkLogin(user1 TEXT, pass TEXT) RETURNS INT2 AS $$
  var ret = 0;
  var sql = "SELECT pass = md5('" + pass + "') as passok FROM player WHERE username = '" + user1 + "'";
  var bPass = plv8.execute(sql);
  if (bPass[0]['passok']) ret = 1;
  return ret;
$$ LANGUAGE plv8;
