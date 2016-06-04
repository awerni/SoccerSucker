CREATE OR REPLACE FUNCTION checkLogin(TEXT, TEXT) RETURNS INT2 AS $$
  my ($user, $pass) = @_;
  my $ret = 0;
  $ret = 1 if ($user eq $pass);
  return($ret);
$$ LANGUAGE plperlu;
