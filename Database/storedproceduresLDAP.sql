CREATE OR REPLACE FUNCTION checkLogin(TEXT, TEXT) RETURNS INT2 AS $$
  use Net::LDAP;
  my $ldap = Net::LDAP->new('ldap.server.com', port => 3268);

  my ($user, $pass) = @_;
  $user = $user . '@EU';

  $mesg = $ldap->bind("$user",
                      password => "$pass",
                      version => 3 );

  my $ldap_result_code = $mesg->{resultCode};
  my $ret = 0;
  $ret = 1 if ($ldap_result_code == 0);
  return($ret);
$$ LANGUAGE plperlu;
