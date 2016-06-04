#!/bin/sh

cd /your/sql/directory
alias d='psql EuroCup2016 -h myserver'

echo "DROP TRIGGER checkTip ON tip;" | d 
cat update.sql | d 
echo "CREATE TRIGGER checkTip BEFORE INSERT OR UPDATE ON tip FOR EACH ROW EXECUTE PROCEDURE checkTip();" | d
