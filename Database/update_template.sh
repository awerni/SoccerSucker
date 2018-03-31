#!/bin/sh

cd /my/DB/dir/database 

alias d='psql WorldCup2018 postgres -h dbserver'

echo "DROP TRIGGER checkTip ON tip;" | d 
cat update.sql | d 
echo "CREATE TRIGGER checkTip BEFORE INSERT OR UPDATE ON tip FOR EACH ROW EXECUTE PROCEDURE checkTip();" | d
