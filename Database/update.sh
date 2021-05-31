#!/bin/sh

cd /home/andreas/SoccerSucker/Database

alias d='psql EuroCup2021 postgres -h localhost'

echo "DROP TRIGGER checkTip ON tip;" | d 
cat update.sql | d 
echo "CREATE TRIGGER checkTip BEFORE INSERT OR UPDATE ON tip FOR EACH ROW EXECUTE PROCEDURE checkTip();" | d
