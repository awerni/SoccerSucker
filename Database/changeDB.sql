DROP VIEW gameview;
DROP VIEW userstat;
DROP VIEW groupphasetable;
DROP VIEW nationstat;
DROP VIEW teampointstat;
DROP view tipgame;
DROP VIEW tipview;

DROP TRIGGER checkTip ON tip;
DROP TRIGGER addPoints ON game;

ALTER TABLE tip RENAME column regulartimegoals1 to goals1;
ALTER TABLE tip RENAME column regulartimegoals2 to goals2;

DROP FUNCTION place_tip(smallint,text,smallint,smallint,character);
