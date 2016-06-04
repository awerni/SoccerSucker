/*==============================================================*/
/* DBMS name:      PostgreSQL 7.3                               */
/* Created on:     06.06.2014 09:01:18                          */
/*==============================================================*/


/*==============================================================*/
/* Table: GAME                                                  */
/*==============================================================*/
create table GAME (
GAMEID               INT2                 not null,
TEAM1                TEXT                 null,
TEAM2                TEXT                 null,
CITY                 TEXT                 null,
STARTTIME            TIMESTAMP            null,
KOGAME               BOOL                 null,
HALFTIMEGOALS1       INT2                 null,
HALFTIMEGOALS2       INT2                 null,
REGULARTIMEGOALS1    INT2                 null,
REGULARTIMEGOALS2    INT2                 null,
OVERTIMEGOALS1       INT2                 null,
OVERTIMEGOALS2       INT2                 null,
PENALTYGOALS1        INT2                 null,
PENALTYGOALS2        INT2                 null,
constraint PK_GAME primary key (GAMEID)
);

/*==============================================================*/
/* Table: PLAYER                                                */
/*==============================================================*/
create table PLAYER (
USERNAME             TEXT                 not null,
NAME                 TEXT                 null,
FIRSTNAME            TEXT                 null,
NATIONALITY          TEXT                 null,
PASS                 TEXT                 null,
constraint PK_PLAYER primary key (USERNAME)
);

/*==============================================================*/
/* Table: TEAM                                                  */
/*==============================================================*/
create table TEAM (
TEAM                 TEXT                 not null,
FIFARANKING          INT2                 null,
INITIALGROUP         TEXT                 null,
constraint PK_TEAM primary key (TEAM)
);

/*==============================================================*/
/* Table: TIP                                                   */
/*==============================================================*/
create table TIP (
GAMEID               INT2                 not null,
USERNAME             TEXT                 not null,
TIPTIME              TIMESTAMP            not null,
REGULARTIMEGOALS1    INT2                 not null,
REGULARTIMEGOALS2    INT2                 not null,
KOWINNER             CHAR                 null,
POINTS               INT2                 null,
constraint PK_TIP primary key (GAMEID, USERNAME)
);

alter table GAME
   add constraint FK_GAME_REFERENCE_TEAM1 foreign key (TEAM1)
      references TEAM (TEAM)
      on delete restrict on update restrict;

alter table GAME
   add constraint FK_GAME_REFERENCE_TEAM2 foreign key (TEAM2)
      references TEAM (TEAM)
      on delete restrict on update restrict;

alter table TIP
   add constraint FK_TIP_REFERENCE_GAME foreign key (GAMEID)
      references GAME (GAMEID)
      on delete restrict on update restrict;

alter table TIP
   add constraint FK_TIP_REFERENCE_PLAYER foreign key (USERNAME)
      references PLAYER (USERNAME)
      on delete restrict on update restrict;

