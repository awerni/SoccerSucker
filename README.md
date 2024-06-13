# SoccerSucker
Football/Soccer tournament betting software for groups up to ~100 people.

It is based on R/Shiny and a Postgresql database backend.

## Installation
* create the database (e.g. at [Supabase](https://supabase.com/) or [Tembo](https://tembo.io/))
* create the database structure and insert all required information (Database/command | psql database user -h server)
* create a settings.R out of the settings_template.R file with database credentials
* create two docker images (use Dockerfile_shiny and Dockerfile) and put it on a cloud service.
* the player/user authentication has several options
    * LDAP
    * passwords using the MD5 hash function. 
        * Put the md5 hashed password into table gameuser of the postgresql database.
        * and add the checkLogin() stored procedure to your database (storedproceduresPassword_plpgsql.sql)

See the [Wiki](https://github.com/awerni/SoccerSucker/wiki) for details

![SoccerSuckerRank](https://user-images.githubusercontent.com/10331094/121005319-fb948f00-c78f-11eb-8349-c8f260abbd3b.png)
![SoccerSuckerLineRank](https://user-images.githubusercontent.com/10331094/121005335-fe8f7f80-c78f-11eb-9074-3624e65c6973.png)
![SoccerSuckerHeatmap](https://user-images.githubusercontent.com/10331094/121005349-02230680-c790-11eb-8275-f5b5abac78f3.png)
