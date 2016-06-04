library(RPostgreSQL)
library(pheatmap)
library(ggplot2)

connectPostgresql <- function() {
  drv <- dbDriver("PostgreSQL")
  dbConnect(drv, dbname = "EuroCup2016", host = "charlotte", user = "postgres", password = "")
#  dbConnect(drv, dbname = "EuroCup2016", host = "vie-bio-postgres", user = "writer", password = "writer")
#  dbConnect(drv,
#            dbname   = 'dbname=hgkvdmno sslmode=require',
#            host     = 'pellefant-02.db.elephantsql.com',
#            port     = 5432,
#            user     = 'hgkvdmno',
#            password = 'ueeCqHhd5OH23ikLafFa3YRdiPA9bMgX')
}

disconnectPostgresql <- function(con) {
  dbDisconnect(con)
}

getPostgresql <- function(sql) {
  con <- connectPostgresql()
  ret <- dbGetQuery(con, sql)
  disconnectPostgresql(con)
  return(ret)
}

getGames <- function() {
  getPostgresql("SELECT gameid, team1, team2, city, starttime FROM game ORDER BY gameid")
}

getNumberOfGames <- function() {
  getPostgresql("SELECT count(*) AS num FROM game")$num
}

getNumberOfPlayers <- function() {
  getPostgresql("SELECT count(*) AS num FROM player")$num
}

getRanking <- function() {
  sql <- paste("SELECT rank() over (ORDER BY grouppoints desc), firstname, name, ",
               "nationality, grouppoints + kopoints AS points, grouppoints, kopoints, ",
               "evalgroupgames + evalkogames AS games, ",
               "CASE WHEN evalgroupgames + evalkogames = 0 THEN 0 ",
               "ELSE (grouppoints + kopoints)/(evalgroupgames::REAL + evalkogames::REAL) ",
               "END AS pointsPerGame FROM userstat")
  rank <- getPostgresql(sql)
  rank$pointspergame <- signif(rank$pointspergame, digits = 2)
  colnames(rank) <- c("Rank", "Firstname", "Name", "Nationality", "Total Points", "Group-Points", 
                      "KO-Points", "Games", "Points/Game")
  return(rank)
}

checkLogin <- function(user, pass) {
  if (is.null(user) | user == "") return(list(name = "", registered = TRUE, knownuser = TRUE))
  sql <- paste0("SELECT count(*) AS e FROM player WHERE username = '", user, "'")
  pg <- getPostgresql(sql)$e > 0
  sql <- paste0("SELECT checklogin FROM checkLogin('", user, "', '", pass, "')")
  ldap <- getPostgresql(sql)$checklogin > 0 
  list(name = ifelse(ldap, user, ""), registered = pg, knownuser = (ldap))
}

registerUser <- function(user, firstname, surname, nationality) {
  if (firstname == "" | surname == "") return(FALSE)
  sql <- paste0("INSERT INTO player (username,firstname,name,nationality) VALUES ('", user, "','", 
                firstname, "','", surname, "','", nationality, "')")
  pg <- getPostgresql(sql)
  return(TRUE)
}

upsertTip <- function(user, tiptable) {
  con <- connectPostgresql()
  ret <- sapply(tiptable, function(tt) {
    gameid <- tt[["g"]]
    tipgoals1 <- tt[["g1"]]
    tipgoals2 <- tt[["g2"]]
    kowinner <- ifelse(is.na(tt[["kowinner"]]), "NULL", tt[["kowinner"]])
    sql <- paste0("SELECT regulartimegoals1, regulartimegoals2, kowinner ",
                  "FROM tip WHERE username = '", user, "' AND gameid = ", gameid)
    tip <- dbGetQuery(con, sql)
    if (nrow(tip) == 0) {
      sql <- paste0("INSERT INTO tip (gameid, username, tiptime, regulartimegoals1, regulartimegoals2, kowinner) ",
                    "VALUES (", gameid, ",'", user, "', now(), ", tipgoals1, ",", tipgoals2, ",", kowinner, ")")
      dbGetQuery(con, sql)
      return(1)
    } else {
      if ((tipgoals1 != tip$regulartimegoals1) | (tipgoals2 != tip$regulartimegoals2)) {
        sql <- paste0("UPDATE tip SET regulartimegoals1 = ", tipgoals1, ", regulartimegoals2 = ", tipgoals2, 
                      ", kowinner = ", kowinner, ", tiptime = now() ",
                      " WHERE username = '", user, "' AND gameid = ", gameid)
        dbGetQuery(con, sql)
        return(1)
      } else return(0)
    }
  })
  disconnectPostgresql(con)  
  return(sum(ret))
}

getName <- function(user) {
  sql <- paste0("SELECT firstname || ' ' || name as name FROM player WHERE username = '", user, "'")
  getPostgresql(sql)$name
}

getAllTips <- function(username) {
  sql <- paste("SELECT g.gameid, g.team1, g.team2, ",
               "t.regulartimegoals1 as tipgoals1, t.regulartimegoals2 as tipgoals2, ",
               "city, starttime ",
               "FROM gameview g LEFT OUTER JOIN (SELECT * FROM tipview WHERE username = '",
               username, "') t ON t.gameid = g.gameid WHERE starttime > now() ORDER BY gameid", sep ="")
  tips <- getPostgresql(sql)
  tips$starttime <- format(tips$starttime,'%Y-%m-%d %H:%M')
  tips$tipgoals1 <- formatInput(tips$gameid, "1", tips$tipgoals1)
  tips$tipgoals2 <- formatInput(tips$gameid, "2", tips$tipgoals2)
  return(tips)
}

getFutureGames <- function() {
  getPostgresql("SELECT gameid, kogame FROM game WHERE starttime > now()")
}

formatInput <- function(gameid, team, goals) {
  paste0("<input id='g", gameid, "t", team, "' class='shiny-bound-input' type='number' value='", 
         goals, "' min = '0' max = '10'>")
}

getResultCross <- function() {
  sql <- paste0("SELECT firstname || ' ' || name as name, nationality, ",
                "to_char(tv.gameid, '00') || ' ' || team1 || '-' || team2 ",
                "|| ' (' || g.regulartimegoals1 || ':' || g.regulartimegoals2 || ')' as gamename, ",
                "points FROM tipview tv JOIN game g ON g.gameid = tv.gameid ",
                "JOIN player p on p.username = tv.username WHERE points IS NOT NULL")
  data <- getPostgresql(sql)
  if (nrow(data) < 2) return(list(data = NULL, n = NULL))
  dn <- unique(data[, c("name", "nationality")])
  n <- dn$nationality
  names(n) <- dn$name
  list(data = tapply(data$points, list(data$name, data$gamename), sum), n = n)
}

getHeatmap <- function(data) {
  pheatmap(data$data, cluster_cols = FALSE)
}

getPCA <- function(data) {
  data$data <- apply(data$data, 2, function(x) ifelse(is.na(x), 0, x))
  pca <- prcomp(data$data)
  Nationality <- data$n[rownames(pca$x)]
  p <- qplot(pca$x[,1], pca$x[,2], main = "Principle Component Analysis based on bet game points", 
             xlab = "PCA1", ylab = "PCA2", label = rownames(pca$x)) 
  p + geom_point(aes(colour = Nationality), size = 6, alpha = 1) + geom_text(size = 4)
}

getPlayerResult <- function(username) {
  sql <- paste0("SELECT tv.gameid as game, team1 || '-' || team2 AS teams, starttime as time, ",
                "tv.regulartimegoals1 || ':' || tv.regulartimegoals2 as tip, ",
                "g.regulartimegoals1 || ':' || g.regulartimegoals2 as result, ",
                "winner, points FROM tipview tv JOIN game g ON g.gameid = tv.gameid ",
                "JOIN player p on p.username = tv.username ",
                "WHERE tv.username = '", username, "' AND points IS NOT NULL ",
                "ORDER BY tv.gameid DESC")
  getPostgresql(sql)
}

getPlayerBarplot <- function(data, limit) {
  if (nrow(data) == 0) return()
  if (nrow(data) > limit) data <- data[1:limit, ]
  data$info <- paste0(data$teams, " (tip:", data$tip, " result:", data$result, ")")
  data$info <- factor(data$info, data[order(data$game), "info"])
  p <- ggplot(data, aes(info, y = points, fill = points)) + geom_bar(colour = "black", stat = "identity") 
  p + coord_flip() + theme(text = element_text(size = 20), axis.title.y = element_blank())
  #p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

getCumulativeRanking <- function(){
  sql <- paste0("SELECT tv.gameid as game, firstname || ' ' || name as name, ",
                "points FROM tipview tv JOIN game g ON g.gameid = tv.gameid ",
                "JOIN player p on p.username = tv.username WHERE points IS NOT NULL ORDER by name, tv.gameid")
  data <- getPostgresql(sql)
  if (nrow(data) < 2) return()
  data$totalPoints <- do.call(c, tapply(data$points, data$name, FUN = cumsum))
  myLevels <- names(sort(sapply(split(data, data$name), function(x) max(x$totalPoints)), decreasing = TRUE))
  data$name <- factor(data$name, myLevels)
  return(data)
}

getCumulativePlot <- function(data, numPlayer) {
  if (is.null(data)) return()
  if (numPlayer < length(levels(data$name))) {
    data <- subset(data, name %in% levels(data$name)[1:numPlayer])
  }
  p <- ggplot(data, aes(x = game, y = totalPoints, colour = name, group = name)) + geom_line() 
  p + geom_point() + theme(text = element_text(size = 20))
}

# -------------------------------

insertRandomTips <- function() {
   user <- getPostgresql("SELECT username FROM player")$username
   sapply(user, function(u) {
     tiptable <- lapply(1:36, function(g) {
       c(g = g, g1 = sample(0:5,1), g2 = sample(0:5, 1), kowinner = NA)
     })
     upsertTip(u, tiptable)
   })
   return()
}

insertRandomGameResults <- function() {
  con <- connectPostgresql()
  sapply(1:36, function(g) {
    sql <- paste0("UPDATE game SET regulartimegoals1 =", sample(0:5, 1), ",regulartimegoals2 = ", sample(0:5, 1), " WHERE gameid = ", g)
    ret <- dbGetQuery(con, sql)
  })
  disconnectPostgresql(con)
  return()
}
