library(RPostgreSQL)
library(pheatmap)
library(ggplot2)

source("settings.R")

connectPostgresql <- function() {
  drv <- dbDriver("PostgreSQL")
  dbConnect(drv, dbname = db$dbname, host = db$host, port = db$port, 
                 user = db$user, password = db$password)
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
  sql <- paste("SELECT rank() over (ORDER BY grouppoints + kopoints desc), firstname, name, ",
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

getRankingLastGames <- function(nGames) {
  sql <- paste("SELECT rank() OVER (order by sum(points) desc), firstname, name, nationality, sum(points) as points ",
               "FROM tipview NATURAL JOIN player WHERE gameid > ",
               "(SELECT max(gameid) as g FROM game ",
               "WHERE regulartimegoals1 IS NOT NULL AND regulartimegoals2 IS NOT NULL) -", nGames, 
               "GROUP BY name, firstname, nationality;")
  rank <- getPostgresql(sql)
  colnames(rank) <- c("Rank", "Firstname", "Name", "Nationality", "Points")
  return(rank)
}

getTeamRanking <- function() {
  sql <- paste0("SELECT rank() OVER (partition BY initialgroup ORDER BY ",
                "points desc, goalsfor-goalsagainst desc, goalsfor desc, goalsagainst), ",
                "team, uefaranking, initialgroup, played, won, draw, loss, ",
                "goalsfor, goalsagainst, goalsfor - goalsagainst as goaldiff, points from groupphasetable")
  teamrank <- getPostgresql(sql)
  colnames(teamrank) <- c("Rank", "Team", "UEFA-Rank", "Group", "Games", "Won", "Draw", "Loss", "GF", "GA", "GD", "PTS")
  return(teamrank)
}

getMissingTips <- function() {
  sql <- paste0("select firstname, name, gameid, team1, team2, starttime FROM player p, game g WHERE ", 
                "(select count(*) FROM tipview t WHERE t.username = p.username AND t.gameid = g.gameid) = 0 ",
                "AND username <> 'average' AND gametime(starttime) = 'soon' order by starttime")
  getPostgresql(sql)
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
                    "VALUES (", gameid, ",'", user, "', now() AT TIME ZONE 'Europe/Paris', ", tipgoals1, ",", tipgoals2, ",", kowinner, ")")
      dbGetQuery(con, sql)
      return(1)
    } else {
      if ((tipgoals1 != tip$regulartimegoals1) | (tipgoals2 != tip$regulartimegoals2)) {
        sql <- paste0("UPDATE tip SET regulartimegoals1 = ", tipgoals1, ", regulartimegoals2 = ", tipgoals2, 
                      ", kowinner = ", kowinner, ", tiptime = now() AT TIME ZONE 'Europe/Paris'",
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
               username, "') t ON t.gameid = g.gameid ",
               "WHERE starttime > now() AT TIME ZONE 'Europe/Paris' ORDER BY gameid", sep ="")
  tips <- getPostgresql(sql)
  tips$starttime <- format(tips$starttime,'%Y-%m-%d %H:%M')
  tips$tipgoals1 <- formatInput(tips$gameid, "1", tips$tipgoals1)
  tips$tipgoals2 <- formatInput(tips$gameid, "2", tips$tipgoals2)
  return(tips)
}

getFutureGames <- function() {
  getPostgresql("SELECT gameid, kogame FROM game WHERE starttime > now() AT TIME ZONE 'Europe/Paris'")
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

getTipCross <- function() {
  sql <- paste0("SELECT firstname || ' ' || name as name, nationality, ",
                "to_char(tv.gameid, '00') || ' ' || team1 || '-' || team2 as gamename, ",
                "tv.regulartimegoals1 - tv.regulartimegoals2 as diff FROM tipview tv JOIN game g ON g.gameid = tv.gameid ",
                "JOIN player p on p.username = tv.username WHERE tv.regulartimegoals1 IS NOT NULL AND tv.regulartimegoals2 IS NOT NULL")
  data <- getPostgresql(sql)
  if (nrow(data) < 2) return(list(data = NULL, n = NULL))
  dn <- unique(data[, c("name", "nationality")])
  n <- dn$nationality
  names(n) <- dn$name
  list(data = tapply(data$diff, list(data$name, data$gamename), sum), n = n)
}

getHeatmap <- function(data) {
  if (is.null(data$data)) return()
  data$data <- data$data[apply(data$data, 1, function(x) sum(ifelse(is.na(x), 1, 0))) <= 3,]
  pheatmap(data$data, cluster_cols = FALSE)
}

getPCA <- function(data, mainTitle) {
  if (is.null(data$data)) return()
  data$data <- apply(data$data, 2, function(x) ifelse(is.na(x), 0, x))
  pca <- prcomp(data$data)
  Nationality <- data$n[rownames(pca$x)]
  p <- qplot(pca$x[,1], pca$x[,2], main = mainTitle, 
             xlab = "PCA1", ylab = "PCA2", label = rownames(pca$x)) 
  p + geom_point(aes(colour = Nationality), size = 6, alpha = 1) + geom_text(size = 4)
}

getNationPlot <- function(data) {
  data$totalpoints <- data$'Total Points'
  #p <- qplot(factor(Nationality), total, data = data, fill = Nationality, geom = "boxplot")
  #p <- p + geom_point(aes(colour = Nationality), size = 6, alpha = 1) 
  #p + geom_text(size = 8, label = "hello")
  p <- ggplot(data, aes(factor(Nationality), totalpoints)) + geom_boxplot(aes(fill = Nationality))
  p + coord_flip()
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

getReadyGames <- function() {
  sql <- paste0("SELECT max(gameid) as g FROM game ",
                "WHERE regulartimegoals1 IS NOT NULL AND regulartimegoals2 IS NOT NULL")
  getPostgresql(sql)$g
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
