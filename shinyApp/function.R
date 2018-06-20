library(RPostgreSQL)
#library(d3heatmap)
library(tidyverse)
library(ggrepel)

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

getRanking <- function(showplayers) {
  expert_order <- c("beginner", "intermediate", "expert")
  sql <- paste("SELECT rank() over (ORDER BY grouppoints + kopoints desc), firstname, name,",
               "nationality, expertstatus, grouppoints + kopoints AS points, grouppoints, kopoints, ",
               "evalgroupgames + evalkogames AS games, ",
               "CASE WHEN evalgroupgames + evalkogames = 0 THEN 0 ",
               "ELSE (grouppoints + kopoints)/(evalgroupgames::REAL + evalkogames::REAL) ",
               "END AS pointsPerGame FROM userstat")
  if (showplayers == "human") sql <- paste(sql, "WHERE NOT artificial")
  if (showplayers == "bot") sql <- paste(sql, "WHERE artificial")
  rank <- getPostgresql(sql) 
  if (nrow(rank) == 0) return()
  rank <- rank %>% mutate(pointspergame = signif(pointspergame, digits = 2)) %>%
    mutate(expertstatus = factor(expert_order[expertstatus], levels = expert_order))
  colnames(rank) <- c("Rank", "Firstname", "Name", "Nationality", "Expert-Status", "Total Points", "Group-Points", 
                      "KO-Points", "Games", "Points/Game")
  return(rank)
}

getRankingLastGames <- function(nGames, showplayers) {
  sql <- paste0("SELECT rank() OVER (order by sum(points) desc), name, nationality, sum(points) as points ",
                "FROM tipview tv WHERE points IS NOT NULL AND gameid > ",
                "(SELECT max(gameid) as g FROM tipview ",
                "WHERE points IS NOT NULL) -", nGames, " ")
  if (showplayers == "human") sql <- paste(sql, "AND NOT artificial")
  if (showplayers == "bot") sql <- paste(sql, "AND artificial")
  sql <- paste(sql, " GROUP BY name, nationality")
  rank <- getPostgresql(sql)
  if (nrow(rank) == 0) return()
  colnames(rank) <- c("Rank", "Name", "Nationality", "Points")
  return(rank)
}

getTeamRanking <- function() {
  sql <- paste0("SELECT rank() OVER (partition BY initialgroup ORDER BY ",
                "points desc, goalsfor-goalsagainst desc, goalsfor desc, goalsagainst), ",
                "team, fifaranking, initialgroup, played, won, draw, loss, ",
                "goalsfor, goalsagainst, goalsfor - goalsagainst as goaldiff, points from groupphasetable")
  teamrank <- getPostgresql(sql)
  colnames(teamrank) <- c("Rank", "Team", "FIFA-Rank", "Group", "Games", "Won", "Draw", "Loss", "GF", "GA", "GD", "PTS")
  return(teamrank)
}

getMissingTips <- function() {
  sql <- paste0("select firstname, name, gameid, team1, team2, starttime FROM player p, game g WHERE ", 
                "(select count(*) FROM tipview t WHERE t.username = p.username AND t.gameid = g.gameid) = 0 ",
                "AND NOT artificial AND gametime(starttime) = 'soon' order by starttime")
  getPostgresql(sql)
}

checkLogin <- function(user, pass) {
  if (is.null(user) | user == "") return(list(name = "", registered = TRUE, knownuser = TRUE))
  sql <- paste0("SELECT count(*) AS e FROM player WHERE username = '", user, "'")
  pg <- getPostgresql(sql)$e > 0
  
  account_confirmed <- checkAccount(user, pass)
  
  list(name = ifelse(account_confirmed, user, ""), registered = pg, knownuser = (account_confirmed))
}

registerUser <- function(user, firstname, surname, nationality, expertstatus) {
  if (firstname == "" | surname == "") return(FALSE)
  sql <- paste0("INSERT INTO player (username,firstname,name,nationality,expertstatus,artificial) VALUES ",
                "('", user, "','", firstname, "','", surname, "','", nationality, "',", expertstatus, ", FALSE)")
  getPostgresql(sql)
  return(TRUE)
}

upsertTip <- function(user, tiptable) {
  con <- connectPostgresql()
  ret <- sapply(tiptable, function(tt) {
    gameid <- tt[["g"]]
    tipgoals1 <- tt[["g1"]]
    tipgoals2 <- tt[["g2"]]
    kowinner <- ifelse(is.na(tt[["kowinner"]]), "NULL", tt[["kowinner"]])
    sql <- paste0("SELECT goals1, goals2, kowinner ",
                  "FROM tip WHERE username = '", user, "' AND gameid = ", gameid)
    tip <- dbGetQuery(con, sql)
    if (nrow(tip) == 0) {
      sql <- paste0("INSERT INTO tip (gameid, username, tiptime, goals1, goals2, kowinner) ",
                    "VALUES (", gameid, ",'", user, "', now() AT TIME ZONE 'Europe/Paris', ", tipgoals1, ",", tipgoals2, ",", kowinner, ")")
      dbGetQuery(con, sql)
      return(1)
    } else {
      if ((tipgoals1 != tip$goals1) | (tipgoals2 != tip$goals2)) {
        sql <- paste0("UPDATE tip SET goals1 = ", tipgoals1, ", goals2 = ", tipgoals2, 
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

upsertTip2 <- function(user, tiptable) {
  con <- connectPostgresql()
  ret <- sapply(tiptable, function(tt) {
      gameid <- tt[["g"]]
      tipgoals1 <- tt[["g1"]]
      tipgoals2 <- tt[["g2"]]
      kowinner <- ifelse(is.na(tt[["kowinner"]]), "NULL", paste0("'", tt[["kowinner"]], "'"))
      sql <- paste0("SELECT * FROM place_tip(", gameid, "::INT2, '", user, "',", 
                    tipgoals1, "::INT2, ", tipgoals2, "::INT2, ", kowinner, ")")
      tip <- dbGetQuery(con, sql)
      return(ifelse(tip, 1, 0))
    }
  )
  disconnectPostgresql(con)  
  return(sum(ret))
}

getName <- function(user) {
  sql <- paste0("SELECT firstname || ' ' || name as name FROM player WHERE username = '", user, "'")
  getPostgresql(sql)$name
}

getAllTips <- function(username) {
  sql <- paste0("SELECT g.gameid, g.team1, g.team2, g.kogame, ",
               "tv.goals1 as tipgoals1, tv.goals2 as tipgoals2, tv.kowinner, ",
               "city, starttime ",
               "FROM gameview g LEFT OUTER JOIN (SELECT * FROM tipview WHERE username = '",
               username, "') tv ON tv.gameid = g.gameid ",
               "WHERE starttime > now() AT TIME ZONE 'Europe/Paris' ORDER BY gameid")
  getPostgresql(sql) %>% 
    mutate(starttime = format(starttime,'%Y-%m-%d %H:%M'),
           tipgoals1 = formatInput(gameid, "1", tipgoals1),
           tipgoals2 = formatInput(gameid, "2", tipgoals2),
           kowinner = formatInputKO(gameid, kowinner, kogame)) %>%
    select(-kogame)
}

getFutureGames <- function() {
  getPostgresql(paste("SELECT gameid, kogame FROM game WHERE starttime > now() AT TIME ZONE 'Europe/Paris'",
                      "ORDER by starttime, gameid"))
}

formatInput <- function(gameid, team, goals) {
  paste0("<input id='g", gameid, "t", team, "' class='shiny-bound-input' type='number' value='", 
         goals, "' min = '0' max = '10'>")
}

formatInputKO <- function(gameid, winner, kogame) {
  ifelse(kogame, 
    paste0("<input id='g", gameid, "w","' class='shiny-bound-input' type='number' value='", 
            winner, "' min = '1' max = '2'>")
    , "")
}

getResultCross <- function(showplayers) {
  sql <- paste0("SELECT name, nationality, ",
                "to_char(tv.gameid, '00') || ' ' || team1 || '-' || team2 ",
                "|| ' (' || COALESCE(g.overtimegoals1, g.regulartimegoals1) || ':' || ",
                "COALESCE(g.overtimegoals2, g.regulartimegoals2) || ')' as gamename, ",
                "points FROM tipview tv JOIN game g ON g.gameid = tv.gameid ",
                "WHERE points IS NOT NULL")
  if (showplayers == "human") sql <- paste(sql, "AND NOT artificial")
  if (showplayers == "bot") sql <- paste(sql, "AND artificial")
  data <- getPostgresql(sql)
  if (nrow(data) < 2) return(list(data = NULL, n = NULL))
  dn <- unique(data[, c("name", "nationality")])
  n <- dn$nationality
  names(n) <- dn$name
  list(data = tapply(data$points, list(data$name, data$gamename), sum), n = n)
}

getTipCross <- function(showplayers) {
  sql <- paste0("SELECT name, nationality, ",
                "to_char(tv.gameid, '00') || ' ' || team1 || '-' || team2 as gamename, ",
                "tv.goals1 - tv.goals2 as diff FROM tipview tv JOIN game g ON g.gameid = tv.gameid ",
                "WHERE tv.goals1 IS NOT NULL AND tv.goals2 IS NOT NULL")
  if (showplayers == "human") sql <- paste(sql, "AND NOT artificial")
  if (showplayers == "bot") sql <- paste(sql, "AND artificial")
  data <- getPostgresql(sql)
  if (nrow(data) < 2) return(list(data = NULL, n = NULL))
  dn <- unique(data[, c("name", "nationality")])
  n <- dn$nationality
  names(n) <- dn$name
  list(data = tapply(data$diff, list(data$name, data$gamename), sum), n = n)
}

getHeatmap <- function(data) {
  if (is.null(data$data)) return()
  data$data <- data$data[apply(data$data, 1, function(x) sum(ifelse(is.na(x), 1, 0))) <= 20,]
  g <- pheatmap::pheatmap(data$data, color = c("#cccccc", colorRampPalette(c("blue", "yellow", "red"))(12)), cluster_cols = FALSE)
  return(g)
}

#getHeatmap <- function(data) {
#  if (is.null(data$data)) return()
#  #d <- matrix(rnorm(100), nrow = 10)
#  d3heatmap(data$data, scale="column", colors="Blues")
#}

getPCA <- function(data, mainTitle) {
  if (is.null(data$data)) return()
  data$data <- apply(data$data, 2, function(x) ifelse(is.na(x), 0, x))
  pca <- prcomp(data$data)
  Nationality <- data$n[rownames(pca$x)]
  p <- qplot(pca$x[,1], pca$x[,2], main = mainTitle, 
             xlab = "PCA1", ylab = "PCA2", label = rownames(pca$x)) + theme_gray() + theme(text = element_text(size = 20))
  p + geom_point(aes(colour = Nationality), size = 6, alpha = 1) + geom_text_repel(size = 5)
}

getNationPlot <- function(data) {
  data <- data %>% rename(totalpoints = `Total Points`)
  p <- ggplot(data, aes(factor(Nationality), totalpoints)) + geom_boxplot(aes(fill = Nationality))
  p <- p + theme_gray() + scale_x_discrete(name = "") + scale_y_continuous(name = "Total Points")
  p + coord_flip() + theme(text = element_text(size = 20)) +  theme(text = element_text(size=20))
}

getExpertPlot <- function(data) {
  data <- data %>% rename(expertstatus = `Expert-Status`, totalpoints = `Total Points`) %>% filter(!is.na(expertstatus))
  p <- ggplot(data, aes(factor(expertstatus), totalpoints)) + geom_boxplot(aes(fill = expertstatus))
  p <- p + theme_gray() + scale_x_discrete(name = "") + scale_y_continuous(name = "Total Points")
  p + coord_flip() + theme(text = element_text(size = 20)) +  theme(text = element_text(size=20))
}

getPlayerResult <- function(username) {
  sql <- paste0("SELECT tv.gameid as game, team1 || '-' || team2 AS teams, starttime as time, ",
                "tv.goals1 || ':' || tv.goals2 as tip, ",
                "COALESCE(g.overtimegoals1, g.regulartimegoals1) || ':' || ",
                "COALESCE(g.overtimegoals2, g.regulartimegoals2) as result, ",
                "winner, points FROM tipview tv JOIN game g ON g.gameid = tv.gameid ",
                "WHERE tv.username = '", username, "' AND points IS NOT NULL ",
                "ORDER BY tv.gameid DESC")
  getPostgresql(sql)
}

getGameResults <- function() {
  sql <- paste0("SELECT gameid, team1, team2, city, starttime, ",
                "regulartimegoals1 || ':' || regulartimegoals2 || ' (' || halftimegoals1 || ':' || halftimegoals2 || ')' AS result, ",
                "(SELECT avg(points) FROM tip WHERE gameid = gv.gameid) AS avg_points ",
                "FROM gameview gv WHERE starttime < now() OR gametime(starttime) = 'soon' ORDER BY starttime DESC")
  getPostgresql(sql)
}

getPlayerBarplot <- function(data, limit) {
  if (nrow(data) == 0) return()
  if (nrow(data) > limit) data <- data[1:limit, ]
  data$info <- paste0(data$teams, " (tip:", data$tip, " result:", data$result, ")")
  data$info <- factor(data$info, data[order(data$game), "info"])
  p <- ggplot(data, aes(info, y = points, fill = points)) + geom_bar(colour = "black", stat = "identity") + theme_gray() 
  p <- p + coord_flip() + theme(text = element_text(size = 20), axis.title.y = element_blank())
  p + theme(text = element_text(size=20))
  #p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

getCumulativeRanking <- function(showplayers){
  sql <- paste0("SELECT tv.gameid, team1 || ':' || team2 || ' (' || tv.gameid || ')' as game, name, ",
                "points FROM tipview tv JOIN game g ON g.gameid = tv.gameid ",
                "WHERE points IS NOT NULL ")
  if (showplayers == "human") sql <- paste(sql, "AND NOT artificial")
  if (showplayers == "bot") sql <- paste(sql, "AND artificial")
  sql <- paste(sql, "ORDER by name, tv.gameid")
  data <- getPostgresql(sql)
  if (nrow(data) < 2) return()
  if (length(unique(data$gameid)) == 1) return()
  data$totalPoints <- do.call(c, tapply(data$points, data$name, FUN = cumsum))
  myLevels <- names(sort(sapply(split(data, data$name), function(x) max(x$totalPoints)), decreasing = TRUE))
  data$name <- factor(data$name, myLevels)
  return(data)
}

getCumulativePlot <- function(data, numPlayer, user) {
  if (numPlayer < length(levels(data$name))) {
    un <- c(user$fullname, levels(data$name)[1:numPlayer])
    print(un)
    data <- data %>% filter(name %in% un)
  }
  p <- ggplot(data, aes(x = as_factor(game, gameid), y = totalPoints, colour = name, group = name)) + geom_line() 
  p <- p + theme_gray(base_size = 12) 
  p <- p + theme(text = element_text(size = 14), 
                 axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
                 #plot.background = element_rect(fill = "#AAAAAA"
                 ) 
  p + xlab("")
}

getReadyGames <- function() {
  sql <- paste0("SELECT max(gameid) as g FROM game ",
                "WHERE regulartimegoals1 IS NOT NULL AND regulartimegoals2 IS NOT NULL")
  getPostgresql(sql)$g
}

getBetStat <- function(showplayers) {
  sql <- paste("SELECT count(*), sum(points), sum(points)::FLOAT4/count(*) AS avgpoints,",
               "winner, kowinner IS NOT NULL AS kogame FROM tipview tv",
               "WHERE points IS NOT NULL ")
  if (showplayers == "human") sql <- paste(sql, "AND NOT artificial")
  if (showplayers == "bot") sql <- paste(sql, "AND artificial")
  sql <- paste(sql, "GROUP BY winner, kowinner IS NOT NULL")
  data <- getPostgresql(sql)
  if (nrow(data) == 0) return()
  data$game <- ifelse(data$kogame, "KO-Game", "Group-Phase-Game")
  p <- ggplot(data, aes(winner, avgpoints, fill = game)) + geom_bar(stat="identity", position="dodge") + theme_gray()
  p <- p + theme(text = element_text(size = 20)) 
  p + geom_text(data = data, aes(winner, avgpoints, group = game, 
                                 label = paste("# =", count, "\nsum =", sum, "\navg =", round(avgpoints, 2))),
                vjust=1.5, position=position_dodge(.9), size = 4)
}
# -------- translations ---------
labeltrans <- list(refresh = list(en = "refresh", de = "erfrischen"),
                   overallranking = list(en = "Overall Ranking", de = "Gesamtrangliste"),
                   placebets = list(en = "Place Bets", de = "Wetten"),
                   human = list(en = "humans", de = "Menschen"),
                   bot = list(en = "bot", de = "Maschinen"),
                   show = list(en = "show", de = "zeige"),
                   checkyourresults = list(en = "Check your results", de = "Überprüfe die Ergebnisse"),
                   loginText = list(en = "Please log in with your account.", de = "Bitte logge dich vorher ein."),
                   graph = list(en = "Graph", de = "Grafik"),
                   heatmap = list(en = "Heatmap", de = "Hitzekarte"),
                   lineranking = list(en="Line Ranking", de = "Linienrangfolge"),
                   nationality = list(en = "Nationality", de = "Nationalität"),
                   expertstatus = list(en = "Expert Status", de = "Expertenstatus"),
                   pcapoints = list(en = "PCA Points", de = "HKA Punkte"),
                   pcatips = list(en = "PCA Tips", de = "HKA Tipps"),
                   betstatistics = list(en = "Bet Statistics", de = "Wettstatistik"), 
                   tables = list(en = "Tables", de = "Tabellen"),
                   missingbets = list(en = "Missing Bets", de = "Fehlende Wetten"),
                   latestgames = list(en = "Latest games", de = "Letzte Spiele"),
                   gameresult = list(en = "Game results", de = "Spielresultate"),
                   teamranking = list(en = "Team ranking", de = "Mannschaftsrangliste"),
                   help = list(en = "Help", de = "Hilfe"),
                   login = list(en = "Login", de = "Einloggen"),
                   logout = list(en = "Logout", de = "Ausloggen"),
                   username = list(en = "Username:", de = "Benutzername:"),
                   password = list(en = "Password:", de = "Passwort:"),
                   sportsevent = list(en = "World Cup 2018", de = "Weltmeisterschaft 2018"),
                   firstname = list(en = "First name:", de = "Vorname:"),
                   surname = list(en = "Surname:", de = "Nachname:"),
                   register = list(en = "Register", de = "Registrieren"),
                   usernotregistered = list(en = "User not registered.", de = "Benutzer nicht registriert."),
                   wrongpassword = list(en = "Wrong password.", de = "Falsches Password."),
                   save = list(en = "Save", de = "Speichern"),
                   betstatdesc = list(en = paste("This plot shows the average points players got for their bets",
                                                 "for team1 (1), team2 (2) or draws (X) in the",
                                                 "KO-phase and group phase, respectively of the tournament."), 
                                      de = paste("Diese Grafik zeigt die durschnittlichen Punkte, die Spieler",
                                                 "für Ihre Wette für Mannschaft1 (1), Mannschaft2 (2) oder für ein Unentschieden (X)",
                                                 "in der Gruppenphase bzw. in der KO-Phase des Turniers bekommen haben.")),
                   pointsperteam = list(en = "Points per Team", de = "Punkte pro Mannschaft"),
                   pointsperteamdesc = list(en = paste("This plot shows which team is the source of points for players.",
                                                       "If a team fullfils the expectations of the players of our betting game",
                                                       "the average points per game are high, regardless if they win or loose.",
                                                       "Since the group phase games provide fewer points, the KO-phase games",
                                                       "are displayed seperately.",
                                                       "Only the teams, which made it to the KO-phase, have two bars.",
                                                       "The little number on the bar shows the number of games."),
                                            de = paste("Diese Grafik zeigt, welche Fußballmannschaft Punktequelle in unserem",
                                                       "Wettspiel ist. Wenn ein Team die Erwartungen der Wettspieler erfüllt,",
                                                       "sind die Durchschnittspunkte pro Spiel hoch, egal ob die Mannschaft",
                                                       "verloren oder gewonnen hat. Da in der KO-Phase mehr Punkte pro Spiel",
                                                       "vergeben werden, sind die Gruppen- und KO-Phase getrennt dargestellt. Nur", 
                                                       "die Mannschaften, die es in die KO-Phase geschafft haben, haben zwei Balken.",
                                                       "Die kleine Zahl am Balken zeigt die Anzahl der Spiele.")),
                   pca_point_similarity = list(en = "Principle Component Analysis based bet game point similarity",
                                               de = "Hauptkomponentenanalalyse basierend auf der Ähnlichkeit der Spiele-Punkte"),
                   pca_tip_similarity = list(en = "Principle Component Analysis based on tip similarity", 
                                             de = "Hauptkomponentenanalalyse basierend auf der Ähnlichkeit der Tipps")
                   )

trans <- function(keyword) labeltrans[[keyword]][[lang]]

getGameResult <- function(gameid) {
  h <- new_handle()
  handle_setheaders(h, "X-Auth-Token" = footballdatakey)
  req <- curl_fetch_memory("http://api.football-data.org/v1/soccerseasons/424", handle = h)
  fromJSON(rawToChar(req$content))
}

getTeamBetPoints <- function(showplayers) {
  #myClause <- ifelse(kogame, " NOT ", "")
  sql2 <- ""
  if (showplayers == "human") sql2 <- "WHERE NOT artificial"
  if (showplayers == "bot") sql2 <- "WHERE artificial"
  sql <- paste0("SELECT sum(points)/count(distinct(gameid)) AS avgpoints, sum(points), ",
                "count(distinct(gameid)) as games, team, kogame FROM ",
                "(SELECT points, team1 AS team, tv.gameid, kogame FROM tipview tv JOIN gameview gv ON tv.gameid = gv.gameid ",  sql2,
                " UNION ALL ",
                "SELECT points, team2 AS team, tv.gameid, kogame FROM tipview tv JOIN gameview gv ON tv.gameid = gv.gameid ", sql2, ") t ",
                "WHERE points IS NOT NULL GROUP BY team, kogame")
  data <- getPostgresql(sql) 
  if (nrow(data) == 0) return()
  data <- data %>% mutate(game = ifelse(kogame, "KO-Game", "Group-Phase-Game"))
  p <- ggplot(data, aes(team, avgpoints, fill = game)) + geom_bar(stat="identity", position="dodge")
  p <- p + theme_gray()
  p <- p + theme(text = element_text(size = 20), axis.text.x = element_text(angle = 90, hjust = 1, vjust = +0.5))
  p <- p + scale_y_continuous(name="Average Points per Game") + xlab("")
  p + geom_text(data = data, aes(team, avgpoints, group = game, label = games), 
                vjust=1.5, position=position_dodge(.9), size = 4) 
}

# -------------------------------

insertRandomTips <- function() {
   user <- getPostgresql("SELECT username FROM player")$username
   gameids <- getPostgresql("SELECT gameid FROM game WHERE NOT kogame")$gameid
   sapply(user, function(u) {
     tiptable <- lapply(gameids, function(g) {
       c(g = g, g1 = sample(0:5,1), g2 = sample(0:5, 1), kowinner = NA)
     })
     upsertTip(u, tiptable)
   })
   gameids <- getPostgresql("SELECT gameid FROM game WHERE kogame")$gameid
   sapply(user, function(u) {
     tiptable <- lapply(gameids, function(g) {
       g1tip <- sample(0:5,1)
       g2tip <- sample(0:5,1)
       kow <- ifelse(g1tip > g2tip, 1, 2)
       if (g1tip == g2tip) kow = sample(1:2, 1)
       c(g = g, g1 = g1tip, g2 = g2tip, kowinner = kow)
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

update_FIFA_ranking <- function() {
  url <- "http://api.qa.fifa.com/api/v1/rankings?gender=1&count=100&language=en-GB"
  document <- jsonlite::fromJSON(txt = url)

  data <- data.frame(rank = document$Results$Rank,
                     team = sapply(document$Results[, "TeamName"], function(x) x$Description))
  data <- data %>% mutate(team = gsub("'", "", team)) %>%
    mutate(team = gsub("Korea Republic", "South Korea", team)) %>%
    mutate(sql = paste0("UPDATE team set fifaranking = ", rank, " WHERE team = '", team, "';"))

  con <- connectPostgresql()
  sapply(data$sql, function(s) {
    dbGetQuery(con, s)
  })
  disconnectPostgresql(con)
  return()
}
