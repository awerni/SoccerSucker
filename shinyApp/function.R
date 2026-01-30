library(RPostgres)
#library(d3heatmap)
#library(tidyverse)
library(dplyr)
library(tidyr)
library(ggrepel)

source("settings.R")
source("translation.R")

time_zone_clause <- paste0("AT TIME ZONE '", time_zone, "'")

connectPostgresql <- function() {
  drv <- RPostgres::Postgres()
  DBI::dbConnect(drv, dbname = db$dbname, host = db$host, port = db$port,
                 user = db$user, password = db$password, sslmode = db$sslmode, bigint = "numeric")
}

disconnectPostgresql <- function(con) {
  dbDisconnect(con)
}

getPostgresql <- function(sql) {
  con <- connectPostgresql()
  if (class(con) == "try-error") stop("no connection to database")

  rs <- try(RPostgres::dbSendQuery(con, sql))
  if (class(rs) == "try-error") {
    RPostgres::dbDisconnect(con)
    stop("can not exectute sql command")
  }

  data <- try(RPostgres::dbFetch(rs, n = -1))

  if (class(rs) == "PqResult") RPostgres::dbClearResult(rs)
  RPostgres::dbDisconnect(con)
  if (class(data) == "try-error") stop("can not retrieve data")

  return(data)
}

getNumberOfGames <- function(tournamentid) {
  sql <- paste0("SELECT count(*) AS num FROM game WHERE tournamentid = ", tournamentid)
  getPostgresql(sql)$num
}

getNumberOfPlayers <- function() {
  getPostgresql("SELECT count(*) AS num FROM player")$num
}

getRanking <- function(showplayers, tournamentid) {
  expert_order <- c("beginner", "intermediate", "expert")
  sql <- paste("SELECT rank() over (ORDER BY grouppoints + kopoints desc), firstname, name,",
               "nationality, expertstatus, grouppoints + kopoints AS points, grouppoints, kopoints, ",
               "evalgroupgames + evalkogames AS games, ",
               "CASE WHEN evalgroupgames + evalkogames = 0 THEN 0 ",
               "ELSE (grouppoints + kopoints)/(evalgroupgames::REAL + evalkogames::REAL) ",
               "END AS pointsPerGame FROM userstat WHERE tournamentid = ", tournamentid)
  if (showplayers == "human") sql <- paste(sql, "AND NOT artificial")
  if (showplayers == "bot") sql <- paste(sql, "AND artificial")
  rank <- getPostgresql(sql)
  if (nrow(rank) == 0) return()
  if (max(rank$games) > 0) rank <- rank |> filter(games > 0)
  rank <- rank |> mutate(pointspergame = signif(pointspergame, digits = 2)) |>
    mutate(expertstatus = factor(expert_order[expertstatus], levels = expert_order))
  colnames(rank) <- c("Rank", "Firstname", "Name", "Nationality", "Expert-Status", "Total Points", "Group-Points",
                      "KO-Points", "Games", "Points/Game")
  return(rank)
}

getRankingLastGames <- function(nGames, showplayers, tournamentid) {
  sql <- paste0("SELECT rank() OVER (order by sum(points) desc), name, nationality, sum(points) as points ",
                "FROM tipview tv WHERE points IS NOT NULL AND tournamentid = ", tournamentid, " AND gameid IN ",
                "(SELECT gameid FROM game WHERE tournamentid = ", tournamentid, " ",
                "AND regulartimegoals1 IS NOT NULL and regulartimegoals2 IS NOT NULL ",
                "ORDER BY starttime DESC, gameid DESC LIMIT ", nGames, ") ")

  if (showplayers == "human") sql <- paste(sql, "AND NOT artificial")
  if (showplayers == "bot") sql <- paste(sql, "AND artificial")
  sql <- paste(sql, " GROUP BY name, nationality")
  rank <- getPostgresql(sql)
  if (nrow(rank) == 0) return()
  colnames(rank) <- c("Rank", "Name", "Nationality", "Points")
  return(rank)
}

getTeamRanking <- function(tournamentid) {
  sql <- paste0("SELECT rank() OVER (partition BY initialgroup ORDER BY ",
                "points desc, goalsfor-goalsagainst desc, goalsfor desc, goalsagainst), ",
                "team, fifaranking, initialgroup, played, won, draw, loss, ",
                "goalsfor, goalsagainst, goalsfor - goalsagainst as goaldiff, points FROM groupphasetable ",
                "WHERE tournamentid = ", tournamentid)
  teamrank <- getPostgresql(sql)
  colnames(teamrank) <- c("Rank", "Team", "FIFA-Rank", "Group", "Games", "Won", "Draw", "Loss", "GF", "GA", "GD", "PTS")
  return(teamrank)
}

getMissingTips <- function(tournamentid) {
  sql <- paste0("SELECT firstname, name, gameid, team1, team2, starttime ", time_zone_clause, " AS starttime ",
                "FROM player p, game g WHERE ",
                "tournamentid = ", tournamentid, " AND NOT artificial AND ",
                "(select count(*) FROM tipview t WHERE t.username = p.username AND t.gameid = g.gameid AND g.tournamentid = t.tournamentid) = 0 ",
                "AND gametime(starttime ", time_zone_clause, ") = 'soon' ORDER BY starttime, gameid DESC")
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
  sql <- paste0("INSERT INTO player (username, firstname, name, nationality, expertstatus, artificial) VALUES ",
                "('", user, "','", firstname, "','", surname, "','", nationality, "',", expertstatus, ", FALSE)")
  getPostgresql(sql)
  return(TRUE)
}

upsertTip <- function(user, tiptable, tournamentid) {
  con <- connectPostgresql()
  ret <- sapply(tiptable, function(tt) {
    gameid <- tt[["g"]]
    tipgoals1 <- tt[["g1"]]
    tipgoals2 <- tt[["g2"]]
    kowinner <- ifelse(is.na(tt[["kowinner"]]), "NULL", tt[["kowinner"]])
    sql <- paste0("SELECT goals1, goals2, kowinner ",
                  "FROM tip WHERE username = '", user, "' AND gameid = ", gameid, " AND tournamentid = ", tournamentid)
    tip <- dbGetQuery(con, sql)
    if (nrow(tip) == 0) {
      sql <- paste0("INSERT INTO tip (gameid, tournamentid, username, tiptime, goals1, goals2, kowinner) ",
                    "VALUES (", gameid, ",", tournamentid, ",'", user, "', now() ", time_zone_clause, ", ",
                    tipgoals1, ",", tipgoals2, ",", kowinner, ")")
      dbGetQuery(con, sql)
      return(1)
    } else {
      if ((tipgoals1 != tip$goals1) | (tipgoals2 != tip$goals2)) {
        sql <- paste0("UPDATE tip SET goals1 = ", tipgoals1, ", goals2 = ", tipgoals2,
                      ", kowinner = ", kowinner, ", tiptime = now() ", time_zone_clause, " ",
                      "WHERE username = '", user, "' AND gameid = ", gameid, " AND tournamentid = ", tournamentid)
        dbGetQuery(con, sql)
        return(1)
      } else return(0)
    }
  })
  disconnectPostgresql(con)
  return(sum(ret))
}

upsertTip2 <- function(user, tiptable, tournamentid) {
  con <- connectPostgresql()
  ret <- sapply(tiptable, function(tt) {
      gameid <- tt[["g"]]
      tipgoals1 <- tt[["g1"]]
      tipgoals2 <- tt[["g2"]]
      kowinner <- ifelse(is.na(tt[["kowinner"]]), "NULL", paste0("'", tt[["kowinner"]], "'"))
      sql <- paste0("SELECT * FROM place_tip(", tournamentid, "::INT2, ", gameid, "::INT2, '", user, "',",
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

getAllTips <- function(tournamentid, username) {
  sql <- paste0("SELECT g.gameid, g.team1, g.team2, g.kogame, ",
               "tv.goals1 as tipgoals1, tv.goals2 as tipgoals2, tv.kowinner, ",
               "city, starttime ", time_zone_clause, " AS starttime ",
               "FROM gameview g LEFT OUTER JOIN (SELECT * FROM tipview WHERE username = '",
               username, "' AND tournamentid = '", tournamentid, "') tv ON tv.gameid = g.gameid ",
               "AND tv.tournamentid = g.tournamentid ",
               "WHERE starttime > now() and g.tournamentid = ", tournamentid,
               " ORDER BY starttime, gameid DESC")
  res <- getPostgresql(sql)
  if (nrow(res) == 0) return()
  res |>
    mutate(starttime = format(starttime,'%Y-%m-%d %H:%M'),
           tipgoals1 = formatInput(gameid, "1", tipgoals1),
           tipgoals2 = formatInput(gameid, "2", tipgoals2),
           kowinner = formatInputKO(gameid, kowinner, kogame)) |>
    select(-kogame)
}

getFutureGames <- function(tournamentid) {
  getPostgresql(paste("SELECT gameid, kogame FROM game WHERE starttime > now() ",
                      "AND tournamentid = ", tournamentid, " ORDER by starttime, gameid"))
}

formatInput <- function(gameid, team, goals) {
  paste0("<input id='g", gameid, "t", team, "'class='shiny-input-number' type='number' value='",
         goals, "' min = '0' max = '10'>")
}

formatInputKO <- function(gameid, winner, kogame) {
  ifelse(kogame,
    paste0("<input id='g", gameid, "w","' class='shiny-input-checkbox' type='number' value='",
            winner, "' min = '1' max = '2'>")
    , "")
}

getResultCross <- function(showplayers, tournamentid) {
  sql <- paste0("SELECT name, nationality, ",
                "to_char(tv.gameid, '00') || ' ' || team1 || '-' || team2 ",
                "|| ' (' || COALESCE(g.overtimegoals1, g.regulartimegoals1) || ':' || ",
                "COALESCE(g.overtimegoals2, g.regulartimegoals2) || ')' as gamename, ",
                "points FROM tipview tv ",
                "JOIN game g ON g.gameid = tv.gameid AND g.tournamentid = tv.tournamentid ",
                "WHERE points IS NOT NULL AND tv.tournamentid = ", tournamentid, " ")
  if (showplayers == "human") sql <- paste(sql, "AND NOT artificial")
  if (showplayers == "bot") sql <- paste(sql, "AND artificial")
  sql <- paste(sql, "ORDER by starttime, name")
  data <- getPostgresql(sql)
  if (nrow(data) < 2) return(list(data = NULL, n = NULL))

  data_cross <- data |>
    select(name, gamename, points) |>
    pivot_wider(names_from = gamename, values_from = points) |>
    tibble::column_to_rownames("name")

  dn <- unique(data[, c("name", "nationality")])
  n <- dn$nationality
  names(n) <- dn$name
  #list(data = tapply(data$points, list(data$name, data$gamename), sum), n = n)
  list(data = data_cross, n = n)
}

getTipCross <- function(showplayers, tournamentid) {
  sql <- paste0("SELECT name, nationality, ",
                "to_char(tv.gameid, '00') || ' ' || team1 || '-' || team2 as gamename, ",
                "tv.goals1 - tv.goals2 as diff FROM tipview tv JOIN game g ON g.gameid = tv.gameid AND g.tournamentid = tv.tournamentid ",
                "WHERE tv.goals1 IS NOT NULL AND tv.goals2 IS NOT NULL AND tv.tournamentid = ", tournamentid, " ")
  if (showplayers == "human") sql <- paste(sql, "AND NOT artificial")
  if (showplayers == "bot") sql <- paste(sql, "AND artificial")
  sql <- paste(sql, "ORDER by starttime, tv.gameid DESC, name")
  data <- getPostgresql(sql)
  if (nrow(data) < 2) return(list(data = NULL, n = NULL))

  data_cross <- data |>
    select(name, gamename, diff) |>
    pivot_wider(names_from = gamename, values_from = diff) |>
    tibble::column_to_rownames("name")

  dn <- unique(data[, c("name", "nationality")])
  n <- dn$nationality
  names(n) <- dn$name
  #list(data = tapply(data$diff, list(data$name, data$gamename), sum), n = n)
  list(data = data_cross, n = n)
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

  pca_data <- data.frame(pca$x) |>
    select(PC1, PC2) |>
    tibble::rownames_to_column("player")

  ggplot(pca_data, aes(x = PC1, y = PC2, label = player)) +
    theme_gray() + theme(text = element_text(size = 20)) +
      geom_point(aes(colour = Nationality), size = 6, alpha = 1) + geom_text_repel(size = 5) +
      ggtitle(mainTitle)
}

getNationPlot <- function(data) {
  if (is.null(data)) return()
  data <- data |> rename(totalpoints = `Total Points`)
  ggplot(data, aes(factor(Nationality), totalpoints)) + geom_boxplot(aes(fill = Nationality)) +
    theme_gray() + scale_x_discrete(name = "") + scale_y_continuous(name = "Total Points") +
    coord_flip() + theme(text = element_text(size = 20))
}

getExpertPlot <- function(data) {
  if (is.null(data)) return()
  data <- data |> rename(expertstatus = `Expert-Status`, totalpoints = `Total Points`) |> filter(!is.na(expertstatus))
  ggplot(data, aes(factor(expertstatus), totalpoints)) + geom_boxplot(aes(fill = expertstatus)) +
    theme_gray() + scale_x_discrete(name = "") + scale_y_continuous(name = "Total Points") +
    coord_flip() + theme(text = element_text(size = 20))
}

getPlayerResult <- function(username, tournamentid) {
  sql <- paste0("SELECT tv.gameid as game, team1 || '-' || team2 AS teams, ",
                "starttime ", time_zone_clause, " AS time, ",
                "tv.goals1 || ':' || tv.goals2 as tip, ",
                "COALESCE(g.overtimegoals1, g.regulartimegoals1) || ':' || ",
                "COALESCE(g.overtimegoals2, g.regulartimegoals2) AS result, ",
                "winner, points FROM tipview tv JOIN game g ON g.gameid = tv.gameid AND g.tournamentid = tv.tournamentid ",
                "WHERE tv.username = '", username, "' AND tv.tournamentid = ", tournamentid, " AND points IS NOT NULL ",
                "ORDER BY g.starttime DESC, tv.gameid")
  getPostgresql(sql)
}

getGameResults <- function(showplayers, tournamentid) {
  sql_filter <- ""
  if (showplayers == "human") sql_filter <- "AND NOT artificial"
  if (showplayers == "bot") sql_filter <- "AND artificial"

  sql <- paste0("SELECT gameid, team1, team2, city, starttime ", time_zone_clause, " AS starttime, ",
                "regulartimegoals1 || ':' || regulartimegoals2 || ' (' || halftimegoals1 || ':' || halftimegoals2 || ')' AS result, ",
                "overtimegoals1 || ':' || overtimegoals2 AS overtimeresult, ",
                "penaltygoals1 || ':' || penaltygoals2 AS penaltyresult, ",
                "(SELECT avg(points) FROM tipview WHERE gameid = gv.gameid AND tournamentid = gv.tournamentid ",
                sql_filter, ") AS avg_points ",
                "FROM gameview gv WHERE tournamentid = ", tournamentid, " AND (starttime < now() OR ",
                "gametime(starttime ", time_zone_clause, ") = 'soon') ORDER BY starttime DESC, gameid")

  ret <- getPostgresql(sql)

  ret <- ret |>
    mutate(result = case_when(
      !is.na(penaltyresult) ~ paste(penaltyresult, overtimeresult, result, sep = ", "),
      !is.na(overtimeresult) ~ paste0(overtimeresult, ", ", result),
      TRUE ~ result
    )) |> 
    select(-overtimeresult, -penaltyresult)

  names(ret) <- c("Game", "Team1", "Team2", "City", "Start time", "Result", "Avg points")
  return(ret)
}

getPastGames <- function(tournamentid = NULL) {
  if (is.null(tournamentid)) return()
  sql <- paste0("SELECT gameid, team1 || ':' || team2 as teams, ",
                "COALESCE(regulartimegoals1::TEXT, '?') || ':' || COALESCE(regulartimegoals2::TEXT, '?') || ",
                "' (' || halftimegoals1 || ':' || halftimegoals2 || ')' AS result, ",
                "overtimegoals1 || ':' || overtimegoals2 AS overtimeresult, ",
                "penaltygoals1 || ':' || penaltygoals2 AS penaltyresult ",
                "FROM gameview gv WHERE tournamentid = ", tournamentid, " AND starttime ", time_zone_clause, " < ",
                "now() ", time_zone_clause, " ORDER BY starttime DESC, gameid")

  g <- getPostgresql(sql)
  if (nrow(g) == 0) return()

  g <- g |>
    mutate(result = case_when(
      !is.na(penaltyresult) ~ paste(penaltyresult, overtimeresult, result, sep = ", "),
      !is.na(overtimeresult) ~ paste0(overtimeresult, ", ", result),
      TRUE ~ result
    ))

  ret <- g$gameid
  names(ret) <- paste(g$teams, g$result)
  return(ret)
}

getTips <- function(tournamentid, gameid, showplayers) {
  sql_filter <- ""
  if (showplayers == "human") sql_filter <- " AND NOT artificial"
  if (showplayers == "bot") sql_filter <- " AND artificial"

  sql <- paste0("SELECT rank() OVER (order by points desc) as rank, name, goals1, goals2, winner, kowinner, ",
                "CASE WHEN artificial THEN starttime ELSE tiptime END AS time, points ",
                "FROM tipview tv JOIN game g ON tv.gameid = g.gameid AND tv.tournamentid = g.tournamentid ",
                "WHERE tv.gameid = ", gameid, " AND tv.tournamentid = ", tournamentid,
                sql_filter, " ORDER BY points DESC, name")
  tips <- getPostgresql(sql)
  if (nrow(tips) > 0 ) tips |> rename(Rank = rank, Time = time, Name = name, Points = points)
}

getTeams <- function(tournamentid, gameid) {
  sql <- paste("SELECT team1, team2 FROM game WHERE gameid =", gameid, " AND tournamentid =", tournamentid)
  teams <- getPostgresql(sql)
  if (nrow(teams) > 0) teams |> as.list()
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

getCumulativeRanking <- function(showplayers, tournamentid){
  sql <- paste0("SELECT tv.gameid, team1 || ':' || team2 || ' (' || tv.gameid || ')' AS game, username, name, ",
                "points FROM tipview tv JOIN game g ON g.gameid = tv.gameid AND g.tournamentid = tv.tournamentid ",
                "WHERE tv.tournamentid = ", tournamentid, " AND points IS NOT NULL")
  if (showplayers == "human") sql <- paste(sql, "AND NOT artificial")
  if (showplayers == "bot") sql <- paste(sql, "AND artificial")
  sql <- paste(sql, "ORDER by username, starttime, tv.gameid DESC")
  data <- getPostgresql(sql)
  if (nrow(data) < 2) return()
  if (length(unique(data$gameid)) == 1) return()

  data <- data |>
    group_by(username) |>
    mutate(totalPoints = cumsum(points)) |>
    ungroup() |>
    select(-username)

  myLevels <- data |>
    group_by(name) |>
    summarise(allPoints = max(totalPoints), .groups = "drop") |>
    arrange(desc(allPoints)) |>
    pull(name)

  data |>
    mutate(name = factor(name, myLevels))
}

getGameBetPlot <- function(tips, teams) {
  if (!tips |> select(kowinner) |> is.na() |> all()) {
    tips <- tips |> mutate(winner = kowinner)
  }
  ggplot(tips, aes(x = goals2, y = goals1, label = Name, color = winner)) + geom_point() + theme(text = element_text(size = 14)) +
    geom_text_repel(size = 5) +
    geom_abline(intercept = 0, slope = 1) +
    xlim(0, max(tips$goals2, 2)) +
    ylim(0, max(tips$goals1, 2)) +
    xlab(teams$team2) + ylab(teams$team1)
}

getCumulativePlot <- function(data, numPlayer, showMe, user) {
  if (numPlayer < length(levels(data$name))) {
    if (showMe) {
      un <- unique(c(user$fullname, levels(data$name)[1:numPlayer]))
    } else {
      un <- levels(data$name)[1:numPlayer]
    }
    data <- data |> filter(name %in% un)
  }

  my_size <- 14
  nGames <- unique(data$gameid) |> length()
  if (nGames < 20) my_size <- 25
  if (nGames < 30) my_size <- 20

  ggplot(data, aes(x = forcats::fct_inorder(game), y = totalPoints, colour = name, group = name)) +
    geom_line() + theme_gray(base_size = 12) +
    theme(text = element_text(size = my_size),
                 axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 0)
                 #plot.background = element_rect(fill = "#AAAAAA"
                 ) + xlab("")
}

getReadyGames <- function(tournamentid) {
  sql <- paste0("SELECT count(distinct gameid) AS g FROM game ",
                "WHERE regulartimegoals1 IS NOT NULL AND regulartimegoals2 IS NOT NULL ",
                "AND tournamentid = ", tournamentid)
  getPostgresql(sql)$g
}

getTournament <- function() {
  sql <- "SELECT tournamentid, tournamentname FROM tournament"
  d <- getPostgresql(sql)
  ret <- d$tournamentid
  names(ret) <- d$tournamentname
  return(ret)
}

getTournamentName <- function(tournamentid) {
  sql <- "SELECT tournamentname FROM tournament"
  sql <- paste0(sql, " WHERE tournamentid = ", tournamentid)
  getPostgresql(sql)$tournamentname
}

trans <- function(keyword, currlang = lang) labeltrans[[keyword]][[currlang]]

getGameResult <- function(gameid) {
  h <- new_handle()
  handle_setheaders(h, "X-Auth-Token" = footballdatakey)
  req <- curl_fetch_memory("http://api.football-data.org/v1/soccerseasons/424", handle = h)
  fromJSON(rawToChar(req$content))
}

getTeamBetPoints <- function(showplayers, tournamentid) {
  #myClause <- ifelse(kogame, " NOT ", "")
  sql2 <- ""
  if (showplayers == "human") sql2 <- "WHERE NOT artificial"
  if (showplayers == "bot") sql2 <- "WHERE artificial"
  sql1 <- paste0("tipview tv JOIN gameview gv ON tv.gameid = gv.gameid AND ",
                 "tv.tournamentid = gv.tournamentid AND tv.tournamentid = ", tournamentid, " ")
  sql <- paste0("SELECT sum(points)/count(distinct(gameid)) AS avgpoints, sum(points), ",
                "count(distinct(gameid)) as games, team, kogame FROM ",
                "(SELECT points, team1 AS team, tv.gameid, kogame FROM ", sql1,  sql2,
                " UNION ALL ",
                "SELECT points, team2 AS team, tv.gameid, kogame FROM ", sql1, sql2, ") t ",
                "WHERE points IS NOT NULL GROUP BY team, kogame")
  data <- getPostgresql(sql)
  if (nrow(data) == 0) return()
  data <- data |>
    mutate(game = ifelse(kogame, "KO-Game", "Group-Phase-Game"))

  ggplot(data, aes(team, avgpoints, fill = game)) + geom_bar(stat="identity", position="dodge") +
    theme_gray() + theme(text = element_text(size = 20), axis.text.x = element_text(angle = 90, hjust = 1, vjust = +0.5)) +
    scale_y_continuous(name="Average Points per Game") + xlab("") +
    geom_text(data = data, aes(team, avgpoints, group = game, label = games),
              vjust=1.5, position=position_dodge(.9), size = 4)
}

# -------------------------------

insertRandomTips <- function(tournamentid) {
   user <- getPostgresql("SELECT username FROM player WHERE NOT artificial")$username
   sql <- paste0("SELECT gameid FROM game WHERE NOT kogame AND tournamentid = ", tournamentid)
   gameids <- getPostgresql(sql)$gameid
   sapply(user, function(u) {
     tiptable <- lapply(gameids, function(g) {
       c(g = g, g1 = sample(0:5,1), g2 = sample(0:5, 1), kowinner = NA)
     })
     upsertTip2(u, tiptable, tournamentid)
   })
   sql <- gsub("NOT ", "", sql)
   gameids <- getPostgresql(sql)$gameid
   if (length(gameids) == 0) return()
   sapply(user, function(u) {
     tiptable <- lapply(gameids, function(g) {
       g1tip <- sample(0:5,1)
       g2tip <- sample(0:5,1)
       kow <- ifelse(g1tip > g2tip, 1, 2)
       if (g1tip == g2tip) kow = sample(1:2, 1)
       c(g = g, g1 = g1tip, g2 = g2tip, kowinner = kow)
     })
     upsertTip2(u, tiptable, tournamentid)
   })
   return()
}

insertRandomGameResults <- function(tournamentid) {
  con <- connectPostgresql()
  sapply(1:24, function(g) {
    sql <- paste0("UPDATE game SET regulartimegoals1 =", sample(0:5, 1), 
                  ", regulartimegoals2 = ", sample(0:5, 1), 
                  " WHERE gameid = ", g, "AND tournamentid = ", tournamentid)                  
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
  data <- data |> mutate(team = gsub("'", "", team)) |>
    mutate(team = gsub("Korea Republic", "South Korea", team)) |>
    mutate(sql = paste0("UPDATE team set fifaranking = ", rank, " WHERE team = '", team, "';"))

  con <- connectPostgresql()
  sapply(data$sql, function(s) {
    dbGetQuery(con, s)
  })
  disconnectPostgresql(con)
  return()
}
