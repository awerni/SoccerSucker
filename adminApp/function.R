update_FIFA_ranking <- function(pool) {

  url <- "http://api.qa.fifa.com/api/v1/rankings?gender=1&count=100&language=en-GB"
  document <- jsonlite::fromJSON(url)

  fifa_data <- data.frame(
    team = sapply(document$Results$TeamName, function(x) x$Description),
    rank = document$Results$Rank,
    stringsAsFactors = FALSE
  )

  # Clean names
  fifa_data <- fifa_data |>
    mutate(team = gsub("'", "", team),
           team = gsub("IR Ira", "Ira", team)) |> # Iran and Irak
    mutate(team = case_when(
      team == "Korea Republic" ~ "South Korea",
      team == "Cabo Verde" ~ "Cape Verde",
      team == "Côte dIvoire" ~ "Ivory Coast",
      team == "Czechia" ~ "Czech Republic",
      team == "Türkiye" ~ "Turkey",
      team == "Congo DR" ~ "Congo",
      TRUE ~ team
    ))

  # Load DB teams
  db_data <- dbGetQuery(pool, "SELECT team, fifaranking FROM team")

  # Teams in DB but NOT in FIFA JSON
  db_not_in_fifa <- setdiff(db_data$team, fifa_data$team)

  # Detect ranking changes
  changed <- db_data |>
    inner_join(fifa_data, by = "team") |>
    filter(is.na(fifaranking) | fifaranking != rank) |>
    select(team, rank)

  # Update only changed rankings
  poolWithTransaction(pool, function(conn) {
    for (i in seq_len(nrow(changed))) {
      dbExecute(
        conn,
        "UPDATE team SET fifaranking = $1 WHERE team = $2",
        params = list(
          changed$rank[i],
          changed$team[i]
        )
      )
    }
  })

  # RETURN
  list(
    updated = length(changed$team), # only actually changed teams
    not_matched = db_not_in_fifa    # DB teams missing in FIFA
  )
}