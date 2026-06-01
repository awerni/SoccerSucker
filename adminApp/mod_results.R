
mod_results_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("tournament"), "Tournament", choices = NULL),
    DTOutput(ns("games"))
  )
}



mod_results_server <- function(id, pool, role, user) {

  moduleServer(id, function(input, output, session) {

    observe({
      tournaments <- dbGetQuery(pool,
        "SELECT tournamentid, tournamentname FROM tournament")
      updateSelectInput(session, "tournament",
        choices = setNames(tournaments$tournamentid,
                           tournaments$tournamentname))
    })

    games <- reactive({
      req(input$tournament)
      dbGetQuery(pool,
        "SELECT * FROM game WHERE tournamentid = $1 ORDER BY gameid",
        params = list(input$tournament))
    })

    output$games <- renderDT({
      datatable(games(),
        editable = role() %in% c("admin","editor"),
        rownames = FALSE)
    })

    observeEvent(input$games_cell_edit, {
      req(role() %in% c("admin","editor"))
      
      info <- input$games_cell_edit
      df <- games()
      
      row <- info$row
      col <- info$col
      val <- as.numeric(info$value)
      
      colname <- names(df)[col]
      
      # FIX: Validate column name against whitelist
      ALLOWED_COLUMNS <- c("halftimegoals1", "halftimegoals2", "regulartimegoals1", "regulartimegoals2",
                           "overtimegoals1", "overtimegoals2", "penaltygoals1", "penaltygoals2")

      if (!colname %in% ALLOWED_COLUMNS) {
        showNotification("Invalid column update attempted", type = "error")
        return()
      }
      
      gameid <- df[row,"gameid"]
      
      poolWithTransaction(pool, function(conn) {
        dbExecute(conn,
          paste0("UPDATE game SET ", colname, " = $1 WHERE gameid = $2"),
          params = list(val, gameid))
        dbExecute(conn,
          "INSERT INTO audit_log(action,user_name) VALUES ($1,$2)",
          params = list(
            paste("Updated game", gameid),
            user()
          ))
      })
      
      showNotification("Updated", type="message")
    })
  })
}