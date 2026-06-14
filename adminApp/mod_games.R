mod_games_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),
    h4("Create New Games"),
    br(),

    textInput(ns("gameid"), "Game ID"),
    selectInput(ns("tournament"), "Assign to Tournament", choices = NULL),
    selectInput(ns("team1"), "Team 1", choices = NULL),
    selectInput(ns("team2"), "Team 2", choices = NULL),
    textInput(ns("city"), "City"),
    checkboxInput(ns("kogame"), "KO Game", value = FALSE),
    textInput(ns("starttime"), "Start time"),

    fluidRow(
      column(12,
        actionButton(ns("create_game"), "Create Game", class = "btn-success btn-sm"),
        actionButton(ns("update_game"), "Update Game", class = "btn-primary btn-sm ms-2"),
        actionButton(ns("clear_selection"), "Clear Selection", class = "btn-secondary btn-sm ms-2")
      )
    ),

    br(), br(),
    hr(),
    h4("Existing games"),
    actionButton(ns("refresh_games"), "Refresh", class = "btn-primary btn-sm"),
    DTOutput(ns("game_table"))
  )
}

mod_games_server <- function(id, pool, role, user) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    shinyjs::disable(ns("update_game"))

    observe({
      req(role() == "admin")
    })

    # -------------------------
    # Load tournaments & teams
    # -------------------------

    observe({
      req(role() == "admin")

      tournaments <- dbGetQuery(
        pool,
        "SELECT tournamentid, tournamentname FROM tournament"
      )

      updateSelectInput(
        session,
        "tournament",
        choices = setNames(
          tournaments$tournamentid,
          tournaments$tournamentname
        )
      )

      teams <- dbGetQuery(pool, "SELECT team FROM team ORDER BY team")

      updateSelectInput(
        session,
        "team1",
        choices = c("", teams$team)
      )

      updateSelectInput(
        session,
        "team2",
        choices = c("", teams$team)
      )
    })

    # -------------------------
    # Show existing games
    # -------------------------

    games <- eventReactive(input$refresh_games, {
      dbGetQuery(pool,
        "SELECT gameid, tournamentid, team1, team2, city, starttime, kogame FROM game ORDER BY starttime desc, gameid desc")
    }, ignoreNULL = FALSE)

    output$game_table <- renderDT({
      datatable(games(), selection = "single", rownames = FALSE)
    })

    # -------------------------
    # Row selection → populate form
    # -------------------------

    observeEvent(input$game_table_rows_selected, {
      if (length(input$game_table_rows_selected) == 0) {
        shinyjs::enable(ns("create_game"))
        shinyjs::disable(ns("update_game"))
        shinyjs::enable(ns("gameid"))
        shinyjs::enable(ns("tournament"))
        return()
      }

      g <- games()[input$game_table_rows_selected, ]

      updateTextInput(session, "gameid", value = as.character(g$gameid))
      updateSelectInput(session, "tournament", selected = as.character(g$tournamentid))
      updateSelectInput(session, "team1", selected = g$team1)
      updateSelectInput(session, "team2", selected = g$team2)
      updateTextInput(session, "city", value = g$city)
      updateCheckboxInput(session, "kogame", value = isTRUE(g$kogame))
      updateTextInput(session, "starttime", value = as.character(g$starttime))

      shinyjs::disable(ns("create_game"))
      shinyjs::enable(ns("update_game"))
      shinyjs::disable(ns("gameid"))
      shinyjs::disable(ns("tournament"))
    })

    # -------------------------
    # Clear selection
    # -------------------------

    observeEvent(input$clear_selection, {
      dataTableProxy("game_table") |> selectRows(NULL)
      updateTextInput(session, "gameid", value = "")
      updateSelectInput(session, "tournament", selected = "")
      updateSelectInput(session, "team1", selected = "")
      updateSelectInput(session, "team2", selected = "")
      updateTextInput(session, "city", value = "")
      updateCheckboxInput(session, "kogame", value = FALSE)
      updateTextInput(session, "starttime", value = "")
    })

    # -------------------------
    # Create game
    # -------------------------

    observeEvent(input$create_game, {

      req(role() == "admin")
      req(input$gameid, input$tournament, input$team1, input$team2)

      poolWithTransaction(pool, function(conn) {

        dbExecute(
          conn,
          "INSERT INTO game (gameid, tournamentid, team1, team2, city, starttime, kogame)
          VALUES ($1, $2, $3, $4, $5, $6, $7)",
          params = list(
            input$gameid,
            input$tournament,
            input$team1,
            input$team2,
            input$city,
            input$starttime,
            input$kogame
          )
        )

        dbExecute(
          conn,
          "INSERT INTO audit_log(action, user_name)
          VALUES ($1, $2)",
          params = list(
            paste("Created game", input$gameid, input$team1, "vs", input$team2),
            user()
          )
        )
      })

      showNotification("Game created successfully", type = "message")

      updateTextInput(session, "gameid", value = "")
      updateSelectInput(session, "team1", selected = "")
      updateSelectInput(session, "team2", selected = "")
      updateTextInput(session, "city", value = "")
      updateTextInput(session, "starttime", value = "")
      updateCheckboxInput(session, "kogame", value = FALSE)
    })

    # -------------------------
    # Update game
    # -------------------------

    observeEvent(input$update_game, {

      req(role() == "admin")
      req(length(input$game_table_rows_selected) > 0)

      g <- games()[input$game_table_rows_selected, ]

      poolWithTransaction(pool, function(conn) {

        dbExecute(
          conn,
          "UPDATE game
           SET team1 = $1, team2 = $2, city = $3, starttime = $4, kogame = $5
           WHERE gameid = $6 AND tournamentid = $7",
          params = list(
            input$team1,
            input$team2,
            input$city,
            input$starttime,
            input$kogame,
            g$gameid,
            g$tournamentid
          )
        )

        dbExecute(
          conn,
          "INSERT INTO audit_log(action, user_name)
          VALUES ($1, $2)",
          params = list(
            paste0("Updated game #", g$gameid, " (", g$team1, " vs ", g$team2, ")"),
            user()
          )
        )
      })

      showNotification(paste0("Game #", g$gameid, " updated successfully"), type = "message")
      dataTableProxy("game_table") |> selectRows(NULL)
    })
  })
}
