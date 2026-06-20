mod_results_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    fluidRow(
      column(4, selectInput(ns("tournament"), "Tournament", choices = NULL))
    ),
    br(),
    uiOutput(ns("entry_form")),
    br(),
    h5("Games"),
    DTOutput(ns("games"))
  )
}

mod_results_server <- function(id, pool, role, user) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # ── Tournaments ──────────────────────────────────────────────────────────
    observe({
      req(role())
      tournaments <- dbGetQuery(pool,
        "SELECT tournamentid, tournamentname FROM tournament")
      updateSelectInput(session, "tournament",
        choices = setNames(
          tournaments$tournamentid,
          tournaments$tournamentname
        ))
    })

    # ── Games table ──────────────────────────────────────────────────────────
    refresh <- reactiveVal(0)

    games <- reactive({
      refresh()
      req(input$tournament)
      dbGetQuery(pool,
        "SELECT gameid, team1, team2, kogame,
                halftimegoals1,    halftimegoals2,
                regulartimegoals1, regulartimegoals2,
                overtimegoals1,    overtimegoals2,
                penaltygoals1,     penaltygoals2
         FROM game
         WHERE tournamentid = $1
         ORDER BY gameid",
        params = list(input$tournament))
    })

    output$games <- renderDT({
      datatable(
        games(),
        selection = if (role() %in% c("admin", "editor")) "single" else "none",
        rownames  = FALSE,
        options   = list(pageLength = 25)
      )
    })

    # ── Entry form ────────────────────────────────────────────────────────────
    output$entry_form <- renderUI({
      req(role() %in% c("admin", "editor"))

      g        <- if (length(input$games_rows_selected) > 0) games()[input$games_rows_selected, ] else NULL
      has_game <- !is.null(g)
      is_ko    <- has_game && isTRUE(g$kogame)

      card(
        card_header(
          if (has_game)
            paste0("Game #", g$gameid, " — ", g$team1, " vs ", g$team2,
                   if (is_ko) "  (Knockout)" else "  (Group stage)")
          else
            "Select a game from the table below"
        ),
        card_body(
          # ── Headers ───────────────────────────────────────────────────
          div(style = "display: flex; align-items: center;",
            div(style = "width: 150px; flex-shrink: 0;"),
            div(style = "width: 80px;  flex-shrink: 0; text-align: center; font-weight: bold;",
                "Half-time"),
            div(style = "width: 110px; flex-shrink: 0; text-align: center; font-weight: bold;",
                "Regular time"),
            if (is_ko)
              div(style = "width: 90px; flex-shrink: 0; text-align: center; font-weight: bold;",
                  "Extra time"),
            if (is_ko)
              div(style = "width: 90px; flex-shrink: 0; text-align: center; font-weight: bold;",
                  "Penalties")
          ),
          br(),
          # ── Team 1 ────────────────────────────────────────────────────
          div(style = "display: flex; align-items: center; margin-bottom: 8px;",
            div(style = "width: 150px; flex-shrink: 0;",
                tags$b(if (has_game) g$team1 else "Team 1", style = "color:#2c7be5;")),
            div(style = "width: 80px; flex-shrink: 0;",
                numericInput(ns("ht1"), NULL,
                  value = if (has_game) g$halftimegoals1    else NA, min = 0, width = "65px")),
            div(style = "width: 110px; flex-shrink: 0;",
                numericInput(ns("rt1"), NULL,
                  value = if (has_game) g$regulartimegoals1 else NA, min = 0, width = "65px")),
            if (is_ko)
              div(style = "width: 90px; flex-shrink: 0;",
                  numericInput(ns("ot1"), NULL,
                    value = if (has_game) g$overtimegoals1  else NA, min = 0, width = "65px")),
            if (is_ko)
              div(style = "width: 90px; flex-shrink: 0;",
                  numericInput(ns("pen1"), NULL,
                    value = if (has_game) g$penaltygoals1   else NA, min = 0, width = "65px"))
          ),
          # ── Team 2 ────────────────────────────────────────────────────
          div(style = "display: flex; align-items: center; margin-bottom: 8px;",
            div(style = "width: 150px; flex-shrink: 0;",
                tags$b(if (has_game) g$team2 else "Team 2", style = "color:#e63757;")),
            div(style = "width: 80px; flex-shrink: 0;",
                numericInput(ns("ht2"), NULL,
                  value = if (has_game) g$halftimegoals2    else NA, min = 0, width = "65px")),
            div(style = "width: 110px; flex-shrink: 0;",
                numericInput(ns("rt2"), NULL,
                  value = if (has_game) g$regulartimegoals2 else NA, min = 0, width = "65px")),
            if (is_ko)
              div(style = "width: 90px; flex-shrink: 0;",
                  numericInput(ns("ot2"), NULL,
                    value = if (has_game) g$overtimegoals2  else NA, min = 0, width = "65px")),
            if (is_ko)
              div(style = "width: 90px; flex-shrink: 0;",
                  numericInput(ns("pen2"), NULL,
                    value = if (has_game) g$penaltygoals2   else NA, min = 0, width = "65px"))
          ),
          br(),
          fluidRow(
            column(12,
              actionButton(ns("save_result"), "Save Result",
                class = "btn-primary", disabled = !has_game),
              actionButton(ns("clear_selection"), "Clear Selection",
                class = "btn-secondary ms-2")
            )
          )
        )
      )
    })

    # ── Clear selection ───────────────────────────────────────────────────────
    observeEvent(input$clear_selection, {
      dataTableProxy("games") |> selectRows(NULL)
    })

    # ── Save result ───────────────────────────────────────────────────────────
    observeEvent(input$save_result, {
      req(role() %in% c("admin", "editor"))
      req(length(input$games_rows_selected) > 0)

      g     <- games()[input$games_rows_selected, ]
      is_ko <- isTRUE(g$kogame)

      ht1_v <- input$ht1;  ht2_v <- input$ht2
      rt1_v <- input$rt1;  rt2_v <- input$rt2
      ot1   <- if (is_ko) input$ot1  else NA
      ot2   <- if (is_ko) input$ot2  else NA
      pen1  <- if (is_ko) input$pen1 else NA
      pen2  <- if (is_ko) input$pen2 else NA

      # ── Non-negative integers ─────────────────────────────────────────
      all_vals <- c(ht1_v, ht2_v, rt1_v, rt2_v,
                    if (is_ko) c(ot1, ot2, pen1, pen2))
      non_na   <- all_vals[!is.na(all_vals)]
      if (any(non_na < 0 | non_na != round(non_na))) {
        showNotification("All goal values must be non-negative whole numbers.", type = "error")
        return()
      }

      # ── HT <= RT ──────────────────────────────────────────────────────
      if (!is.na(ht1_v) && !is.na(rt1_v) && ht1_v > rt1_v) {
        showNotification(paste0(g$team1, ": Half-time goals cannot exceed Regular time goals."), type = "error")
        return()
      }
      if (!is.na(ht2_v) && !is.na(rt2_v) && ht2_v > rt2_v) {
        showNotification(paste0(g$team2, ": Half-time goals cannot exceed Regular time goals."), type = "error")
        return()
      }

      if (is_ko) {
        # ── RT <= OT ──────────────────────────────────────────────────
        if (!is.na(rt1_v) && !is.na(ot1) && rt1_v > ot1) {
          showNotification(paste0(g$team1, ": Regular time goals cannot exceed Extra time goals."), type = "error")
          return()
        }
        if (!is.na(rt2_v) && !is.na(ot2) && rt2_v > ot2) {
          showNotification(paste0(g$team2, ": Regular time goals cannot exceed Extra time goals."), type = "error")
          return()
        }

        # ── Must have a winner ────────────────────────────────────────
        rt_draw <- !is.na(rt1_v) && !is.na(rt2_v) && rt1_v == rt2_v
        if (rt_draw) {
          if (is.na(ot1) || is.na(ot2)) {
            showNotification("Knockout game: please enter Extra time goals.", type = "error")
            return()
          }
          if (ot1 == ot2) {
            if (is.na(pen1) || is.na(pen2)) {
              showNotification("Knockout game: please enter Penalty goals.", type = "error")
              return()
            }
            if (pen1 == pen2) {
              showNotification("Knockout game: Penalties must produce a winner.", type = "error")
              return()
            }
          }
        }
      }

      # ── Write to DB ───────────────────────────────────────────────────
      poolWithTransaction(pool, function(conn) {
        dbExecute(conn,
          "UPDATE game
           SET halftimegoals1    = $1,  halftimegoals2    = $2,
               regulartimegoals1 = $3,  regulartimegoals2 = $4,
               overtimegoals1    = $5,  overtimegoals2    = $6,
               penaltygoals1     = $7,  penaltygoals2     = $8
           WHERE gameid = $9",
          params = list(
            ht1_v, ht2_v, rt1_v, rt2_v,
            ot1, ot2, pen1, pen2,
            g$gameid
          )
        )
        dbExecute(conn,
          "INSERT INTO audit_log(action, user_name) VALUES ($1, $2)",
          params = list(
            paste0("Updated result for game #", g$gameid,
                   " (", g$team1, " vs ", g$team2, ")"),
            user()
          )
        )
      })

      showNotification(paste0("Result saved: ", g$team1, " vs ", g$team2), type = "message")
      refresh(refresh() + 1)
    })
  })
}
