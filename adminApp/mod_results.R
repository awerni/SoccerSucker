mod_results_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),
    fluidRow(
      column(4,
        selectInput(ns("tournament"), "Tournament", choices = NULL)
      )
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

    # ── Selected game ─────────────────────────────────────────────────────────
    selected_game <- reactive({
      req(length(input$games_rows_selected) > 0)
      games()[input$games_rows_selected, ]
    })

    # ── Entry form ────────────────────────────────────────────────────────────
    output$entry_form <- renderUI({
      req(role() %in% c("admin", "editor"))

      g        <- if (length(input$games_rows_selected) > 0) games()[input$games_rows_selected, ] else NULL
      is_ko    <- !is.null(g) && isTRUE(g$kogame)
      has_game <- !is.null(g)

      # Determine initial visibility from DB values
      db_rt1 <- if (has_game) g$regulartimegoals1 else NA
      db_rt2 <- if (has_game) g$regulartimegoals2 else NA
      db_ot1 <- if (has_game) g$overtimegoals1    else NA
      db_ot2 <- if (has_game) g$overtimegoals2    else NA

      show_ot  <- is_ko && !is.na(db_rt1) && !is.na(db_rt2) && db_rt1 == db_rt2
      show_pen <- show_ot && !is.na(db_ot1) && !is.na(db_ot2) && db_ot1 == db_ot2

      hidden_ot  <- if (show_ot)  "display: flex;"  else "display: none;"
      hidden_pen <- if (show_pen) "display: flex;"  else "display: none;"
      hidden_ot_hdr  <- if (show_ot)  "" else "display: none;"
      hidden_pen_hdr <- if (show_pen) "" else "display: none;"

      card(
        card_header(
          if (has_game)
            paste0("Game #", g$gameid, " — ", g$team1, " vs ", g$team2,
                   if (is_ko) "  (Knockout)" else "  (Group stage)")
          else
            "Select a game from the table below"
        ),
        card_body(
          # ── Column headers ──────────────────────────────────────────
          fluidRow(
            style = "display: flex; align-items: center;",
            div(style = "width: 150px; flex-shrink: 0;"),
            div(style = "width: 80px;  flex-shrink: 0; text-align: center; font-weight: bold;",
                "Half-time"),
            div(style = "width: 100px; flex-shrink: 0; text-align: center; font-weight: bold;",
                "Regular time"),
            div(id = ns("hdr_ot"),
                style = paste0("width: 80px; flex-shrink: 0; text-align: center; font-weight: bold; ", hidden_ot_hdr),
                "Extra time"),
            div(id = ns("hdr_pen"),
                style = paste0("width: 80px; flex-shrink: 0; text-align: center; font-weight: bold; ", hidden_pen_hdr),
                "Penalties")
          ),
          br(),
          # ── Team 1 row ──────────────────────────────────────────────
          fluidRow(
            style = "display: flex; align-items: center;",
            div(style = "width: 150px; flex-shrink: 0;",
                tags$b(if (has_game) g$team1 else "Team 1", style = "color:#2c7be5;")),
            div(style = "width: 80px; flex-shrink: 0;",
                numericInput(ns("ht1"), NULL,
                  value = if (has_game) g$halftimegoals1    else NA, min = 0, width = "60px")),
            div(style = "width: 100px; flex-shrink: 0;",
                numericInput(ns("rt1"), NULL,
                  value = if (has_game) g$regulartimegoals1 else NA, min = 0, width = "60px")),
            div(id = ns("col_ot1"),
                style = paste0("width: 80px; flex-shrink: 0; ", hidden_ot),
                numericInput(ns("ot1"), NULL,
                  value = if (has_game) g$overtimegoals1    else NA, min = 0, width = "60px")),
            div(id = ns("col_pen1"),
                style = paste0("width: 80px; flex-shrink: 0; ", hidden_pen),
                numericInput(ns("pen1"), NULL,
                  value = if (has_game) g$penaltygoals1     else NA, min = 0, width = "60px"))
          ),
          # ── Team 2 row ──────────────────────────────────────────────
          fluidRow(
            style = "display: flex; align-items: center;",
            div(style = "width: 150px; flex-shrink: 0;",
                tags$b(if (has_game) g$team2 else "Team 2", style = "color:#e63757;")),
            div(style = "width: 80px; flex-shrink: 0;",
                numericInput(ns("ht2"), NULL,
                  value = if (has_game) g$halftimegoals2    else NA, min = 0, width = "60px")),
            div(style = "width: 100px; flex-shrink: 0;",
                numericInput(ns("rt2"), NULL,
                  value = if (has_game) g$regulartimegoals2 else NA, min = 0, width = "60px")),
            div(id = ns("col_ot2"),
                style = paste0("width: 80px; flex-shrink: 0; ", hidden_ot),
                numericInput(ns("ot2"), NULL,
                  value = if (has_game) g$overtimegoals2    else NA, min = 0, width = "60px")),
            div(id = ns("col_pen2"),
                style = paste0("width: 80px; flex-shrink: 0; ", hidden_pen),
                numericInput(ns("pen2"), NULL,
                  value = if (has_game) g$penaltygoals2     else NA, min = 0, width = "60px"))
          ),
          br(),
          div(id = ns("help_ot"),
              style = if (is_ko && !show_ot) "" else "display: none;",
              helpText("Extra time columns appear when Regular time ends in a draw.")),
          div(id = ns("help_pen"),
              style = if (show_ot && !show_pen) "" else "display: none;",
              helpText("Penalty columns appear when Extra time ends in a draw.")),
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

    # ── Show/hide OT and Penalty based on live input changes ─────────────────
    observe({
      g     <- if (length(input$games_rows_selected) > 0) games()[input$games_rows_selected, ] else NULL
      is_ko <- !is.null(g) && isTRUE(g$kogame)

      rt1_v <- input$rt1;  rt2_v <- input$rt2
      ot1_v <- input$ot1;  ot2_v <- input$ot2

      rt_draw <- is_ko &&
        !is.null(rt1_v) && !is.null(rt2_v) &&
        !is.na(rt1_v)   && !is.na(rt2_v)   &&
        rt1_v == rt2_v

      ot_draw <- rt_draw &&
        !is.null(ot1_v) && !is.null(ot2_v) &&
        !is.na(ot1_v)   && !is.na(ot2_v)   &&
        ot1_v == ot2_v

      # Extra time
      if (rt_draw) {
        shinyjs::show("hdr_ot"); shinyjs::show("col_ot1"); shinyjs::show("col_ot2")
        shinyjs::hide("help_ot")
      } else {
        shinyjs::hide("hdr_ot"); shinyjs::hide("col_ot1"); shinyjs::hide("col_ot2")
        updateNumericInput(session, "ot1",  value = NA)
        updateNumericInput(session, "ot2",  value = NA)
        shinyjs::hide("hdr_pen"); shinyjs::hide("col_pen1"); shinyjs::hide("col_pen2")
        updateNumericInput(session, "pen1", value = NA)
        updateNumericInput(session, "pen2", value = NA)
        shinyjs::hide("help_pen")
        if (is_ko) shinyjs::show("help_ot") else shinyjs::hide("help_ot")
      }

      # Penalties
      if (rt_draw) {
        if (ot_draw) {
          shinyjs::show("hdr_pen"); shinyjs::show("col_pen1"); shinyjs::show("col_pen2")
          shinyjs::hide("help_pen")
        } else {
          shinyjs::hide("hdr_pen"); shinyjs::hide("col_pen1"); shinyjs::hide("col_pen2")
          updateNumericInput(session, "pen1", value = NA)
          updateNumericInput(session, "pen2", value = NA)
          shinyjs::show("help_pen")
        }
      }
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

      ot1  <- if (is_ko) input$ot1  else NA
      ot2  <- if (is_ko) input$ot2  else NA
      pen1 <- if (is_ko) input$pen1 else NA
      pen2 <- if (is_ko) input$pen2 else NA

      # ── Validate non-negative integers ───────────────────────────────
      vals <- list(ht1=input$ht1, ht2=input$ht2, rt1=input$rt1, rt2=input$rt2,
                   ot1=ot1, ot2=ot2, pen1=pen1, pen2=pen2)
      bad <- Filter(function(v) !is.null(v) && !is.na(v) && (v < 0 || v != round(v)), vals)
      if (length(bad) > 0) {
        showNotification("All goal values must be non-negative whole numbers.", type = "error")
        return()
      }

      ht1_v <- input$ht1; ht2_v <- input$ht2
      rt1_v <- input$rt1; rt2_v <- input$rt2

      # ── Validate half-time <= regular time ───────────────────────────
      if (!is.na(ht1_v) && !is.na(rt1_v) && ht1_v > rt1_v) {
        showNotification(paste0(g$team1, ": Half-time goals cannot exceed Regular time goals."), type = "error")
        return()
      }
      if (!is.na(ht2_v) && !is.na(rt2_v) && ht2_v > rt2_v) {
        showNotification(paste0(g$team2, ": Half-time goals cannot exceed Regular time goals."), type = "error")
        return()
      }

      if (is_ko) {
        # ── Validate regular time <= extra time ──────────────────────
        if (!is.na(rt1_v) && !is.na(ot1) && rt1_v > ot1) {
          showNotification(paste0(g$team1, ": Regular time goals cannot exceed Extra time goals."), type = "error")
          return()
        }
        if (!is.na(rt2_v) && !is.na(ot2) && rt2_v > ot2) {
          showNotification(paste0(g$team2, ": Regular time goals cannot exceed Extra time goals."), type = "error")
          return()
        }

        # ── KO game must have a winner ────────────────────────────────
        rt_draw <- !is.na(rt1_v) && !is.na(rt2_v) && rt1_v == rt2_v

        if (rt_draw) {
          # Must have OT goals
          if (is.na(ot1) || is.na(ot2)) {
            showNotification("Knockout game: please enter Extra time goals.", type = "error")
            return()
          }
          ot_draw <- ot1 == ot2
          if (ot_draw) {
            # Must have penalty goals and they must differ
            if (is.na(pen1) || is.na(pen2)) {
              showNotification("Knockout game: please enter Penalty goals.", type = "error")
              return()
            }
            if (pen1 == pen2) {
              showNotification("Knockout game: Penalties must produce a winner (scores cannot be equal).", type = "error")
              return()
            }
          }
        }
      }

      # ── Save to database ──────────────────────────────────────────────
      poolWithTransaction(pool, function(conn) {
        dbExecute(conn,
          "UPDATE game
           SET halftimegoals1    = $1,  halftimegoals2    = $2,
               regulartimegoals1 = $3,  regulartimegoals2 = $4,
               overtimegoals1    = $5,  overtimegoals2    = $6,
               penaltygoals1     = $7,  penaltygoals2     = $8
           WHERE gameid = $9",
          params = list(
            input$ht1, input$ht2,
            input$rt1, input$rt2,
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

      showNotification(
        paste0("Result saved: ", g$team1, " vs ", g$team2),
        type = "message"
      )
      refresh(refresh() + 1)
    })
  })
}
