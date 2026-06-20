library(shiny)
#library(shinyBS)
library(RPostgres)
library(DT)

source("function.R")

function(input, output, session) {
  tz_choices <- OlsonNames()  # at top of server function, computed once
  
  # ---- settings modal ---------------------------------------------------------
  settings_modal <- function(current_lang = "en", current_tz = "Europe/Paris") {
    modalDialog(
      title = trans("settings", current_lang),
      selectInput(
        "language",
        label = trans("language", current_lang),
        choices = c("English" = "en", "Deutsch" = "de",
                    "Português" = "pt", "Français" = "fr"),
        selected = current_lang
      ),
      selectizeInput(
        "timezone",
        label = trans("timezone", current_lang),
        choices = tz_choices,
        selected = current_tz,
        options = list(placeholder = "Select timezone...")
      ),
      footer = modalButton(trans("close", current_lang)),
      easyClose = TRUE,
      size = "s"
    )
  }
  
  # Track last-known values so the modal can restore them on reopen
  last_lang <- reactiveVal("en")
  last_tz   <- reactiveVal("Europe/Paris")
  
  observeEvent(input$language, ignoreNULL = TRUE, last_lang(input$language))
  observeEvent(input$timezone, ignoreNULL = TRUE, last_tz(input$timezone))
  
  observeEvent(input$open_settings, {
    showModal(settings_modal(last_lang(), last_tz()))
  })

  # ---- language ---------------------------------------------------------------
  available_languages <- c("en", "de", "pt", "fr")

  language_mapping <- list(
    "en" = "en", "en-US" = "en", "en-GB" = "en",
    "de" = "de", "de-DE" = "de", "de-AT" = "de", "de-CH" = "de",
    "pt" = "pt", "pt-BR" = "pt", "pt-PT" = "pt",
    "fr" = "fr", "fr-FR" = "fr", "fr-CA" = "fr", "fr-CH" = "fr"
  )

  # user_language follows the selectInput; browser detection pre-selects it
  user_language <- reactive({
    lang <- input$language %||% last_lang()
    if (is.null(lang) || !lang %in% available_languages) "en" else lang
  })

  # Pre-select the language selectInput from the browser's detected language
  observeEvent(input$client_language, ignoreNULL = TRUE, ignoreInit = TRUE, {
    detected <- input$client_language
    chosen <- if (!is.null(detected)) {
      if (detected %in% available_languages) {
        detected
      } else if (detected %in% names(language_mapping)) {
        language_mapping[[detected]]
      } else {
        prefix <- substr(detected, 1, 2)
        if (prefix %in% available_languages) prefix else "en"
      }
    } else "en"
    last_lang(chosen)
    # If modal happens to be open, update the live input too
    if (!is.null(input$language)) updateSelectInput(session, "language", selected = chosen)
  })
  
  # Convenience: reactive translation helper
  trans_r <- function(keyword) trans(keyword, user_language())

  # ---- timezone ---------------------------------------------------------------
  time_zone <- reactive({
    tz <- input$timezone %||% last_tz()
    if (is.null(tz) || !nzchar(tz) || !tz %in% tz_choices) "Europe/Paris" else tz
  })

  observeEvent(input$client_timezone, ignoreNULL = TRUE, ignoreInit = TRUE, {
    detected <- input$client_timezone
    chosen <- if (!is.null(detected) && nzchar(detected) && detected %in% tz_choices) {
      detected
    } else "Europe/Paris"
    last_tz(chosen)
    if (!is.null(input$timezone)) updateSelectizeInput(session, "timezone", selected = chosen)
  })

  # ---- sidebar dynamic content ------------------------------------------------
  output$sidebar_content <- renderUI({
    tl <- user_language()
    list(
      radioButtons(
        "tournament",
        label = paste0(trans("select", tl), ":"),
        choices = getTournament()
      ),
      hr(),
      {
        myChoice <- c("human", "human_bot", "bot")
        names(myChoice) <- c(
          trans("human", tl),
          paste(trans("human", tl), "+", trans("bot", tl)),
          trans("bot", tl)
        )
        radioButtons("showplayers", paste0(trans("show", tl), ":"), myChoice)
      },
      hr(),
      h5(textOutput("user")),
      uiOutput("status")
    )
  })

  # Update the refresh button label reactively
  observe({
    updateActionButton(session, "refresh", label = trans_r("refresh"))
  })

  # ---- main tab panel ---------------------------------------------------------
  output$main_tabs <- renderUI({
    tl <- user_language()
    navset_tab(
      nav_panel(
        trans("overallranking", tl),
        card(
          height = "calc(100vh - 140px)",
          full_screen = TRUE,
          DTOutput("ranking", height = "100%")
        )
      ),
      nav_panel(trans("placebets", tl), uiOutput("placebets")),
      nav_panel(trans("checkyourresults", tl), uiOutput("yourresults")),
      nav_panel(
        trans("player_comparison", tl),
        navset_card_tab(
          nav_panel(
            trans("heatmap", tl),
            card(
              full_screen = TRUE,
              plotOutput("heatmap", height = "75vh")
            )
          ),
          nav_panel(trans("lineranking", tl),  uiOutput("rankingTab")),
          nav_panel(trans("latestgames", tl),  uiOutput("latestGames")),
          nav_panel(
            trans("gamebet", tl),
            selectInput("tipgame2show", "Select Game:", NULL),
            layout_columns(
              col_widths = c(6, 6),
              DTOutput("gamebet"),
              plotOutput("gamebetgraph", height = "60vh")
            )
          ),
          nav_panel(trans("pcapoints", tl), uiOutput("pcaPoints")),
          nav_panel(trans("pcatips", tl),   uiOutput("pcaTips")),
          nav_panel(
            trans("missingbets", tl),
            card(
              height = "calc(100vh - 140px)",
              full_screen = TRUE,
              DTOutput("missingbets")
            )
          )
        )
      ),
      nav_panel(
        trans("summary_statistics", tl),
        navset_card_tab(
          nav_panel(trans("nationality", tl), uiOutput("nationplot")),
          nav_panel(trans("expertstatus", tl), uiOutput("expertplot"))
        )
      ),
      nav_panel(
        trans("game_statistics", tl),
        navset_card_tab(
          nav_panel(
            trans("gameresult", tl),
            card(
              height = "calc(100vh - 140px)",
              DTOutput("gameresult", height = "100%")
            )
          ),
          nav_panel(
            trans("teamranking", tl),
            card(
              height = "calc(100vh - 140px)",
              DTOutput("teamranking", height = "100%")
            )
          ),
          nav_panel(
            trans("pointsperteam", tl),
            plotOutput("pointsperteam", height = "60vh"),
            textOutput("pointsperteamdesc")
          )
        )
      ),
      nav_panel(
        trans("help", tl),
        card(
          card_body(
            p(trans("helptext", tl)),
            hr(),
            a("Registration help & legal disclaimer",
              href   = "register_help.html",
              target = "_blank"
            ),
            hr(),
            p(R.Version()$version.string)
          )
        )
      )
    )
  })

  # ---- data reactives ---------------------------------------------------------
  ranking <- reactive({
    input$refresh
    getRanking(input$showplayers, input$tournament)
  })

  output$ranking <- DT::renderDataTable({
    my_rank <- ranking()
    validate(need(my_rank, "no ranking available"))
    my_rank |> select(-`Expert-Status`)
  }, rownames = FALSE, selection = "none",
  options = list(
    pageLength = 100,  # Show more rows
    scrollY = "calc(100vh - 250px)",  # Fill available height
    scrollCollapse = TRUE,
    paging = FALSE,  # Remove pagination to show all rows
    info = FALSE
  ))

  teamranking <- reactive({
    input$refresh
    getTeamRanking(input$tournament)
  })

  output$teamranking <- DT::renderDataTable({
    datatable(
      teamranking(),
      rownames = FALSE,
      selection = "none",
      options = list(pageLength = 24, lengthChange = FALSE)
    ) |>
    formatStyle(
      'Group',
      backgroundColor = styleEqual(LETTERS[1:length(group_colors)], group_colors)
    )
  })

  tournament <- reactive({
    getTournamentName(input$tournament)
  })

  missingTips <- reactive({
    input$refresh
    getMissingTips(input$tournament, time_zone())
  })

  tips <- reactive({
    input$refresh
    getTips(input$tournament, input$tipgame2show, input$showplayers, time_zone())
  })

  output$missingbets <- DT::renderDataTable({
    mt <- missingTips()
    if (nrow(mt) > 0) mt |> mutate(starttime = format(starttime, '%Y-%m-%d %H:%M'))
  }, rownames = FALSE, selection = "none", options = list(pageLength = 20))

  output$gameresult <- DT::renderDataTable({
    input$refresh
    gr <- getGameResults(input$showplayers, input$tournament, time_zone())
    validate(need(gr, "no game result available"))
    gr |> mutate(`Start time` = format(`Start time`, '%Y-%m-%d %H:%M'), `Avg points` = round(`Avg points`, 2))
  }, rownames = FALSE, selection = "none", options = list(pageLength = 15))

  # ---- user handling ----------------------------------------------------------
  user <- reactiveValues(name = "", registered = TRUE, knownuser = TRUE, fullname = "")

  observeEvent(input$login, {
    u <- isolate(input$username)
    p <- isolate(input$password)
    updateTextInput(session, "username", value = "")
    updateTextInput(session, "password", value = "")
    cLog <- checkLogin(u, p)

    user$name       <- cLog$name
    user$registered <- cLog$registered
    user$knownuser  <- cLog$knownuser
    user$fullname   <- getName(user$name)
    logLogin(u, cLog$name != "")
    # Restore stored language and timezone preferences
    if (cLog$name != "") {
      prefs <- getUserPreferences(cLog$name)
      if (nrow(prefs) > 0) {
        stored_lang <- prefs$language[1]
        stored_tz   <- prefs$timezone[1]
        if (!is.na(stored_lang) && nzchar(stored_lang) && stored_lang %in% available_languages) {
          last_lang(stored_lang)
          updateSelectInput(session, "language", selected = stored_lang)
        }
        if (!is.na(stored_tz) && nzchar(stored_tz) && stored_tz %in% tz_choices) {
          last_tz(stored_tz)
        }
      }
    }
  })

  observeEvent(input$logout, {
    user$name       <- ""
    user$fullname   <- ""
    user$registered <- TRUE
    user$knownuser  <- TRUE
  })

  observeEvent(input$register, {
    user$registered <- registerUser(user$name, input$firstname, input$surname,
                                    input$nationality, input$expertstatus)
    if (user$registered) user$fullname <- paste(input$firstname, input$surname)
  })

  output$status <- renderUI({
    tl <- user_language()
    if (user$name == "") {
      list(
        textInput("username", trans("username", tl)),
        passwordInput("password", trans("password", tl)),
        actionButton("login", label = trans("login", tl))
      )
    } else {
      if (user$registered) {
        actionButton("logout", label = trans("logout", tl))
      } else {
        list(
          a("registration help", href = "register_help.html", target = "_blank"),
          textInput("firstname",   trans("firstname", tl)),
          textInput("surname",     trans("surname", tl)),
          textInput("nationality", paste0(trans("nationality", tl), ":")),
          radioButtons("expertstatus", "expert status",
                       c("beginner" = 1, "intermediate" = 2, "expert" = 3), selected = 2),
          actionButton("register", label = trans("register", tl)),
          actionButton("logout",   label = trans("logout", tl))
        )
      }
    }
  })

  output$user <- renderText({
    tl <- user_language()
    validate(
      need(user$registered, trans("usernotregistered", tl)),
      need(user$knownuser,  trans("wrongpassword", tl))
    )
    user$fullname
  })

  # ---- place bets -------------------------------------------------------------
  # Track unsaved changes
  has_unsaved <- reactiveVal(FALSE)

  observe({
    req(input$tournament)
    games <- getFutureGames(input$tournament)
    if (is.null(games) || nrow(games) == 0) return()
    lapply(1:nrow(games), function(n) {
      g <- games[n, "gameid"]
      input[[paste0("g", g, "t1")]]
      input[[paste0("g", g, "t2")]]
    })
    if (!is.null(isolate(user$name)) && isolate(user$name) != "") {
      has_unsaved(TRUE)
    }
  })

  output$unsaved_warning <- renderUI({
    if (has_unsaved()) {
      span(
        style = "color: orange; margin-left: 15px; font-weight: bold;",
        icon("triangle-exclamation"), " Unsaved changes!"
      )
    }
  })

  output$placebets <- renderUI({
    tl <- user_language()
    if (user$name != "") {
      input$refresh
      list(
        tableOutput("bet"),
        div(
          class = "sticky-save-bar",
          actionButton("save", trans("save", tl), class = "btn-success btn-lg"),
          uiOutput("unsaved_warning")
        )
      )
    } else {
      list(h2(trans("loginText", tl)))
    }
  })

  output$bet <- renderTable({
    input$refresh
    getAllTips(input$tournament, user$name, time_zone())
  }, include.rownames = FALSE, sanitize.text.function = function(x) x)

  observeEvent(input$save, {
    has_unsaved(FALSE)
    tl <- isolate(user_language())
    games <- getFutureGames(input$tournament)
    if (nrow(games) == 0) {
      showModal(modalDialog(title = trans("save", tl), "nothing to save"))
      return()
    }
    tiptable <- lapply(1:nrow(games), function(n) {
      g   <- games[n, "gameid"]
      k   <- games[n, "kogame"]
      g1  <- input[[paste0("g", g, "t1")]]
      g2  <- input[[paste0("g", g, "t2")]]
      kow <- ifelse(k, input[[paste0("g", g, "w")]], NA)
      if (!is.na(g1) & !is.na(g2)) c(g = g, g1 = g1, g2 = g2, kowinner = kow)
      else NULL
    })
    tiptable <- tiptable[!sapply(tiptable, is.null)]

    if (length(tiptable) > 0) {
      fb     <- upsertTip2(user$name, tiptable, input$tournament)
      mytext <- paste(
        if (fb == 1) paste(fb, trans("bet", tl)) else paste(fb, trans("bets", tl)),
        trans("saved", tl)
      )
    } else {
      mytext <- trans("missingbets", tl)
    }

    showModal(modalDialog(title = trans("save", tl), mytext))
  })

  # ---- check your results -----------------------------------------------------
  output$yourresults <- renderUI({
    tl <- user_language()
    if (user$name != "") {
      list(
        sliderInput("lastgame", "Number of games to display:",
                    min = 1, max = getNumberOfGames(input$tournament), value = 10),
        plotOutput("yourbarplot", width = "80%", height = "600px")
      )
    } else {
      list(h2(trans("loginText", tl)))
    }
  })

  playerResult <- reactive({
    input$refresh
    getPlayerResult(user$name, input$tournament, time_zone())
  })

  output$yourbarplot <- renderPlot({
    getPlayerBarplot(playerResult(), input$lastgame)
  })

  # ---- player comparison ------------------------------------------------------
  resultCross <- reactive({
    input$refresh
    getResultCross(input$showplayers, input$tournament)
  })

  tipCross <- reactive({
    input$refresh
    getTipCross(input$showplayers, input$tournament)
  })

  output$heatmap <- renderPlot({
    rc <- resultCross()
    validate(need(rc$n, "no data available"))
    getHeatmap(rc)
  })

  output$rankingTab <- renderUI({
    list(
      br(),
      fluidRow(
        column(6, sliderInput("numberOfTopPlayer", "Number of players to display:",
                              min = 1, max = getNumberOfPlayers(), value = 10)),
        column(6, checkboxInput("showMe", "show me", TRUE))
      ),
      plotOutput("topPlayer", width = "100%", height = "700px")
    )
  })

  cumulativeResult <- reactive({
    input$refresh
    getCumulativeRanking(input$showplayers, input$tournament)
  })

  output$topPlayer <- renderPlot({
    validate(
      need(cumulativeResult(), "no table available"),
      need(input$numberOfTopPlayer, "wait")
    )
    getCumulativePlot(cumulativeResult(), input$numberOfTopPlayer, input$showMe, user)
  })

  output$nationplot <- renderUI({
    plotOutput("nationboxplot", width = "100%", height = "600px")
  })

  output$nationboxplot <- renderPlot({
    getNationPlot(ranking())
  })

  output$expertplot <- renderUI({
    plotOutput("expertboxplot", width = "100%", height = "600px")
  })

  output$expertboxplot <- renderPlot({
    getExpertPlot(ranking())
  })

  output$pcaPoints <- renderUI({
    plotOutput("topPCA", width = "100%", height = "600px")
  })

  output$topPCA <- renderPlot({
    getPCA(resultCross(), trans_r("pca_point_similarity"))
  })

  output$pcaTips <- renderUI({
    plotOutput("tipPCA", width = "100%", height = "600px")
  })

  output$tipPCA <- renderPlot({
    getPCA(tipCross(), trans_r("pca_tip_similarity"))
  })

  output$latestGames <- renderUI({
    input$refresh
    n_games <- getReadyGames(input$tournament)
    validate(need(n_games, "no game finished yet"))
    list(
      sliderInput("numberOfGames", "Show n latest games:",
                  min = 1, max = n_games, step = 1, value = 1),
      DT::dataTableOutput("lastGames", width = "100%")
    )
  })

  output$lastGames <- DT::renderDataTable({
    getRankingLastGames(input$numberOfGames, input$showplayers, input$tournament)
  }, rownames = FALSE, selection = "none", options = list(pageLength = 15))

  output$gamebet <- DT::renderDataTable({
    input$refresh
    validate(need(input$tipgame2show, "no game running or finished yet"))
    myTips <- tips()
    validate(need(myTips, "no valid tip"))
    myTips <- myTips |> mutate(Time = format(Time, '%Y-%m-%d %H:%M'),
                               Tip  = paste0(goals1, ":", goals2))
    if (!myTips |> select(kowinner) |> is.na() |> all()) {
      myTips <- myTips |> mutate(
        Tip = ifelse(goals1 == goals2 & !is.na(kowinner), paste0(Tip, " (", kowinner, ")"), Tip)
      )
    }
    myTips |> select(-goals1, -goals2, -winner, -kowinner)
  }, rownames = FALSE, selection = "none", options = list(pageLength = 15))

  output$gamebetgraph <- renderPlot({
    input$refresh
    validate(need(input$tipgame2show, "no game running or finished yet"))
    myTips  <- tips()
    myTeams <- getTeams(input$tournament, input$tipgame2show)
    validate(need(myTips, "no valid tip"))
    getGameBetPlot(myTips, myTeams)
  })

  observeEvent(list(input$tournament, time_zone()), {
    pg <- getPastGames(input$tournament)
    if (is.null(pg)) {
      updateSelectInput(session, "tipgame2show", choices = NULL, selected = NULL)
    } else {
      updateSelectInput(session, "tipgame2show", choices = pg, selected = pg[1])
    }
    validate(need(pg, "no past games available"))
  })

  # ---- persist user preferences -----------------------------------------------
  observeEvent(list(input$language, input$timezone), {
    req(user$name != "")
    lang <- input$language
    tz   <- input$timezone
    if (!is.null(lang) && !is.null(tz) && nzchar(lang) && nzchar(tz)) {
      saveUserPreferences(user$name, lang, tz)
    }
  }, ignoreInit = TRUE)

  # ---- game statistics --------------------------------------------------------
  output$pointsperteamdesc <- renderText(trans_r("pointsperteamdesc"))

  output$pointsperteam <- renderPlot({
    getTeamBetPoints(input$showplayers, input$tournament)
  })

}
