library(shiny)
#library(shinyBS)
library(RPostgres)
library(DT)

source("function.R")
source("settings.R")

shinyServer(function(input, output, session) {

  ranking <- reactive({
    input$refresh
    getRanking(input$showplayers)
  })

  output$ranking <- DT::renderDataTable({
      my_rank <- ranking()
      validate(need(my_rank, "no ranking available"))
      my_rank %>% select(-`Expert-Status`)
    },  rownames = FALSE, selection = "none", options = list(pageLength = 15)
  )

  teamranking <- reactive({
    input$refresh
    getTeamRanking()
  })

  output$teamranking <- DT::renderDataTable({
    datatable(teamranking(), rownames = FALSE, selection = "none", options = list(pageLength = 24)) %>%
      formatStyle('Group', backgroundColor = styleEqual(LETTERS[1:8], c('#f5fffa', '#fffacd', '#e6e6fa',
                                                                        '#faebd7', '#F0F8FF', '#cdc0b0',
                                                                        '#FFCCCC', '#CCFFE5'))
      )
  })

  #%>%
  #  formatStyle('Group', backgroundColor = styleEqual(LETTERS[1:8], c('#f5fffa', '#fffacd', '#e6e6fa',
  #                                                                    '#faebd7', '#F0F8FF', '#cdc0b0',
  #                                                                    '#987654', '#537827')))

  missingTips <- reactive({
    input$refresh
    getMissingTips()
  })

  tips <- reactive({
    input$refresh
    getTips(input$tipgame2show, input$showplayers)
  })

  output$missingbets <- DT::renderDataTable({
    mt <- missingTips()
    if (nrow(mt) > 0) mt %>% mutate(starttime = format(starttime,'%Y-%m-%d %H:%M'))
  },rownames = FALSE, selection = "none", options = list(pageLength = 10))

  output$gameresult <- DT::renderDataTable({
    input$refresh
    gr <- getGameResults(input$showplayers)
    validate(need(gr, "no game result available"))
    gr %>% mutate(`Start time` = format(`Start time`,'%Y-%m-%d %H:%M'), `Avg points` = round(`Avg points`, 2))
  }, rownames = FALSE, selection = "none", options = list(pageLength = 15))

  # ---- user handling -------
  #user <- reactiveValues(name = "wernitzn", registered = TRUE, knownuser = TRUE, fullname = getName("wernitzn"))
  user <- reactiveValues(name = "", registered = TRUE, knownuser = TRUE, fullname = "")

  observeEvent(input$login, {
    u <- isolate(input$username)
    p <- isolate(input$password)
    updateTextInput(session, "username", value = "")
    updateTextInput(session, "password", value = "")
    cLog <- checkLogin(u, p)
    user$name <- cLog$name
    user$registered <- cLog$registered
    user$knownuser <- cLog$knownuser
    user$fullname <- getName(user$name)
  })

  observeEvent(input$logout, {
    user$name <- ""
    user$fullname <- ""
    user$registered <- TRUE
    user$knownuser <- TRUE
  })

  observeEvent(input$register, {
    user$registered <-registerUser(user$name, input$firstname, input$surname, input$nationality, input$expertstatus)
    if (user$registered) user$fullname <- paste(input$firstname, input$surname)
  })

  output$status <- renderUI({
    if (user$name == "") {
      list(
        textInput("username", trans("username")),
        passwordInput("password", trans("password")),
        actionButton("login", label = trans("login"))
      )
    } else {
      if (user$registered) {
        actionButton("logout", label = trans("logout"))
      } else {
        list(
          a("registration help", href="register_help.html", target="_blank"),
          textInput("firstname", trans("firstname")),
          textInput("surname", trans("surname")),
          textInput("nationality", paste(trans("nationality"), ":")),
          radioButtons("expertstatus", "expert status", c("beginner" = 1, "intermediate" = 2, "expert" = 3), selected = 2),
          actionButton("register", label = trans("register")),
          actionButton("logout", label = trans("logout"))
        )
      }
    }
  })

  output$user <- renderText({
    validate(
      need(user$registered, trans("usernotregistered")),
      need(user$knownuser, trans("wrongpassword"))
    )
    user$fullname
  })

  # ------- place bets -----------
  output$placebets <- renderUI({
    if (user$name != "") {
      input$refresh
      list(
        tableOutput("bet"),
        actionButton("save", trans("save"))
      )
    } else {
      list(h2(trans("loginText")))
    }
  })

  output$bet <-renderTable({
      input$refresh
      getAllTips(user$name)
    }, include.rownames = FALSE, sanitize.text.function = function(x) x)

  observeEvent(input$save, {
    games <- getFutureGames()
    if (nrow(games) == 0) {
      session$sendCustomMessage(type = 'savemessage',
                                message = "nothing to save")
      return()
    }
    fb <- 0
    n <- as.numeric(NA)
    tiptable <- lapply(1:nrow(games), function(n) {
        g <- games[n, "gameid"]
        k <- games[n, "kogame"]
        g1 <- input[[paste0("g", g, "t1")]]
        g2 <- input[[paste0("g", g, "t2")]]
        kow <- ifelse(k, input[[paste0("g", g, "w")]], NA)
        if (!is.na(g1) & !is.na(g2)) c(g = g, g1 = g1, g2 = g2, kowinner = kow)
        else NULL
      }
    )
    tiptable <- tiptable[!sapply(tiptable, is.null)]

    if (length(tiptable) > 0) {
      fb <- upsertTip2(user$name, tiptable)
      mytext <- paste(if(fb <= 1) paste(fb, trans("bet")) else paste(fb, trans("bets")), trans("saved"))
    } else mytext <- trans("missingbets")

    showModal(modalDialog(
      title = "Save bets",
      mytext
    ))

    # session$sendCustomMessage(type = 'savemessage',
    #                           message = mytext)
  })

  output$yourresults <- renderUI({
    if (user$name != "") {
      list(
        sliderInput("lastgame", "Number of games to display:", min = 1, max = getNumberOfGames(), value = 10),
        plotOutput("yourbarplot", width = "80%", height = "600px")
      )
    } else {
      list(h2(trans("loginText")))
    }
  })

  playerResult <- reactive({
    input$refresh
    getPlayerResult(user$name)
  })

  output$yourbarplot <- renderPlot({
    getPlayerBarplot(playerResult(), input$lastgame)
  })

  resultCross <- reactive({
    input$refresh
    getResultCross(input$showplayers)
  })

  tipCross <- reactive({
    input$refresh
    getTipCross(input$showplayers)
  })

  output$heatmap <- renderPlot({
    getHeatmap(resultCross())
  })

  output$rankingTab <- renderUI({
    list(
      br(),
      fluidRow(
        column(6, sliderInput("numberOfTopPlayer", "Number of players to display:", min = 1, max = getNumberOfPlayers(), value = 10)),
        column(6, checkboxInput("showMe", "show me", TRUE))
      ),
      plotOutput("topPlayer", width = "100%", height = "700px")
    )
  })

  cumulativeResult <- reactive({
    input$refresh
    getCumulativeRanking(input$showplayers)
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
    getPCA(resultCross(), trans("pca_point_similarity"))
  })

  output$pcaTips <- renderUI({
    plotOutput("tipPCA", width = "100%", height = "600px")
  })

  output$tipPCA <- renderPlot({
    getPCA(tipCross(), trans("pca_tip_similarity"))
  })

  output$latestGames <- renderUI({
    input$refresh
    validate(
      need(getReadyGames(), "no game finished yet")
    )
    list(
      sliderInput("numberOfGames", "Show n latest games:", min = 1, max = getReadyGames(), step = 1, value = 1),
      DT::dataTableOutput("lastGames", width = "100%", height = "500px")
    )
  })

  output$lastGames <- DT::renderDataTable({
    getRankingLastGames(input$numberOfGames, input$showplayers)
    }, rownames = FALSE, selection = "none", options = list(pageLength = 15)
  )

  output$betstatdesc <- renderText(trans("betstatdesc"))
  output$betstat <- renderPlot({
    getBetStat(input$showplayers)
  })

  output$gamebet <- DT::renderDataTable({
    input$refresh
    validate(
      need(input$tipgame2show, "no game running or finished yet")
    )
    myTips <- tips()
    validate(
      need(myTips, "no valid tip")
    )
    myTips <- myTips %>% mutate(Time = format(Time,'%Y-%m-%d %H:%M'), Tip = paste0(goals1, ":", goals2))
    if (!myTips %>% select(kowinner) %>% is.na() %>% all()) {
      myTips <- myTips %>% mutate(Tip = ifelse(goals1 == goals2 & !is.na(kowinner), paste0(Tip, " (", kowinner, ")"), Tip))
    }
    myTips %>% select(-goals1, -goals2, -winner, -kowinner)
  }, rownames = FALSE, selection = "none", options = list(pageLength = 15))

  output$gamebetgraph <- renderPlot({
    input$refresh
    validate(
      need(input$tipgame2show, "no game running or finished yet")
    )
    myTips <- tips()
    myTeams <- getTeams(input$tipgame2show)
    validate(
      need(myTips, "no valid tip")
    )
    getGameBetPlot(myTips, myTeams)
  })

  observeEvent(input$refresh, {
    input$refresh
    pg <- getPastGames()
    validate(need(pg, "no past games available"))
    updateSelectInput(session, "tipgame2show", choices = getPastGames(), selected = input$tipgame2show)
  })

  output$pointsperteamdesc <- renderText(trans("pointsperteamdesc"))
  output$pointsperteam <- renderPlot({
    getTeamBetPoints(input$showplayers)
  })

})
