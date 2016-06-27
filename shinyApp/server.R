library(shiny)
#library(shinyBS)
library(RPostgreSQL)
library(DT)

source("function.R")
source("settings.R")

shinyServer(function(input, output, session) {

  ranking <- reactive({
    input$refresh
    getRanking()
  })
  
  output$ranking <- DT::renderDataTable({ranking()},  rownames = FALSE, options = list(pageLength = 10))

  teamranking <- reactive({
    input$refresh
    getTeamRanking()
  })
  
  output$teamranking <- DT::renderDataTable({
      datatable(teamranking(), rownames = FALSE, selection = "none", options = list(pageLength = 25)) %>% 
        formatStyle('Group', backgroundColor = styleEqual(LETTERS[1:6], c('#f5fffa', '#fffacd', '#e6e6fa', 
                                                                          '#faebd7', '#F0F8FF', '#cdc0b0'))
      )
    }
  )
  missingTips <- reactive({
    input$refresh
    getMissingTips()
  })
  
  output$missingbets <- DT::renderDataTable({missingTips()}, rownames = FALSE, options = list(pageLength = 10))
  
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
    user$registered <-registerUser(user$name, input$firstname, input$surname, input$nationality)
    if (user$registered) user$fullname <- paste(input$firstname, input$surname)
  })
  
  output$status <- renderUI({
    if (user$name == "") {
      list(
        textInput("username", "Username:"),
        passwordInput("password", "Password:"),
        actionButton("login", label = "Login")
      )
    } else {
      if (user$registered) {
        actionButton("logout", label = "Logout")
      } else {
        list(
          textInput("firstname", "First Name:"),
          textInput("surname", "Surname:"),
          textInput("nationality", "Nationality:"),
          actionButton("register", label = "Register"),
          actionButton("logout", label = "Logout")
        )
      }
    }
  })

  output$user <- renderText({
    validate(
      need(user$registered, "User not registered."),
      need(user$knownuser, "Wrong password.")
    )
    user$fullname
  })
  
  # ------- place bets -----------
  output$placebets <- renderUI({
    if (user$name != "") {
      list(tableOutput("bet"),
           actionButton("save", "Save"))
    } else {
      list(h2(loginText))
    }
  })
  
  observe({
    isolate({
      output$bet <-renderTable({
        input$refresh
        getAllTips(user$name)
      }, include.rownames = FALSE, sanitize.text.function = function(x) x)
    })
  })
  
  observeEvent(input$save, {
    games <- getFutureGames()
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
      mytext <- ifelse(fb <= 1, paste(fb, "bet saved."), paste(fb, "bets saved."))
    } else mytext <- "Please enter something"
    session$sendCustomMessage(type = 'savemessage',
                              message = mytext)
  })
  
  output$yourresults <- renderUI({
    if (user$name != "") {
      list(
        sliderInput("lastgame", "Number of games to display:", min = 1, max = getNumberOfGames(), value = 10),
        plotOutput("yourbarplot", width = "80%", height = "600px")
      )
    } else {
      list(h2(loginText))
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
    getResultCross()
  })
  
  tipCross <- reactive({
    input$refresh
    getTipCross()
  })
    
  output$heatmap <- renderPlot({
    getHeatmap(resultCross())
  })
  
  output$rankingTab <- renderUI({
    list(
      sliderInput("numberOfTopPlayer", "Number of players to display:", min = 1, max = getNumberOfPlayers(), value = 10),
      plotOutput("topPlayer", width = "100%", height = "500px")
    )
  })
  
  cumulativeResult <- reactive({
    input$refresh
    getCumulativeRanking()
  })
  
  output$topPlayer <- renderPlot({
    getCumulativePlot(cumulativeResult(), input$numberOfTopPlayer)
  })
  
  output$nationplot <- renderUI({
    plotOutput("nationboxplot", width = "100%", height = "600px")
  })
  
  output$nationboxplot <- renderPlot({
    getNationPlot(ranking())
  })
  
  output$pcaPoints <- renderUI({
    plotOutput("topPCA", width = "100%", height = "600px")
  })
  
  output$topPCA <- renderPlot({
    getPCA(resultCross(), "Principle Component Analysis based on bet game points")
  })
  
  output$pcaTips <- renderUI({
    plotOutput("tipPCA", width = "100%", height = "600px")
  })
  
  output$tipPCA <- renderPlot({
    getPCA(tipCross(), "Principle Component Analysis based on tip similarity")
  })

  output$latestGames <- renderUI({
    list(
      sliderInput("numberOfGames", "Show n latest games:", min = 1, max = getReadyGames(), step = 1, value = 1),
      DT::dataTableOutput("lastGames", width = "100%", height = "500px")
    )
  })
  
  output$lastGames <- DT::renderDataTable({
    getRankingLastGames(input$numberOfGames)
    }, rownames = FALSE, options = list(pageLength = 10)
  )
  
  output$betstatdesc <- renderText(betstatdesc)
  output$betstat <- renderPlot({
    getBetStat()
  })
})
