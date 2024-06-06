library(shiny)
library(DT)
library(bslib)

#library(shinythemes)

source("settings.R")
source("function.R")

fluidPage(theme = bs_theme(bootswatch = "cerulean"),
  titlePanel(trans("sportsevent"), windowTitle = "Euro Cup 2024"),
  tags$head(tags$script(src = "message-handler.js")),
  fluidRow(
    column(2,
           img(src=logo_file, width="200", align = "center"),
           hr(),
           p("Version 1.3"),
           hr(),
           {
             myChoise <- c("human", "human_bot", "bot")
             names(myChoise) <- c(trans("human"), paste(trans("human"), "+", trans("bot")),  trans("bot"))
             radioButtons("showplayers", paste0(trans("show"), ":"), myChoise)
           },
           hr(),
           h3(textOutput("user")),
           uiOutput("status"),
           br(),
           actionButton("refresh", label = trans("refresh"))
    ),
    column(10,
           tabsetPanel(
             tabPanel(trans("overallranking"), DT::dataTableOutput("ranking")),
             tabPanel(trans("placebets"), uiOutput("placebets")),
             tabPanel(trans("checkyourresults"), uiOutput("yourresults")),
             tabPanel(trans("player_comparison"), tabsetPanel(
               tabPanel(trans("heatmap"), plotOutput("heatmap", width = "100%", height = "650px")),
               tabPanel(trans("lineranking"), uiOutput("rankingTab")),
               tabPanel(trans("latestgames"), uiOutput("latestGames")),
               tabPanel(trans("gamebet"), 
                 p(),
                 selectInput("tipgame2show", "Select Game:", getPastGames()),
                 fluidRow(
                   column(6, DT::dataTableOutput("gamebet")),  
                   column(6, plotOutput("gamebetgraph", width = "100%", height = "500px"))
                 )
               ),
               tabPanel(trans("pcapoints"), uiOutput("pcaPoints")),
               tabPanel(trans("pcatips"), uiOutput("pcaTips")),
               tabPanel(trans("missingbets"), DT::dataTableOutput("missingbets"))
             )),
             tabPanel(trans("summary_statistics"), tabsetPanel(
               tabPanel(trans("nationality"), uiOutput("nationplot")),
               tabPanel(trans("expertstatus"), uiOutput("expertplot")),
               tabPanel(trans("pointsperteam"), plotOutput("pointsperteam", width = "100%", height = "400px"), textOutput("pointsperteamdesc"))
             )),
             tabPanel(trans("game_statistics"), tabsetPanel(
               tabPanel(trans("gameresult"), p(), DT::dataTableOutput("gameresult")),
               tabPanel(trans("teamranking"), p(), DT::dataTableOutput("teamranking"))
             )),
             tabPanel(
               trans("help"), br(), helpText(helpEng), hr(), helpText(helpGer),
               p(
                 a("registration help and legal disclaimer", href="register_help.html", target="_blank")
               ),
               p(
                 R.Version()$version.string
               )
             )
           )
    )
  )
)
