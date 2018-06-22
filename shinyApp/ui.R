library(shiny)
library(DT)
library(shinythemes)

source("settings.R")
source("function.R")

shinyUI(fluidPage(theme = shinytheme("cerulean"),
  titlePanel(trans("sportsevent"), windowTitle = "WC 2018"),
  tags$head(tags$script(src = "message-handler.js")),
  fluidRow(
    column(2,
           img(src=logo_file, width="200", align = "center"),
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
             tabPanel(trans("graph"), tabsetPanel(
               tabPanel(trans("heatmap"), plotOutput("heatmap", width = "100%", height = "650px")),
               tabPanel(trans("lineranking"), uiOutput("rankingTab")),
               tabPanel(trans("nationality"), uiOutput("nationplot")),
               tabPanel(trans("expertstatus"), uiOutput("expertplot")),
               tabPanel(trans("pcapoints"), uiOutput("pcaPoints")),
               tabPanel(trans("pcatips"), uiOutput("pcaTips")),
               tabPanel(trans("betstatistics"), plotOutput("betstat", width = "100%", height = "400px"), textOutput("betstatdesc")),
               tabPanel(trans("pointsperteam"), plotOutput("pointsperteam", width = "100%", height = "400px"), textOutput("pointsperteamdesc"))
             )),
             tabPanel(trans("tables"), tabsetPanel(
               tabPanel(trans("missingbets"), DT::dataTableOutput("missingbets")),
               tabPanel(trans("latestgames"), uiOutput("latestGames")),
               tabPanel(trans("gamebet"), p(), selectInput("tipgame2show", "Select Game:", getPastGames()), DT::dataTableOutput("gamebet")),
               tabPanel(trans("gameresult"), p(), DT::dataTableOutput("gameresult")),
               tabPanel(trans("teamranking"), p(), DT::dataTableOutput("teamranking"))
             )),
             tabPanel(trans("help"), br(), helpText(helpGer), hr(), helpText(helpEng))
           )
    )
  )
  
))
