library(shiny)
library(DT)

source("settings.R")

shinyUI(fluidPage(
  
  titlePanel("Euro 2016", windowTitle = "Euro 2016"),
  tags$head(tags$script(src = "message-handler.js")),
  fluidRow(
    column(2,
           img(src='GoStrong.png', width="200", align = "center"),
           hr(),
           actionButton("refresh", label = trans("refresh")),
           hr(),
           h3(textOutput("user")),
           uiOutput("status")
    ),
    column(10,
           tabsetPanel(
             tabPanel(trans("overallranking"), DT::dataTableOutput("ranking")),
             tabPanel(trans("placebets"), uiOutput("placebets")),
             tabPanel(trans("checkyourresults"), uiOutput("yourresults")),
             tabPanel(trans("graph"), tabsetPanel(
               tabPanel(trans("heatmap"), plotOutput("heatmap", width = "100%", height = "600px")),
               tabPanel(trans("lineranking"), uiOutput("rankingTab")),
               tabPanel(trans("nationality"), uiOutput("nationplot")),
               tabPanel(trans("pcapoints"), uiOutput("pcaPoints")),
               tabPanel(trans("pcatips"), uiOutput("pcaTips")),
               tabPanel(trans("betstatistics"), plotOutput("betstat", width = "100%", height = "400px"), textOutput("betstatdesc"))
             )),
             tabPanel(trans("tables"), tabsetPanel(
               tabPanel(trans("missingbets"), DT::dataTableOutput("missingbets")),
               tabPanel(trans("latestgames"), uiOutput("latestGames")),
               tabPanel(trans("teamranking"), DT::dataTableOutput("teamranking"))
             )),
             tabPanel(trans("help"), br(), helpText(helpGer), hr(), helpText(helpEng))
           )
    )
  )
  
))
