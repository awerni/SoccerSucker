library(shiny)
library(DT)
library(shinythemes)

source("settings.R")
source("function.R")

shinyUI(fluidPage(theme = shinytheme("spacelab"),
  
  titlePanel(trans("sportsevent"), windowTitle = "WC 2018"),
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
               tabPanel(trans("heatmap"), d3heatmapOutput("heatmap")),
               tabPanel(trans("lineranking"), uiOutput("rankingTab")),
               tabPanel(trans("nationality"), uiOutput("nationplot")),
               tabPanel(trans("pcapoints"), uiOutput("pcaPoints")),
               tabPanel(trans("pcatips"), uiOutput("pcaTips")),
               tabPanel(trans("betstatistics"), plotOutput("betstat", width = "100%", height = "400px"), textOutput("betstatdesc")),
               tabPanel(trans("pointsperteam"), plotOutput("pointsperteam", width = "100%", height = "400px"), textOutput("pointsperteamdesc"))
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
