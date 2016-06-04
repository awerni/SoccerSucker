library(shiny)
source("settings.R")

shinyUI(fluidPage(
  
  titlePanel("Euro 2016", windowTitle = "Euro 2016"),
  tags$head(tags$script(src = "message-handler.js")),
  fluidRow(
    column(2,
           img(src='GoStrong.png', width="200", align = "center"),
           hr(),
           h3(textOutput("user")),
           uiOutput("status")
    ),
    column(10,
           tabsetPanel(
             tabPanel("Overall Ranking", dataTableOutput("ranking")),
             tabPanel("Place Bets", uiOutput("placebets")),
             tabPanel("Check your results", uiOutput("yourresults")),
             tabPanel("Graphs", tabsetPanel(
               tabPanel("Heatmap", plotOutput("heatmap", width = "100%", height = "600px")),
               tabPanel("Line Ranking", uiOutput("rankingTab")),
               tabPanel("PCA", uiOutput("pca"))
             )),
             tabPanel("Help", br(), helpText(helpGer), hr(), helpText(helpEng))
           )
    )
  )
  
))
