library(shiny)
library(DT)
library(bslib)

source("function.R")

page_sidebar(
  theme = bs_theme(version = 5,
    bootswatch = "flatly",
    primary = "#1f4e79",
    base_font = font_google("Inter"),
    enable_tooltips = TRUE
  ),
  title = "⚽ Soccer Succer",
  window_title = "Soccer Succer",
  sidebar = sidebar(
    width = 300,
    div(class = "text-center",
        img(src = logo_file, width = "180px"),
        hr(),
        strong("Version 1.5")
    ),
    hr(),
    textOutput("timezone"),
    actionButton("reload", "Switch Timezone", class = "btn-sm"),
    hr(),
    radioButtons(
      "tournament",
      label = paste0(trans("select"), ":"),
      choices = getTournament()
    ),
    hr(),
    {
      myChoise <- c("human", "human_bot", "bot")
      names(myChoise) <- c(trans("human"), paste(trans("human"), "+", trans("bot")),  trans("bot"))
      radioButtons("showplayers", paste0(trans("show"), ":"), myChoise)
    },
    hr(),
    h5(textOutput("user")),
    uiOutput("status"),
    hr(),
    actionButton("refresh", trans("refresh"), class = "btn-primary w-100")
  ),
  navset_tab(
    nav_panel(
      trans("overallranking"),
      card(
        full_screen = TRUE,
        card_body(
          DTOutput("ranking")
        )
      )
    ),
    nav_panel(trans("placebets"), uiOutput("placebets")),
    nav_panel(trans("checkyourresults"), uiOutput("yourresults")),
    nav_panel(
      trans("player_comparison"),
      navset_card_tab(
        nav_panel(
          trans("heatmap"),
          card(
            full_screen = TRUE,
            plotOutput("heatmap", height = "75vh")
          )
        ),
        nav_panel(trans("lineranking"), uiOutput("rankingTab")),
        nav_panel(trans("latestgames"), uiOutput("latestGames")),
        nav_panel(
          trans("gamebet"),
          selectInput("tipgame2show", "Select Game:", NULL),
          layout_columns(
            col_widths = c(6, 6),
            DTOutput("gamebet"),
            plotOutput("gamebetgraph", height = "60vh")
          )
        ),
        nav_panel(trans("pcapoints"), uiOutput("pcaPoints")),
        nav_panel(trans("pcatips"), uiOutput("pcaTips")),
        nav_panel(trans("missingbets"), DTOutput("missingbets"))
      )
    ),
    nav_panel(
      trans("summary_statistics"),
      navset_card_tab(
        nav_panel(trans("nationality"), uiOutput("nationplot")),
        nav_panel(trans("expertstatus"), uiOutput("expertplot"))
      )
    ),
    nav_panel(
      trans("game_statistics"),
      navset_card_tab(
        nav_panel(trans("gameresult"), DTOutput("gameresult")),
        nav_panel(
          trans("teamranking"), 
          card(
            height = "calc(100vh - 140px)",
            DTOutput("teamranking", height = "100%")
          )
        ),
        nav_panel(
          trans("pointsperteam"),
          plotOutput("pointsperteam", height = "60vh"),
          textOutput("pointsperteamdesc")
        )
      )
    ),
    nav_panel(
      trans("help"),
      card(
        card_body(
          p(trans("helptext")),
          hr(),
          a("Registration help & legal disclaimer",
            href = "register_help.html",
            target = "_blank"
          ),
          hr(),
          p(R.Version()$version.string)
        )
      )
    )
  )
)
