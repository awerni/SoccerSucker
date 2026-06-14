
library(shiny)
library(bslib)
library(DT)
library(DBI)
library(pool)
library(httr)
library(jsonlite)
library(tidyverse)

source("settings_admin.R")
source("mod_results.R")
source("mod_settings.R")
source("mod_games.R")
source("mod_users.R")
source("mod_fifa.R")
source("mod_activity.R") 
source("function.R")

# ---------------------------
# DB Pool
# ---------------------------
pool <- dbPool(
  drv      = RPostgres::Postgres(),
  dbname   = db_admin$dbname,
  host     = db_admin$host,
  port     = db_admin$port,
  user     = db_admin$user,
  password = db_admin$password,
  sslmode  = db_admin$sslmode
)

onStop(function() poolClose(pool))

# ===========================
# UI
# ===========================

ui <- uiOutput("main_ui")

# ===========================
# SERVER
# ===========================

server <- function(input, output, session) {

  user_role <- reactiveVal(NULL)
  user_name <- reactiveVal(NULL)

  # ---------------- LOGIN ----------------

  output$main_ui <- renderUI({

    if (is.null(user_role())) {
      return(
        page_fluid(
          theme = bs_theme(version = 5, bootswatch = "flatly"),

          div(
            class = "mx-auto",
            style = "max-width: 400px; margin-top: 120px;",
            card(
              card_header("Admin Login"),
              card_body(
                textInput("user", "Username"),
                passwordInput("pw", "Password"),
                actionButton("login", "Login", class = "btn-primary w-100")
              )
            )
          )
        )
      )
    }
    
    page_navbar(
      theme = bs_theme(version = 5, bootswatch = "flatly"),

      title = paste("Admin (", user_role(), ")", sep = ""),

      nav_panel("Results", mod_results_ui("results")),
      nav_panel("Games", mod_games_ui("games")),
      nav_panel("Users", mod_users_ui("users")),
      nav_panel("User Activities", mod_activity_ui("activity")),
      nav_panel("Update FIFA Ranking", mod_fifa_ui("fifa")),
      nav_panel("Settings", mod_settings_ui("settings")),
      nav_spacer(),
      nav_item(actionButton("logout", "Logout", class = "btn-danger"))
    )
    
  })

  observeEvent(input$login, {

    sql <- "SELECT username, role FROM admin_user WHERE username = $1 AND password = md5($2)"
    res <- dbGetQuery(pool, sql, params = list(input$user, input$pw))

    if (nrow(res) == 1) {
      user_role(res$role)
      user_name(res$username)
    } else {
      showNotification("Invalid login", type = "error")
    }
  })

  observeEvent(input$logout, {
    user_role(NULL)
    user_name(NULL)
  })

  # ---------------- Modules ----------------

  observe({
    req(user_role())

    mod_results_server(
      "results",
      pool = pool,
      role = user_role,
      user = user_name
    )

    mod_fifa_server(
      "fifa",
      pool = pool,
      role = user_role,
      user = user_name
    )

    mod_settings_server(
      "settings",
      pool = pool,
      role = user_role,
      user = user_name
    )

    mod_users_server(
      "users",
      pool = pool,
      role = user_role,
      user = user_name
    )

    mod_games_server(
      "games",
      pool = pool,
      role = user_role,
      user = user_name
    )

    mod_activity_server(
      "activity",
      pool = pool,
      role = user_role,
      user = user_name
    )
  })
}

shinyApp(ui, server)
