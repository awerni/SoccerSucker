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
  title = NULL,
  header = tags$head(
    tags$script(HTML('
      // Send user timezone and language to server on app initialization
      $(document).ready(function() {
        var userTimezone = Intl.DateTimeFormat().resolvedOptions().timeZone;
        Shiny.setInputValue("client_timezone", userTimezone, {priority: "event"});
        var userLang = navigator.language || navigator.userLanguage || "en";
        Shiny.setInputValue("client_language", userLang, {priority: "event"});
      });
    '))
  ),
  window_title = "Soccer Sucker",
  sidebar = sidebar(
    width = 300,
    div(
      class = "text-center",
      img(src = logo_file, width = "180px")
    ),
    hr(),
    # Language selector — static so the server can read it from the very first tick
    selectInput(
      "language",
      label = NULL,
      choices = c("English" = "en", "Deutsch" = "de", "Português" = "pt", "Français" = "fr"),
      selected = "en"
    ),
    # Timezone selector — static input, choices populated server-side for performance
    selectizeInput(
      "timezone",
      label = NULL,
      choices = NULL,
      options = list(placeholder = "Select timezone...")
    ),
    hr(),
    # Dynamic sidebar content: tournament picker, player filter, user section
    uiOutput("sidebar_content"),
    hr(),
    actionButton("refresh", "...", class = "btn-primary w-100")
  ),
  # The entire tab panel is rendered server-side so tab labels react to language changes
  uiOutput("main_tabs")
)
