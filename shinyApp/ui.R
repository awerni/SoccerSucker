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
    ')),
    tags$style(HTML('
      .sticky-save-bar {
        position: sticky;
        bottom: 0;
        background-color: white;
        padding: 10px 0;
        border-top: 2px solid #dee2e6;
        z-index: 999;
        text-align: center;
      }
    '))
  ),
  window_title = "Soccer Sucker",
  sidebar = sidebar(
    width = 300,
    div(
      class = "d-flex justify-content-between align-items-center",
      img(src = logo_file, width = "180px"),
      actionButton(
        "open_settings",
        label = NULL,
        icon  = icon("gear"),
        class = "btn-sm btn-outline-secondary",
        title = "Settings"
      )
    ),
    hr(),
    uiOutput("sidebar_content"),
    hr(),
    actionButton("refresh", "...", class = "btn-primary w-100")
  ),
  # The entire tab panel is rendered server-side so tab labels react to language changes
  uiOutput("main_tabs")
)
