mod_fifa_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h4("Update FIFA Ranking"),
    br(),

    actionButton(
      ns("update"),
      "Update FIFA Ranking Now",
      class = "btn-primary"
    ),
    br(), br(),
    verbatimTextOutput(ns("status")),
    br(),
    h5("Teams not matched in database:"),
    verbatimTextOutput(ns("missing"))
  )
}

mod_fifa_server <- function(id, pool, role, user) {

  moduleServer(id, function(input, output, session) {
    status <- reactiveVal("")
    missing_teams <- reactiveVal(NULL)
    output$status <- renderText({
      status()
    })

    output$missing <- renderText({
      req(missing_teams())
      if (length(missing_teams()) == 0) {
        "All teams matched ✅"
      } else {
        paste(missing_teams(), collapse = "\n")
      }
    })

    observeEvent(input$update, {
      req(role() == "admin")
      status("Downloading latest FIFA ranking...")
      withProgress(message = "Updating FIFA Ranking", value = 0, {
        incProgress(0.3)
        result <- update_FIFA_ranking(pool)
        updated <- result$updated
        not_matched <- result$not_matched
        incProgress(0.8)

        # Save unmatched teams in reactiveVal
        missing_teams(not_matched)

        # Audit log
        dbExecute(
          pool,
          "INSERT INTO audit_log(action, user_name) VALUES ($1, $2)",
          params = list(
            paste("Updated FIFA ranking -", updated, "teams affected"),
            user()
          )
        )
        incProgress(1)
      })

      status(
        paste0(
          "✅ Update finished.\n\n",
          result$updated, " teams were updated.\n\n",
          length(result$not_matched), " teams were NOT matched."
        )
      )

      showNotification(
        paste(result$updated, "rankings updated"),
        type = "message"
      )
    })
  })
}