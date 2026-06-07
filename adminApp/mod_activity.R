mod_activity_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("User Activities"),
    br(),
    h5("Tip Counts by User"),
    DTOutput(ns("tip_counts")),
    br(),
    h5("Login Logs"),
    DTOutput(ns("login_logs"))
  )
}

mod_activity_server <- function(id, pool, role, user) {
  moduleServer(id, function(input, output, session) {
    # Tip counts by user
    tip_counts <- reactive({
      dbGetQuery(pool, "SELECT count(*) as tip_count, username FROM tip GROUP BY username")
    })

    output$tip_counts <- renderDT({
      datatable(tip_counts(), rownames = FALSE)
    })

    # Login logs
    login_logs <- reactive({
      dbGetQuery(pool, "SELECT username, login_time, success FROM login_log ORDER BY login_time DESC")
    })

    output$login_logs <- renderDT({
      datatable(login_logs(), rownames = FALSE)
    })
  })
}