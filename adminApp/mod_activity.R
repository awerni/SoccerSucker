mod_activity_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("User Activities"),
    actionButton(ns("refresh_activity"), "Refresh", class = "btn-primary"),
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
    tip_counts <- eventReactive(input$refresh_activity, {
      sql <- paste(
        "SELECT tip_count, STRING_AGG(username, ', ' ORDER BY username) AS usernames",
        "FROM (SELECT p.username, COUNT(t.gameid) AS tip_count FROM player p",
        "LEFT JOIN tip t ON p.username = t.username GROUP BY p.username",
        "HAVING NOT p.artificial) AS user_tips",
        "GROUP BY tip_count ORDER BY tip_count DESC"
      )
      dbGetQuery(pool, sql)
    }, ignoreNULL = FALSE)

    output$tip_counts <- renderDT({
      datatable(tip_counts(), rownames = FALSE)
    })

    # Login logs
    login_logs <- eventReactive(input$refresh_activity, {
      sql <- paste(
        "SELECT p.username,
         COUNT(l.logid) FILTER (WHERE l.success = TRUE) AS successful_logins,
         COUNT(l.logid) FILTER (WHERE l.success = FALSE) AS unsuccessful_logins,
         MAX(l.login_time) AS last_login
         FROM player p LEFT JOIN login_log l ON p.username = l.username
         GROUP BY p.username HAVING NOT p.artificial
         ORDER BY last_login DESC"
      )
      dbGetQuery(pool, sql)
    }, ignoreNULL = FALSE)

    output$login_logs <- renderDT({
      datatable(login_logs(), rownames = FALSE)
    })
  })
}