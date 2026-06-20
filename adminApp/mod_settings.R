
mod_settings_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("tournament"), "Tournament", choices=NULL),
    checkboxInput(ns("locked"), "Tournament Locked"),
    actionButton(ns("save"), "Save", class="btn-primary")
  )
}

mod_settings_server <- function(id, pool, role, user) {

  moduleServer(id, function(input, output, session) {

    observe({
      req(role())
      tournaments <- dbGetQuery(pool,
        "SELECT tournamentid,tournamentname FROM tournament")
      updateSelectInput(session,"tournament",
        choices=setNames(tournaments$tournamentid,
                         tournaments$tournamentname))
    })

    observeEvent(input$tournament,{
      req(input$tournament)
      res <- dbGetQuery(pool,
        "SELECT locked FROM tournament WHERE tournamentid=$1",
        params=list(input$tournament))
      updateCheckboxInput(session,"locked",value=res$locked)
    })

    observeEvent(input$save,{

      req(role()=="admin")

      poolWithTransaction(pool, function(conn) {
        dbExecute(conn,
          "UPDATE tournament SET locked=$1 WHERE tournamentid=$2",
          params=list(input$locked,input$tournament))

        dbExecute(conn,
          "INSERT INTO audit_log(action,user_name)
           VALUES ($1,$2)",
          params=list("Changed tournament lock",user()))
      })

      showNotification("Saved",type="message")
    })
  })
}

