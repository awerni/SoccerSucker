mod_users_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h4("Create New SoccerSuccer User"),
    br(),

    textInput(ns("username"), "Username"),
    textInput(ns("firstname"), "First Name"),
    textInput(ns("name"), "Surname"),
    passwordInput(ns("password"), "Password"),
    textInput(ns("nationality"), "Nationality"),
    textInput(ns("expertstatus"), "Expert Status"),
    checkboxInput(ns("artificial"), "Artificial User", value = FALSE),

    selectInput(
      ns("tournament"),
      "Assign to Tournament",
      choices = NULL
    ),

    actionButton(
      ns("create_user"),
      "Create User",
      class = "btn-success"
    ),

    br(), br(),
    hr(),
    h4("Existing Users"),
    DTOutput(ns("user_table"))
  )
}

mod_users_server <- function(id, pool, role, user) {

  moduleServer(id, function(input, output, session) {

    # Only admin can use this module
    observe({
      req(role() == "admin")
    })

    # -------------------------
    # Load tournaments
    # -------------------------

    observe({
      tournaments <- dbGetQuery(
        pool,
        "SELECT tournamentid, tournamentname FROM tournament"
      )

      updateSelectInput(
        session,
        "tournament",
        choices = setNames(
          tournaments$tournamentid,
          tournaments$tournamentname
        )
      )
    })

    # -------------------------
    # Show existing users
    # -------------------------

    users <- reactive({
      dbGetQuery(pool,
        "SELECT username, name, firstname, nationality, expertstatus, artificial FROM player ORDER BY name, firstname")
    })

    output$user_table <- renderDT({
      datatable(users(), rownames = FALSE)
    })

    # -------------------------
    # Create user
    # -------------------------

    observeEvent(input$create_user, {

      req(role() == "admin")
      req(input$username, input$password)

      poolWithTransaction(pool, function(conn) {

        # NEW: Insert player information
        dbExecute(
          conn,
          "INSERT INTO player (username, name, firstname, nationality, expertstatus, artificial)
          VALUES ($1, $2, $3, $4, $5, $6)",
          params = list(
            input$username,
            input$name,
            input$firstname,
            input$nationality,
            input$expertstatus,
            input$artificial
          )
        )

        dbExecute(
          conn,
          "INSERT INTO gameuser (username, password)
          VALUES ($1, md5($2))",
          params = list(
            input$username,
            input$password
          )
        )

        # Audit log
        dbExecute(
          conn,
          "INSERT INTO audit_log(action, user_name)
          VALUES ($1, $2)",
          params = list(
            paste("Created user", input$username),
            user()
          )
        )
      })

      showNotification("User created successfully", type = "message")

      # Clear all input fields
      updateTextInput(session, "username", value = "")
      updateTextInput(session, "firstname", value = "")
      updateTextInput(session, "name", value = "")
      updateTextInput(session, "password", value = "")
      updateTextInput(session, "nationality", value = "")
      updateTextInput(session, "expertstatus", value = "")
      updateCheckboxInput(session, "artificial", value = FALSE)
    })
  })
}