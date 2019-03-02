# Thu Feb 28 01:36:57 2019 ------------------------------
# Shiny server backend script

server <- shinyServer(function(input, output, session) {
  current_tab  <- "overview"
  data <- callModule(data_config, "ui_config")
  analytics <- callModule(server_analytics, "ui_analytics", holdings = data$holdings, end_date = data$end_date)
  explorer <- callModule(server_explorer, "ui_explorer")
  config <- callModule(server_config, "ui_config", data$nav)
  
  # config modal dialogue ---------------------------------------------------
  observeEvent(input$navbar, {
    navbar <- input$navbar
    
    # Display configuration page and return to existing tab
    if (navbar == "config") {
      updateTabsetPanel(session, "navbar", selected = current_tab)
      showModal(
        modalDialog(
          ui_config("ui_config"),
          title = "Configure",
          size = "l",
          easyClose = FALSE,
          fade = TRUE
        )
      )
    } else {
      current_tab <<- navbar
    }
  })
})