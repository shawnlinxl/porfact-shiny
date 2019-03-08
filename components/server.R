# Thu Feb 28 01:36:57 2019 ------------------------------
# Shiny server backend script

server <- shinyServer(function(input, output, session) {
  current_tab  <- "overview"
  data <- callModule(data_config, "ui_config")
  callModule(server_analytics, "ui_analytics", holdings = data$holdings)
  callModule(server_explorer, "ui_explorer")
  callModule(server_config, "ui_config", data$nav)
  callModule(server_overview, "ui_overview", attr = data$attr)
  
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