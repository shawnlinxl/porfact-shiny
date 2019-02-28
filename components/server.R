# Thu Feb 28 01:36:57 2019 ------------------------------
# Shiny server backend script

server <- shinyServer(function(input, output, session){
  
  analytics <- callModule(server_analytics, "ui_analytics")
  explorer <- callModule(server_explorer, "ui_explorer")
  
})