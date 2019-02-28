# Thu Feb 28 01:10:53 2019 ------------------------------
# UI interface for the shiny app


ui <- shinyUI(
  navbarPage(
    
    title = "Porfact",
    id = "navbar",

    tabPanel(
      title = "Overview",
      value = "overview",
      "test1"
    ),
    
    tabPanel(
      title = "Analytics",
      ui_analytics("ui_analytics"),
      value = "analytics"
    ),
    
   tabPanel(
      title = "Stock Explorer",
      ui_explorer("ui_explorer"),
      value = "explorer"
    )   
    
  )
)