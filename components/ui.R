# Thu Feb 28 01:10:53 2019 ------------------------------
# UI interface for the shiny app


ui <- shinyUI(
  navbarPage(
    
    theme = shinythemes::shinytheme("flatly"),
    title = "Porfact",
    id = "navbar",

    
    tabPanel(
      title = "Overview",
      ui_overview("ui_overview"),
      value = "overview"
    ),
    
    tabPanel(
      title = "Analytics",
      ui_analytics("ui_analytics"),
      value = "analytics"
    ),
    
   tabPanel(
      title = "Stock Explorer",
      shiny::includeCSS("styles.css"),
      ui_explorer("ui_explorer"),
      value = "explorer"
    ),
   
   tabPanel(
     "Configure",
     value = "config",
     icon = icon("cog")
   )
    
  )
)