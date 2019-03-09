# Thu Feb 28 01:21:11 2019 ------------------------------
# Shiny app main


# Source components -------------------------------------------------------
source("components/libs.R")
source("components/data_loader.R")
source("ui.R")
source("server.R")


shinyApp(ui, server)
