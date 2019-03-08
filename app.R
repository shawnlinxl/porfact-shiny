# Thu Feb 28 01:21:11 2019 ------------------------------
# Shiny app main


# Source components -------------------------------------------------------
source("components/libs.R")
source("components/data_loader.R")
source("components/module_configure.R")
source("components/module_analytics.R")
source("components/module_overview.R")
source("components/module_explorer.R")
source("components/ui.R")
source("components/server.R")


shinyApp(ui, server)
