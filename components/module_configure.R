# Fri Mar  1 04:37:42 2019 ------------------------------

account_use <- unique(nav$account)
start_date <- min(nav$tradeday)
end_date <- max(nav$tradeday)

# UI ----------------------------------------------------------------------
ui_config <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    h2("Configure data used for all pages on this portal"),
    
    selectizeInput(
      inputId = ns("account"),
      label = "Account",
      choices = unique(nav$account),
      selected = account_use,
      multiple = TRUE
    ),
    
    dateRangeInput(
      inputId = ns("daterange"),
      label = "Date Range",
      min = start_date,
      max = end_date,
      start = start_date,
      end = end_date
    )
  )
  
}


# Server ------------------------------------------------------------------
server_config <- function(input, output, session, nav) {
  observeEvent(input$daterange, {
    start_date <<- input$daterange[1]
    end_date <<- input$daterange[2]
  })
  
  observe({
    min_date <- min(nav()$tradeday)
    max_date <- max(nav()$tradeday)
    
    updateDateRangeInput(session, "daterange", start = min_date, end = max_date, min = min_date, max = max_date)
  })
  
  observeEvent(input$account, {
    account_use <<- input$account
  })
}

# Data --------------------------------------------------------------------
data_config <- function(input, output, session) {

  
  nav_out <- reactive({
    nav %>%
      dplyr::filter(account %in% input$account)
  })
  
  start_date <- reactive({
    input$daterange[1]
  })
  
  end_date <- reactive({
    input$daterange[2]
  })
  
  holdings_out <- reactive({
    holdings %>%
      dplyr::filter(account %in% input$account, tradeday >= start_date(), tradeday <= end_date())
    
  })
  
  attr_out <- reactive({
    attr %>%
      dplyr::filter(account %in% input$account, tradeday >= start_date(), tradeday <= end_date())
  })
  
  return(
    list(
      holdings = holdings_out,
      nav = nav_out,
      attr = attr_out,
      start_date = start_date,
      end_date = end_date
    )
  )
}
