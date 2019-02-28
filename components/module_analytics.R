# Thu Feb 28 02:12:23 2019 ------------------------------
# UI and Server for analytics page


# UI ----------------------------------------------------------------------
ui_analytics <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      
      column(
        width = 6,
        
        h2("Holdings"),

        # Selection ---------------------------------------------------------------
        fluidRow(
          column(
            width = 6,
            dateInput(
              inputId = ns("holdings_date"),
              label = "Date",
              value = max(holdings$tradeday),
              min = min(holdings$tradeday),
              max = max(holdings$tradeday)
            )
          ),
          
          column(
            width = 6,
            selectizeInput(
              inputId = ns("account"),
              label = "Account",
              choices = unique(holdings$account),
              selected = unique(holdings$account),
              multiple = TRUE
            )
          )
        ),
        

        # Show holdings table -----------------------------------------------------
        DT::dataTableOutput(ns("holdings_table"))
      )
    )
  )
}



# Server ------------------------------------------------------------------
server_analytics <- function(input, output, session) {
  holdings_table <- reactive({
    holdings %>%
      dplyr::filter(tradeday == input$holdings_date & account %in% input$account) %>%
      dplyr::select(-tradeday) %>%
      dplyr::group_by(ticker) %>%
      dplyr::summarize(quantity = sum(quantity, na.rm = TRUE)) %>%
      dplyr::arrange(ticker) %>%
      DT::datatable(style = "default",
                    options = list(pageLength = 20, dom = 'tip'))

  })
  
  output$holdings_table <- DT::renderDataTable(holdings_table())
}
