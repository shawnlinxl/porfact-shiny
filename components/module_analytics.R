# Thu Feb 28 02:12:23 2019 ------------------------------
# UI and Server for analytics page


# UI ----------------------------------------------------------------------
ui_analytics <- function(id) {
  
  ns <- NS(id)
  
  # Show holdings table -----------------------------------------------------
  DT::dataTableOutput(ns("holdings_table"))
}



# Server ------------------------------------------------------------------
server_analytics <- function(input, output, session, holdings) {
  holdings_table <- reactive({
    holdings() %>%
      dplyr::filter(tradeday == max(holdings()$tradeday)) %>%
      dplyr::select(-tradeday, -account) %>%
      dplyr::group_by(ticker) %>%
      dplyr::summarize(quantity = sum(quantity, na.rm = TRUE)) %>%
      dplyr::arrange(ticker) %>%
      DT::datatable(style = "default",
                    options = list(pageLength = 20, dom = 'tip'))

  })
  
  output$holdings_table <- DT::renderDataTable(holdings_table())
}
