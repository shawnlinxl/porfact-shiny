# Thu Feb 28 02:12:23 2019 ------------------------------
# UI and Server for analytics page


# UI ----------------------------------------------------------------------
ui_analytics <- function(id) {
  
  ns <- NS(id)
  
  # Show holdings table -----------------------------------------------------
  fluidPage(
    fluidRow(
      column(width=6,
             h3("Holdings"),
             uiOutput(ns("holdings_table"))
    ),
    column(width=6,
           h3("Allocation"),
           uiOutput(ns("allocation")))
  )
  )
  
}



# Server ------------------------------------------------------------------
server_analytics <- function(input, output, session, holdings) {

  output$holdings_table <- renderUI({
      do.call(tabsetPanel, c(id = 't', lapply(unique(holdings()$account), function(account_use) {
        holdings_table <-
          dplyr::filter(holdings(), account == account_use) %>%
          dplyr::filter(tradeday == max(tradeday))  %>%
          dplyr::select(-tradeday, -account) %>%
          dplyr::group_by(ticker) %>%
          dplyr::summarize(quantity = sum(quantity, na.rm = TRUE)) %>%
          dplyr::arrange(ticker) %>%
          DT::datatable(style = "default",
                        options = list(pageLength = 20, dom = 'tip'),
                        width = "400px",
                        height = "800px",
                        rownames = NULL)
        
        tabPanel(
          title = account_use,
          holdings_table
        )
      })))
    })
  
  
    output$allocation <- renderUI({
      do.call(tabsetPanel, c(id = 't', lapply(unique(holdings()$account), function(account_use) {
        holdings_table <-
          dplyr::filter(holdings(), account == account_use)  %>%
          dplyr::filter(tradeday == max(tradeday))  %>%
          dplyr::left_join(prices, by = c("tradeday", "ticker")) %>%
          dplyr::left_join(nav, by = c("tradeday", "account")) %>%
          dplyr::select(-tradeday, -account) %>%
          dplyr::group_by(ticker) %>%
          dplyr::summarize(value = sum(quantity * close/nav, na.rm = TRUE)) %>%
          dplyr::arrange(-value)
        
        holdings_table <- rbind(holdings_table, data.frame(ticker = "Cash", value = 1 - sum(holdings_table$value)))
        
        result <-
          highcharter::highchart() %>%
          highcharter::hc_chart(type = "pie") %>%
          highcharter::hc_add_series_labels_values(labels = holdings_table$ticker, values = holdings_table$value * 100)
        
        
        tabPanel(
          title = account_use,
          result
        )
      })))
    })
  
}
