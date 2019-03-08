# Thu Feb 28 03:45:55 2019 ------------------------------
# Explorer Page


# Utility function --------------------------------------------------------
hc_plot_ohlc <- function(ticker_use, start_date) {
  ticker_price <-
    prices %>%
    dplyr::filter(ticker == ticker_use & tradeday >= start_date) %>%
    dplyr::select(tradeday, open, high, low, close) %>%
    data.frame(stringsAsFactors = FALSE)
  data_plot <-
    xts::xts(ticker_price[, -1], order.by = ticker_price[, 1])
  hc_plot <-
    highcharter::highchart(type = "stock") %>%
    highcharter::hc_add_series(data_plot, name = ticker_use) %>%
    highcharter::hc_title(text = ticker_use) %>%
    highcharter::hc_rangeSelector(selected = "4") %>%
    highcharter::hc_scrollbar(enabled = FALSE) %>%
    highcharter::hc_navigator(enabled = FALSE)
  
  return(hc_plot)
}

# use memoise to speedup repeated run
hc_plot_mem <- memoise::memoise(hc_plot_ohlc)

# UI ----------------------------------------------------------------------
ui_explorer <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    h2("Explore stock prices"),
    
    fluidRow(
      column(
        width = 6,
        selectizeInput(
          inputId = ns("ticker"),
          label = "Ticker",
          choices = unique(prices$ticker),
          selected = unique(prices$ticker[1:3]),
          multiple = TRUE
        )
      ),
      
      column(
        width = 6,
        dateInput(
          inputId = ns("start_date"),
          label = "Start Date",
          min = min(prices$tradeday),
          max = max(prices$tradeday),
          value = "2018-01-01",
        )
      )
    ),

    
    htmlOutput(ns("hcplot"))
  )
}



# Server ------------------------------------------------------------------
server_explorer <- function(input, output, session) {
  ohlc_plot <- reactive({
    result <- list()
    for (ticker_use in input$ticker) {
      result <- rlist::list.append(result, hc_plot_mem(ticker_use=ticker_use, start_date = input$start_date))
    }
    
    highcharter::hw_grid(result, ncol = 3)
  })
  
  output$hcplot <- renderUI(ohlc_plot())
}
