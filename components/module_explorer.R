# Thu Feb 28 03:45:55 2019 ------------------------------
# Explorer Page


# Utility function --------------------------------------------------------
hc_plot_ohlc <- function(ticker_use) {
  ticker_price <-
    prices %>%
    dplyr::filter(ticker == ticker_use) %>%
    dplyr::select(tradeday, open, high, low, close) %>%
    data.frame(stringsAsFactors = FALSE)
  data_plot <-
    xts::xts(ticker_price[, -1], order.by = ticker_price[, 1])
  hc_plot <-
    highchart(type = "stock") %>%
    hc_add_series(data_plot, name = ticker_use) %>%
    hc_title(text = ticker_use) %>%
    hc_rangeSelector(selected = "4")
  
  return(hc_plot)
}

# use memoise to speedup repeated run
hc_plot_mem <- memoise::memoise(hc_plot_ohlc)

# UI ----------------------------------------------------------------------
ui_explorer <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    h2("Explore stock prices"),
    
    selectizeInput(
      inputId = ns("ticker"),
      label = "Tickers to plot",
      choices = unique(prices$ticker),
      selected = unique(prices$ticker[1:3]),
      multiple = TRUE
    ),
    
    htmlOutput(ns("hcplot"))
  )
}



# Server ------------------------------------------------------------------
server_explorer <- function(input, output, session) {
  ohlc_plot <- reactive({
    result <- list()
    for (ticker_use in input$ticker) {
      result <- rlist::list.append(result, hc_plot_mem(ticker_use=ticker_use))
    }
    
    hw_grid(result, ncol = 3)
  })
  
  output$hcplot <- renderUI(ohlc_plot())
}
