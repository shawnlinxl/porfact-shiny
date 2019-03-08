# Sat Mar  2 10:05:50 2019 ------------------------------
# UI and Server for overview page


# Utility function --------------------------------------------------------
hc_plot_returns <- function(cum_returns) {
  hc_plot <-
    highcharter::highchart(type = "stock") %>%
    highcharter::hc_title(text = "VAMI")
  
  for (account_use in unique(cum_returns$account)) {
    data_plot <-
      cum_returns %>%
      dplyr::filter(account == account_use) %>%
      data.frame(stringsAsFactors = FALSE) %>%
      dplyr::select(tradeday, returns) %>%
      dplyr::mutate(returns = (1 + returns) * 1000)
    data_plot <-
      xts::xts(data_plot[, -1], order.by = data_plot[, 1])
    hc_plot <-
      hc_plot %>%
      highcharter::hc_add_series(data_plot, name = account_use, compare =
                                   "percent")
  }
  
  hc_plot <-
    hc_plot %>%
    highcharter::hc_legend(display = TRUE)
  
  return(hc_plot)
}

# use memoise to speedup repeated run
hc_plot_returns_mem <- memoise::memoise(hc_plot_returns)

# UI ----------------------------------------------------------------------
ui_overview <- function(id) {
  ns <- NS(id)
  
  fluidPage(fluidRow(column(
    width = 6,
    highcharter::highchartOutput(ns("vami_chart"))
  )))
}



# Server ------------------------------------------------------------------
server_overview <-
  function(input,
           output,
           session,
           attr) {

    returns <-
      reactive({
        attr() %>%
          dplyr::group_by(tradeday, account) %>%
          dplyr::summarize(returns = sum(attr, na.rm = TRUE))
      })

    cum_returns <-
      reactive({
        returns() %>%
          dplyr::group_by(account) %>%
          dplyr::mutate(returns = cumprod(1 + returns)) %>%
          dplyr::mutate(returns = returns - 1)
      })
    
    index_returns <-
      prices %>%
      dplyr::filter(ticker %in% c("SPY", "URTH")) %>%
      dplyr::group_by(ticker) %>%
      dplyr::mutate(returns = adj/dplyr::lag(adj) - 1) %>%
      dplyr::select(tradeday, ticker, returns)
    

    output$vami_chart <-
      highcharter::renderHighchart(hc_plot_returns_mem(cum_returns()))
  }
