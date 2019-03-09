# Thu Feb 28 01:18:44 2019 ------------------------------
# Load libraries for the shiny app

library(shiny)
library(readr)
library(PerformanceAnalytics)
library(argonDash)
library(argonR)
library(dplyr)
library(xts)
library(highcharter)
library(DT)

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
hc_plot_ohlc_mem <- memoise::memoise(hc_plot_ohlc)

hc_plot_returns <- function(cum_returns) {
  hc_plot <-
    highcharter::highchart(type = "stock")
  
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
    highcharter::hc_legend(enabled = TRUE) %>%
    highcharter::hc_scrollbar(enabled = FALSE) %>%
    highcharter::hc_yAxis(labels = list(format = '{value}%')) %>%
    highcharter::hc_tooltip(pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>VAMI: {point.y}</b> (Period Return: {point.change}%)<br/>',
                            valueDecimals = 0)
  
  return(hc_plot)
}

# use memoise to speedup repeated run
hc_plot_returns_mem <- memoise::memoise(hc_plot_returns)
