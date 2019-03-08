# Sat Mar  2 10:05:50 2019 ------------------------------
# UI and Server for overview page


# Utility function --------------------------------------------------------
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
    highcharter::hc_yAxis(labels = list(format = '{value}%'))
  
  return(hc_plot)
}

# use memoise to speedup repeated run
hc_plot_returns_mem <- memoise::memoise(hc_plot_returns)

# UI ----------------------------------------------------------------------
ui_overview <- function(id) {
  ns <- NS(id)
  
  fluidPage(fluidRow(
    column(
      width = 8,
      h3("Cumulative Returns"),
      highcharter::highchartOutput(ns("vami_chart"))
    ),
    
    column(width = 4,
           h3("Summary"),
           uiOutput(ns(
             "performance_summary"
           )))
  ),
  
  fluidRow(
    h3("Calendar Returns"),
    uiOutput(ns("calendar_returns"))
  ))
  
}



# Server ------------------------------------------------------------------
server_overview <-
  function(input,
           output,
           session,
           attr_use) {
    ns <- session$ns
    
    returns <-
      reactive({
        attr_use() %>%
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
      reactive({
        prices %>%
          dplyr::filter(ticker %in% c("SPY", "URTH")) %>%
          dplyr::group_by(ticker) %>%
          dplyr::mutate(returns = adj / dplyr::lag(adj) - 1) %>%
          data.frame(stringsAsFactors = FALSE, check.names = FALSE) %>%
          dplyr::select(tradeday, account = ticker, returns) %>%
          dplyr::filter(tradeday >= min(cum_returns()$tradeday)) %>%
          dplyr::mutate(account = ifelse(account == "SPY", "S&P 500", "MSCI World"))
      })
    
    index_cum_returns <-
      reactive({
        index_returns() %>%
          dplyr::group_by(account) %>%
          dplyr::mutate(returns = cumprod(1 + returns)) %>%
          dplyr::mutate(returns = returns - 1)
      })
    
    
    output$vami_chart <-
      highcharter::renderHighchart(hc_plot_returns_mem(rbind(cum_returns(), index_cum_returns())))
    
    
    output$performance_summary <- renderUI({
      do.call(tabsetPanel, c(id = 't', lapply(unique(returns()$account), function(account_use) {
        returns_use <-
          dplyr::filter(returns(), account == account_use) %>%
          dplyr::select(tradeday, returns) %>%
          data.frame(stringsAsFactors = FALSE)
        result <- data.frame()
        result[1, 1] <- as.character(max((returns_use$tradeday)))
        names(result)[1] <- ""
        rownames(result)[1] <- "Report Date: "
        
        
        returns_use <-
          xts::xts(returns_use[, -1], order.by = returns_use[, 1])
        returns_use <-
          xts::apply.monthly(returns_use, function(x) {
            prod(1 + x) - 1
          })
        
        result[2, 1] <-
          as.character(scales::percent(
            PerformanceAnalytics::Return.annualized(returns_use)[1]
          ))
        rownames(result)[2] <- "Annualized Return"
        result[3, 1] <-
          as.character(scales::percent(
            PerformanceAnalytics::StdDev.annualized(returns_use)[1]
          ))
        rownames(result)[3] <- "Annualized Volatility"
        result[4, 1] <-
          as.character(
            round(
              PerformanceAnalytics::Return.annualized(returns_use)[1] / PerformanceAnalytics::StdDev.annualized(returns_use)[1],
              2
            )
          )
        rownames(result)[4] <- "Sharpe"
        result[5, 1] <-
          as.character(scales::percent(PerformanceAnalytics::maxDrawdown(returns_use)[1]))
        rownames(result)[5] <- "Maximum Drawdown"
        
        tabPanel(
          title = account_use,
          DT::datatable(
            data = result,
            style = "default",
            options = list(dom = 't', ordering = FALSE),
            width = "300px",
            height = "200px",
            colnames = c("")
          )
        )
      })))
    })
    
    output$calendar_returns <- renderUI({
      do.call(tabsetPanel, c(id = 't', lapply(unique(returns()$account), function(account_use) {
        returns_use <-
          dplyr::filter(returns(), account == account_use) %>%
          dplyr::select(tradeday, returns) %>%
          data.frame(stringsAsFactors = FALSE)
        returns_use <-
          xts::xts(returns_use[, -1], order.by = returns_use[, 1])
        names(returns_use) <- account_use
        returns_use <-
          xts::apply.monthly(returns_use, function(x) {
            prod(1 + x) - 1
          })
        
        tabPanel(
          title = account_use,
          PerformanceAnalytics::table.CalendarReturns(returns_use, digits = 2) %>%
            DT::datatable(
              data = .,
              style = "default",
              options = list(dom = 't', ordering = FALSE),
              width = "800px",
              height = "300px",
            ) %>%
            DT::formatRound(columns = c(1:13), digits = 2)
        )
      })))
    })
  }
