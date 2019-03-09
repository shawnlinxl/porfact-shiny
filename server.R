# Thu Feb 28 01:36:57 2019 ------------------------------
# Shiny server backend script


library(magrittr)

server <- shinyServer(function(input, output, session) {
  current_tab  <- "overview"
  
  observeEvent(input$daterange, {
    start_date <<- input$daterange[1]
    end_date <<- input$daterange[2]
  })
  
  observe({
    min_date <-
      min(dplyr::filter(nav, account == input$account)$tradeday)
    max_date <-
      max(dplyr::filter(nav, account == input$account)$tradeday)
    updateDateRangeInput(
      session,
      "daterange",
      start = min_date,
      end = max_date,
      min = min_date,
      max = max_date
    )
  })
  
  observeEvent(input$account, {
    account_use <<- input$account
  })
  
  
  returns <-
    reactive({
      attr %>%
        dplyr::filter(account %in% input$account) %>%
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
          width = "200px",
          height = "200px",
          colnames = c(""),
          class = "nowrap compact"
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
            height = "150px",
            class = "nowrap compact"
          ) %>%
          DT::formatRound(columns = c(1:13), digits = 2)
      )
    })))
  })
  
  ohlc_plot <- reactive({
    result <- list()
    for (ticker_use in input$ticker) {
      result <- rlist::list.append(result, hc_plot_mem(ticker_use=ticker_use, start_date = input$start_date))
    }
    
    highcharter::hw_grid(result, ncol = 3)
  })
  
  output$hcplot <- renderUI(ohlc_plot())
  
  
  output$allocation <- renderUI({
    do.call(tabsetPanel, c(id = 't', lapply(input$account, function(account_use) {
      holdings_table <-
        dplyr::filter(holdings, account == account_use)  %>%
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
  
})