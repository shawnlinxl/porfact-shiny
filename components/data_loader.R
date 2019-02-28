# Thu Feb 28 01:08:32 2019 ------------------------------
# Load csv data for shiny app


# Load Library ------------------------------------------------------------
library(readr)


# Load data ---------------------------------------------------------------
prices <-
  read_csv(file = "data/prices.csv",
           col_types = cols(tradeday = col_date(format = "%Y-%m-%d %H:%M:%S")))

holdings <-
  read_csv(file = "data/holdings.csv",
           col_types = cols(tradeday = col_date(format = "%Y-%m-%d")))
