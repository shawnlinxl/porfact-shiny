# Thu Feb 28 01:08:32 2019 ------------------------------
# Load csv data for shiny app


# Load Library ------------------------------------------------------------
library(readr)

github <-"https://raw.githubusercontent.com/shawnlinxl/data-warehouse/master/csv/"

# Load data ---------------------------------------------------------------
prices <-
  read_csv(paste0(github, "prices.csv"),
           col_types = cols(tradeday = col_date(format = "%Y-%m-%d")))

holdings <-
  read_csv(paste0(github, "holdings.csv"),
           col_types = cols(tradeday = col_date(format = "%Y-%m-%d")))

nav <-
  read_csv(paste0(github, "nav.csv"),
           col_types = cols(tradeday = col_date(format = "%Y-%m-%d")))

attr <-
  read_csv(paste0(github, "attr.csv"),
           col_types = cols(tradeday = col_date(format = "%Y-%m-%d")))

account_use <- unique(nav$account)
