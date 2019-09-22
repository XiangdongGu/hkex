library(tidyverse)
library(readxl)
library(rvest)
library(RPostgreSQL)
source("utils.R")

equities <- get_equities()

con <- psql_con()

codes <- equities$code
codes <- codes[order(runif(length(codes)))]

# Loadd all codes in equities
for (code in codes) {
  msg <- try(load_stock(con, code))
  if (inherits(msg, "try-error")) {
    Sys.sleep(3)
    msg <- try(load_stock(con, code))
  }
  cat(sprintf("%s for %s\n", msg, code))
}
