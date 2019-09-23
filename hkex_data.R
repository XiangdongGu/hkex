library(tidyverse)
library(readxl)
library(rvest)
library(RPostgreSQL)
source("utils.R")

con <- psql_con()

# Load stock code
codes <- loade_codes(con)
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

