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
  msg <- load_stock(con, code)
  cat(sprintf("%s for %s\n", msg, code))
}
