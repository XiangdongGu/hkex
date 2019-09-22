library(tidyverse)
library(readxl)
library(rvest)
library(RPostgreSQL)
source("utils.R")

equities <- get_equities()

con <- psql_con()

# Loadd all codes in equities
for (code in equities$code) load_stock(con, code)
