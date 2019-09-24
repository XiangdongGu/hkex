library(tidyverse)
library(RPostgreSQL)
source("utils.R")

con <- psql_con()

data <- dbGetQuery(con, "select code, date, adj_close from stock
                   order by code, date")

data <- data %>% 
  arrange(code, date) %>%
  group_by(code) %>%
  mutate(
    rsi = compute_rsi(adj_close),
    trade_rsi = rsi_trade(rsi),
    macd = compute_macd(adj_close),
    macd_sig = macd_signal(macd, 9),
    trade_macd = macd_trade(macd, macd_sig)
  )

data <- data %>% select(-adj_close)

dbSendQuery(con, "drop table if exists signal")
for (cd in unique(data$code)) {
  d <- data %>% filter(code == cd)
  dbWriteTable(con, "signal", d, row.names = FALSE, append = TRUE)
}
