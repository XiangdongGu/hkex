library(tidyverse)
library(RPostgreSQL)
source("utils.R")

con <- psql_con()

codes <- dbGetQuery(con, "select distinct code from stock;") %>% pull(1)

for (code in codes) {
  cat(sprintf("Processing %s\n", code))
  data <- dbGetQuery(con, sprintf(
    "select code, date, adj_close from stock where code = '%s'
    order by code, date ", code))
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
  dbSendQuery(con, sprintf(
    "delete from signal where code = '%s'", code
  ))
  dbWriteTable(con, "signal", data, row.names = FALSE, append = TRUE)
}




# dbSendQuery(con, "drop table if exists signal")
for (cd in unique(data$code)) {
  d <- data %>% filter(code == cd)
  dbWriteTable(con, "signal", d, row.names = FALSE, append = TRUE)
}
