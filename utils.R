library(tidyverse)
library(readxl)
library(rvest)
library(RPostgreSQL)

# Get all equities-------------------------------------------------------------
get_equities <- function() {
  url <- paste0("https://www.hkex.com.hk/eng/services/trading/securities/",
                "securitieslists/ListOfSecurities.xlsx")
  path <- file.path(tempdir(), "equities.xlsx")
  on.exit(unlink(path))
  download.file(url, path, quiet = TRUE)
  data <- read_excel(path, skip = 2)
  data <- data %>% filter(Category == "Equity")
  data %>% select(
    code = `Stock Code`,
    name = `Name of Securities`
  )
}

# Helper function to convert date to integer to put in scraping URL------------
date_to_int <- function(date) {
  if (is.character(date)) date <- as.Date(date)
  org <- 1569081600L
  intv <- 3600 * 24
  org_date <- as.Date("2019-09-22")
  dif <- as.integer(date - org_date)
  org + intv * dif
}

# Get stock price from start date and end date (need < 100 records)------------
get_stock <- function(code, start, end) {
  period1 <- date_to_int(start)
  period2 <- date_to_int(end)
  url <- sprintf(paste0(
    "https://finance.yahoo.com/quote/%s.HK/history?period1=%s",
    "&period2=%s&interval=1d&filter=history&frequency=1d"),
    code, period1, period2
  )
  data <- read_html(url) %>% html_table()
  if (length(data) == 0) return(NULL)
  data <- data[[1]]
  # Remove the last row, which is commen row
  data <- data[-nrow(data), ]
  # Remove Dividend row
  data <- data %>% filter(!grepl("Dividend", Open))
  # Remove hyphen row
  data <- data %>% filter(Open != "-")
  # If volume is -, then make it 0
  data <- data %>% mutate(Volume = ifelse(Volume == "-", 0, Volume))
  f <- function(x) as.numeric(gsub(",", "", x))
  data <- data %>% mutate(
    Date = as.Date(Date, "%b %d, %Y")
  ) %>% mutate_at(vars(-Date), list(f))
  names(data) <- tolower(gsub("\\*", "", names(data)))
  names(data) <- gsub("[[:space:]]+", "_", names(data))
  data
}

# Get all historical stock pricies for a stock---------------------------------
get_stock_hist <- function(code) {
  end <- Sys.Date()
  data <- list()
  d <- get_stock(code, end - 100, end)
  while(nrow(d) > 0) {
    # print(end)
    data <- bind_rows(data, d)
    end <- end - 101
    d <- get_stock(code, end - 100, end)
  }
  data$code <- code
  data <- data %>% select(code, everything())
  data
}

# create stock table in postgresql database------------------------------------
create_stock_table <- function(con) {
  q <- paste0(
    "create table stock (",
    "code varchar(256),",
    "date date,",
    "open float,",
    "high float,",
    "low float,",
    "close float,",
    "adj_close float,",
    "volume float,",
    "primary key (code, date));"
  )
  dbSendQuery(con, q)
}

# Make postgresql connection---------------------------------------------------
psql_con <- function() {
  con <- dbConnect(
    dbDriver("PostgreSQL"),
    host = Sys.getenv("HOST"),
    dbname = Sys.getenv("DB"),
    user = Sys.getenv("USER"),
    password = Sys.getenv("PASS")
  )
  con
}


