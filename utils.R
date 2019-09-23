library(tidyverse)
library(readxl)
library(rvest)
library(RPostgreSQL)
Sys.setenv(TZ = "Asia/Hong_Kong")

# Get all equities-------------------------------------------------------------
get_equities <- function() {
  url <- paste0("https://www.hkex.com.hk/eng/services/trading/securities/",
                "securitieslists/ListOfSecurities.xlsx")
  path <- file.path(tempdir(), "equities.xlsx")
  on.exit(unlink(path))
  download.file(url, path, quiet = TRUE)
  data <- read_excel(path, skip = 2)
  data <- data %>% filter(Category == "Equity")
  data <- data %>% select(
    code = `Stock Code`,
    name = `Name of Securities`
  )
  # remove leading 0
  data <- data %>%
    mutate(leading = substr(code, 1, 1)) %>%
    filter(leading == '0') %>%
    mutate(code = substring(code, 2)) %>%
    select(-leading)
  data
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
  # Remove stock splick
  data <- data %>% filter(!grepl("Stock Split", Open))
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
  data$code <- rep(code, nrow(data))
  data <- data %>% select(code, everything())
  data
}

# Get all historical stock pricies for a stock---------------------------------
get_stock_hist <- function(code) {
  end <- Sys.Date()
  data <- list()
  d <- get_stock(code, end - 100, end)
  if (is.null(d)) return(NULL)
  while(nrow(d) > 0) {
    # print(end)
    data <- bind_rows(data, d)
    end <- end - 101
    d <- get_stock(code, end - 100, end)
  }
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

# Reload whole history of a stock----------------------------------------------
reload_stock <- function(con, code) {
  dbSendQuery(con, sprintf(
    "delete from stock where code = '%s'", code))
  data <- get_stock_hist(code)
  if (is.null(data) | length(data) == 0) return()
  dbWriteTable(con, "stock", data, row.names = FALSE, append = TRUE)
}

# Load codes-------------------------------------------------------------------
loade_codes <- function(con) {
  equities <- try(get_equities())
  if (inherits(equities, 'try-error')) {
    equities <- dbGetQuery(con, "select * from stock_code;")
  } else {
    dbWriteTable(con, "stock_code", equities, row.names = FALSE,
                 overwrite = TRUE)
  }
  equities
}
  
# Retrieve and load data to database-------------------------------------------
load_stock <- function(con, code) {
  # check existing records
  loaded <- dbGetQuery(con, sprintf(
    "select max(date) as max from stock where code = '%s'", code))
  max <- loaded$max
  # load all historical values if not loaded
  if (is.na(max)) {
    reload_stock(con, code)
    return("Reloaded whole data")
  }
  # if loaded difference is greater than 80 days, reload history
  today <- Sys.Date()
  dif <- as.numeric(Sys.Date() - max)
  if (dif > 80) {
    reload_stock(con, code)
    return("Reloaded whole data")
  }
  # load max and prior 20 days as check for any value adjustments
  # compare same date records with loaded
  data <- get_stock(code, max - 20, today)
  dl <- dbGetQuery(con, sprintf(
    "select * from stock where code = '%s' and date >= '%s'",
    code, as.character(max - 20)
  ))
  data_dif <- data %>% select(code, date, close) %>%
    inner_join(dl %>% select(code, date, close1 = close),
               by = c("code", "date")) %>%
    mutate(delta = abs(1 - close1/close))
  # if there are discrepencies between loaded and new retrieved, reload
  if (!all(data_dif$delta < 0.001)) {
    reload_stock(con, code)
    return("Reloaded whole data")
  }
  # load the new data
  data <- data %>% filter(date > max)
  dbWriteTable(con, "stock", data, row.names = FALSE, append = TRUE)
  return(sprintf("Loaded new %s records", nrow(data)))
}

