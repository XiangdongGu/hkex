shinyServer(function(input, output, session) {
  # Data Base Connection-------------------------------------------------------
  con <- psql_con()
  session$onSessionEnded(function() {
    dbDisconnect(con)
  })
  
  # Get most recent day's summary----------------------------------------------
  max_date <- dbGetQuery(con, "select max(date) from stock") %>% pull("max")
  sumdata <- dbGetQuery(con, sprintf(
    "select a.*, b.name, c.trade_rsi, c.trade_macd
    from stock a inner join stock_code b on a.code = b.code
    inner join signal c on a.code = c.code and a.date = c.date
    where a.date = '%s'", max_date
  ))
  sumdata <- sumdata %>% select(code, name, everything())
  
  # Display summary table------------------------------------------------------
  output$stock_sum <- DT::renderDT({
    sumdata %>% datatable(
      class = "compact display",
      selection = "single",
      rownames = FALSE,
      filter = "top",
      options = list(
        ordering = TRUE,
        dom = "t p"
      ))
  })
  
  # Selected stock data--------------------------------------------------------
  output$sel_stock <- reactive({
    id <- input$stock_sum_rows_selected
    if (is.null(id)) return("Please click above row to select a stock")
    code <- sumdata$code[id]
    name <- sumdata$name[id]
    sprintf("%s (%s)", code, name)
  })
  
  stock_data <- reactive({
    id <- input$stock_sum_rows_selected
    req(!is.null(id))
    code <- sumdata$code[id]
    data <- dbGetQuery(con, sprintf(
      "select b.*, a.adj_close from stock a inner join signal b
      on a.code = b.code and a.date = b.date where a.code = '%s'
      and a.date > '%s'",
      code, max_date - 730
    )) %>% select(-code) %>% arrange(date)
    data
  })
  
  output$adj_close <- renderDygraph({
    d <- stock_data()
    d <- xts(d %>% select(adj_close), order.by = d$date)
    dygraph(d, main = NULL, group = "series") %>% 
      dyAxis("y", label = "Adjsted Close")
  })
  
  output$rsi <- renderDygraph({
    d <- stock_data()
    d <- d %>% 
      select(date, rsi) %>%
      mutate(
        "70% line" = 70,
        "30% line" = 30
      )
    d <- xts(d %>% select(-date), order.by = d$date)
    dygraph(d, main = NULL, group = "series") %>% 
      dyAxis("y", label = "RSI") %>%
      dySeries("70% line", strokePattern = 'dashed') %>%
      dySeries("30% line", strokePattern = 'dashed')
  })
  
  output$macd <- renderDygraph({
    d <- stock_data()
    d <- d %>% select(date, macd, macd_sig)
    d <- xts(d %>% select(-date), order.by = d$date)
    dygraph(d, main = NULL, group = "series") %>% 
      dyAxis("y", label = "MACD")
  })
})
