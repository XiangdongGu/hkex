library(shiny)
library(DT)
source("utils.R")

con <- psql_con()
codes <- dbGetQuery(con, "select distinct code from stock order by code")
codes <- codes$code
dbDisconnect(con)

## app.R ##
server <- function(input, output, session) {
  con <- psql_con()
  session$onSessionEnded(function() {
    dbDisconnect(con)
  })
  
  output$stock <- DT::renderDT({
    code <- input$code
    d <- dbGetQuery(con, sprintf(
      "select * from stock where code = '%s'",  code
    ))
    d <- d %>% arrange(desc(date))
    d %>% datatable(
      class = "compact display",
      selection = "single",
      escape = FALSE,
      rownames = FALSE,
      options = list(
        dom = "t p"
      )
    )
  })
}

ui <- fluidPage(
  selectInput("code", "Stock Code", codes),
  DT::dataTableOutput("stock"),
  title = "Hong Kong Exchange Stock Data"
)

shinyApp(ui = ui, server = server)
