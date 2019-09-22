library(shiny)
library(DT)
source("utils.R")

## app.R ##
server <- function(input, output, session) {
  con <- psql_con()
  session$onSessionEnded(function() {
    dbDisconnect(con)
  })
  
  output$sel <- renderUI({
    codes <- dbGetQuery(con, "select distinct code from stock order by code")
    codes <- codes$code
    selectInput("code", "Stock Code", codes)
  })
  
  output$stock <- DT::renderDT({
    code <- input$code
    req(!is.null(code))
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
  uiOutput("sel"),
  DT::dataTableOutput("stock"),
  title = "Hong Kong Exchange Stock Data"
)

shinyApp(ui = ui, server = server)
