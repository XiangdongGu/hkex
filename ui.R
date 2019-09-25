dashboardPage(
  dashboardHeader(title = "Hong Kong Exchange"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Equities", tabName = "equities", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      # Equities tab
      tabItem(
        "equities",
        # Today's stock summary
        fluidRow(box(
          DT::dataTableOutput("stock_sum"),
          status = "success", width = 12
        )),
        # By stock view
        fluidRow(box(
          dygraphOutput("adj_close"),
          dygraphOutput("rsi"),
          dygraphOutput("macd"),
          status = "info", width = 12, title = textOutput("sel_stock")
        ))
      )
    )
  )
)
