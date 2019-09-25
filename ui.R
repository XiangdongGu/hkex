dashboardPage(
  dashboardHeader(title = "Hong Kong Exchange"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Equities", tabName = "equities", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("equities")
    )
  )
)
