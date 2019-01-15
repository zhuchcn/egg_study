sidebar = dashboardSidebar(
    sidebarMenu(
        menuItem("Home", icon = icon("home"), newtab = FALSE,
                 href = "http://www.chenghaozhu.net/studies/egg/hdl.html")
    ),
    uiOutput("VarsInput"),
    sidebarMenu(
        id = "sidebar",
        menuItem("Lipidome", tabName = "lpd", newtab = FALSE),
        menuItem("HDL Functions", tabName = "fct", newtab = FALSE),
        menuItem("Clinical Values", tabName = "cli", newtab = FALSE),
        menuItem("Diet", tabName = "diet", newtab = FALSE)
    )
)