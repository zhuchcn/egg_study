sidebar = dashboardSidebar(
    sidebarMenu(
        menuItem("Home", icon = icon("home"), newtab = FALSE,
                 href = "http://www.chenghaozhu.net/studies/egg/hdl.html")
    ),
    uiOutput("VarsInput"),
    sidebarMenu(
        id = "sidebar",
        menuItem(
            icon = icon("caret-right"), "Proteome", 
            menuSubItem("Box Plot", tabName = "prt_boxplot"),
            menuSubItem("vs Lipidome", tabName = "prt_lpd"),
            menuSubItem("vs HDL Functions", tabName = "prt_fct")
        ),
        menuItem(
            icon = icon("caret-right"), "Lipidome",
            menuSubItem("Box Plot", tabName = "lpd_boxplot")
        ),
        menuItem(
            icon = icon("caret-right"), "HDL Function",
            menuSubItem("Box Plot", tabName = "fct_boxplot")
        )
    )
)