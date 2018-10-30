sidebar = dashboardSidebar(
    sidebarMenu(
        menuItem("Home", icon = icon("home"), newtab = FALSE,
                 href = "http://www.chenghaozhu.net/studies/egg/docs/hdl.html")
    ),
    uiOutput("VarsInput"),
    sidebarMenu(
        id = "sidebar",
        menuItem(
            icon = icon("caret-right"), "Lipidome", 
            menuSubItem("Boxplot", tabName = "lpd_boxplot"),
            menuSubItem("Histograms", tabName = "lpd_hist"),
            menuSubItem("Pie Chart", tabName = "lpd_pie"),
            menuSubItem("vs Ion Morbility", tabName = "lpd_imb"),
            menuSubItem("vs HDL Function", tabName = "lpd_fct"),
            menuSubItem("vs Clinical Values", tabName = "lpd_cli")
        ),
        menuItem(
            icon = icon("caret-right"), "Ion Morbility",
            menuSubItem("Boxplot", tabName = "imb_boxplot"),
            menuSubItem("vs HDL Function", tabName = "imb_fct"),
            menuSubItem("vs Clinical Values", tabName = "imb_cli"),
            menuSubItem("vs Diet Data", tabName = "imb_diet")
        ),
        menuItem(
            icon = icon("caret-right"), "HDL Functions",
            menuSubItem("Boxplot", tabName = "fct_boxplot"),
            menuSubItem("vs HDL Function", tabName = "fct_fct"),
            menuSubItem("vs Clinical Values", tabName = "fct_cli")
        ),
        menuItem(
            icon = icon("caret-right"), "Clinical Values",
            menuSubItem("Boxplot", tabName = "cli_boxplot")
        ),
        menuItem(
            icon = icon("caret-right"), "Diet",
            menuSubItem("Boxplot", tabName = "diet_boxplot")
        )
    )
)