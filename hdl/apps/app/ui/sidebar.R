sidebar = dashboardSidebar(
    uiOutput("VarsInput"),
    sidebarMenu(
        id = "sidebar",
        menuItem(
            icon = icon("caret-right"), "Lipidome", 
            menuSubItem("Boxplot", tabName = "lpd_boxplot"),
            menuSubItem("Histograms", tabName = "lpd_hist"),
            menuSubItem("vs Ion Morbility", tabName = "lpd_imb"),
            menuSubItem("vs HDL Function", tabName = "lpd_fct"),
            menuSubItem("vs Clinical Values", tabName = "lpd_cli")
        ),
        menuItem(
            icon = icon("caret-right"), "Ion Morbility",
            menuSubItem("Boxplot", tabName = "imb_boxplot"),
            menuSubItem("vs HDL Function", tabName = "imb_fct"),
            menuSubItem("vs Clinical Values", tabName = "imb_cli")
        ),
        menuItem(
            icon = icon("caret-right"), "HDL Functions",
            menuSubItem("Boxplot", tabName = "fct_boxplot"),
            menuSubItem("vs HDL Function", tabName = "fct_fct"),
            menuSubItem("vs Clinical Values", tabName = "fct_cli")
        )
    )
)