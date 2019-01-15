sidebar = dashboardSidebar(
    sidebarMenu(
        menuItem("Home", icon = icon("home"), newtab = FALSE,
                 href = "http://www.chenghaozhu.net/studies/egg/hdl.html")
    ),
    selectInput(
        "responders","Select Responders",
        choices = levels(data$fct$sample_table$Subject),
        selected = c(101, 104, 105, 107, 108, 110, 111, 113, 114, 119, 123, 124),
        multiple = TRUE,
        selectize = TRUE
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