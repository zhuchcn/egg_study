boxplotTabGen = function(tabName) {
    type = strsplit(tabName, "_")[[1]][1]
    tabItem(
        tabName = tabName,
        fluidRow(
            column(
                width = 6,
                box(width = NULL,
                    DTOutput(paste0(type, "_limma")), height = "90%")
            ),
            column(
                width = 6,
                box(width = NULL, 
                    plotlyOutput(paste0(type, "_boxplot")))
            )
        )
    )
}

histTabGen = function(tabName) {
    type = substr(tabName,0,3)
    tabItem(
        tabName = tabName,
        fluidRow(
            column(
                width = 6,
                box(width = NULL,
                    plotlyOutput(paste0(type, "_hist_pval")), height = "100%"),
                box(width = NULL,
                    plotlyOutput(paste0(type, "_hist_padj")), height = "100%")
            ),
            column(
                width = 6,
                box(width = NULL,
                    plotlyOutput(paste0(type, "_volcano")), height = "100%")
            )
        )
    )
}

corrTabGen = function(tabName) {
    tabItem(
        tabName = tabName,
        fluidRow(
            column(
                width = 6,
                box(width = NULL,
                    DTOutput(paste0(tabName, "_dt")), height = "100%" )
            ),
            column(
                width = 6,
                box(width = NULL, height = "10%",
                    column(
                        width = 6, 
                        uiOutput(paste0(tabName, "_Selector")))
                ),
                box(width = NULL, height = "90%",
                    plotlyOutput(paste0(tabName,"_scatter")))
            )
        )
    )
}

lpdPieChart = tabItem(
    tabName = "lpd_pie",
    fluidRow(
        column(
            width = 6,
            box(width = NULL,
                tableOutput("lpd_pie_tbl"), height = "100%")
        ),
        column(
            width = 6,
            box(width = NULL,
                plotOutput("lpd_pie"))
        )
    )
)

body = dashboardBody(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
        boxplotTabGen("lpd_boxplot"),
        histTabGen("lpd_hist"),
        lpdPieChart,
        corrTabGen("lpd_imb"),
        corrTabGen("lpd_fct"),
        corrTabGen("lpd_cli"),
        boxplotTabGen("imb_boxplot"),
        histTabGen("imb_hist"),
        corrTabGen("imb_fct"),
        corrTabGen("imb_cli"),
        corrTabGen("imb_diet"),
        boxplotTabGen("fct_boxplot"),
        corrTabGen("fct_fct"),
        corrTabGen("fct_cli"),
        boxplotTabGen("cli_boxplot"),
        boxplotTabGen("diet_boxplot")
    )
)