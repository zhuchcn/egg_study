boxPlotTabGenerator = function(tabName) {
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

corrPlotTabGenerator = function(tabName) {
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
                box(width = NULL, height = "90%",
                    plotlyOutput(paste0(tabName,"_scatter")))
            )
        )
    )
}

body = dashboardBody(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
        # Proteome
        boxPlotTabGenerator("prt_boxplot"),
        corrPlotTabGenerator("prt_lpd"),
        corrPlotTabGenerator("prt_fct"),
        ## Lipidome
        boxPlotTabGenerator("lpd_boxplot"),
        ## Functions
        boxPlotTabGenerator("fct_boxplot")
    )
)