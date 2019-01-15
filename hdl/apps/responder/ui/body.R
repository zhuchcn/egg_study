tabItemGenerator = function(tabName){
    tabItem(
        tabName = tabName,
        fluidRow(
            tabsetPanel(
                type = "pills",
                tabPanel(
                    "Model 1", 
                    tagList(
                        column(
                            width = 6,
                            box(width = NULL,
                                DT::DTOutput(paste0(tabName, '_stat1'))
                            )
                        ),
                        column(
                            width = 6,
                            box(width = NULL,
                                plotlyOutput(paste0(tabName, '_boxplot1'), 
                                             height = "500px")
                            )
                        )
                    )
                ),
                tabPanel(
                    "Model 2",
                    column(
                        width = 6,
                        box(width = NULL,
                            DT::DTOutput(paste0(tabName, '_stat2'))
                        )
                    ),
                    column(
                        width = 6,
                        box(width = NULL,
                            plotlyOutput(paste0(tabName, '_boxplot2'), 
                                         height = "500px")
                        )
                    )
                )
            )
        )
    )
}

body = dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
        tabItemGenerator("lpd"),
        tabItemGenerator("fct"),
        tabItemGenerator("cli"),
        tabItemGenerator("diet")
    )
)
