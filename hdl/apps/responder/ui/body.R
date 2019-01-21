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

cliTab = tabItem(
    tabName = "cli",
    fluidRow(
        tabsetPanel(
            type = "pills",
            tabPanel(
                "Model 1", 
                tagList(
                    column(
                        width = 6,
                        box(width = NULL,
                            DT::DTOutput('cli_stat1')
                        )
                    ),
                    column(
                        width = 6,
                        box(width = NULL,
                            plotlyOutput('cli_boxplot1', height = "500px")
                        )
                    ),
                    column(
                        width = 12,
                        downloadButton("cli_rmd1", "Download Summarized Table",
                                       class = "btn-primary")
                    )
                )
            ),
            tabPanel(
                "Model 2",
                column(
                    width = 6,
                    box(width = NULL,
                        DT::DTOutput('cli_stat2')
                    )
                ),
                column(
                    width = 6,
                    box(width = NULL,
                        plotlyOutput('cli_boxplot2', height = "500px")
                    )
                ),
                column(
                    width = 12,
                    downloadButton("cli_rmd2", "Download Summarized Table",
                                   class = "btn-primary")
                )
            )
        )
    )
)

body = dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
        tags$script(src = "scripts.js")
    ),
    tabItems(
        tabItemGenerator("lpd"),
        tabItemGenerator("fct"),
        cliTab,
        tabItemGenerator("diet")
    )
)
