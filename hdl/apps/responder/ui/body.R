tabItemGenerator = function(tabName){
    tabItem(
        tabName = tabName,
        fluidRow(
            tabsetPanel(
                type = "pills",
                id = paste0(tabName, "_tab"),
                tabPanel(
                    "Model 1", 
                    value = paste0(tabName, "_model1"),
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
                        ),
                        tags$div(
                            if(tabName == "cli") {
                                column(
                                    width = 12,
                                    downloadButton("cli_rmd1", "Download Summarized Table",
                                                   class = "btn-primary disabled")
                                )
                            }
                        )
                    )
                ),
                tabPanel(
                    "Model 2",
                    value = paste0(tabName, "_model2"),
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
                    ),
                    tags$div(
                        if (tabName == "cli") {
                            column(
                                width = 12,
                                downloadButton("cli_rmd2", "Download Summarized Table",
                                               class = "btn-primary disabled")
                            )
                        }
                    )
                )
            )
        )
    )
}

body = dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
        tags$script(src = "scripts.js")
    ),
    tabItems(
        tabItemGenerator("lpd"),
        tabItemGenerator("fct"),
        tabItemGenerator("cli"),
        tabItemGenerator("diet")
    )
)
