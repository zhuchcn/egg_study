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

prtHeatmapTab = tabItem(
    tabName = "prt_heatmap",
    fluidRow(
        column(
            width = 9,
            box(
                width = NULL,
                tabsetPanel(
                    tabPanel(
                        "Heatmap",
                        shinyjqui::jqui_resizable(plotlyOutput("prt_heatmap"))
                    ),
                    tabPanel(
                        "PCA",
                        shinyjqui::jqui_resizable(plotlyOutput("prt_pca"))
                    )
                )
            )
        ),
        column(
            width = 3,
            box(
                width = NULL,
                radioButtons(
                    "prt.cutoff_type","Filter proteins",
                    choices = c("p-values", "abundance"),
                    inline = TRUE
                ),
                tags$div(
                    class = "col-md-6",
                    style = "padding-left: 0px",
                    numericInput("prt.cutoff", "P-value cut off",
                                 min = 0, max = 1, value = 0.4, step = 0.05)
                ),
                tags$div(
                    class = "col-md-6",
                    style = "padding-right: 0px",
                    numericInput("prt.topn", "Most abundant",
                                 min = 1, max = 73, step = 1, value = 20)
                ),
                tags$hr(),
                selectInput("prt.exclude", "Proteins to exclude",
                            choices = featureNames(data$data$prt$iBAQ),
                            multiple = TRUE),
                tags$hr(),
                radioButtons("prt.scale", "A scale method",
                             choiceNames = c(
                                 "Z-score scale", "Absolute scale",
                                 "log scale", "loged z-score scale", 
                                 "loged absolute scale"
                             ),
                             choiceValues = c(
                                 "z-score", "abs-scale", "log", "log z-score",
                                 "log abs-score"
                             )),
                tags$hr(),
                checkboxInput("prt.collapse", "Collapse between pre & post?",
                              value = FALSE),
                tags$hr(),
                tags$button(
                    type = "button",
                    class = "btn btn-danger",
                    'data-toggle' = "popover",
                    title = "Resize it?",
                    'data-content' = "If the aspect ratio look strange, resize it by dragging on the little triangle at the right bottom cornor.",
                    'data-placement' = "bottom",
                    'data-trigger' ="focus",
                    "Resize it?"
                )
            )
        )
    )
)

body = dashboardBody(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
        # Proteome
        boxPlotTabGenerator("prt_boxplot"),
        prtHeatmapTab,
        corrPlotTabGenerator("prt_lpd"),
        corrPlotTabGenerator("prt_fct"),
        ## Lipidome
        boxPlotTabGenerator("lpd_boxplot"),
        ## Functions
        boxPlotTabGenerator("fct_boxplot")
    ),
    tags$script(src = "scripts.js")
)