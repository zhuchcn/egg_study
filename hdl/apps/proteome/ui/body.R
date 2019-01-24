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
                        plotlyOutput("prt_pca")
                    )
                )
            )
        ),
        column(
            width = 3,
            box(
                width = NULL,
                numericInput("prt.cutoff", "P-value cut off",
                             min = 0, max = 1, value = 0.4),
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
                shinyjs::disabled(
                    numericInput("prt.hm_ht", "Adjust the height of the heatmap",
                                 min = 400, max = 1000, value = 400)
                ),
                tags$button(
                    type = "button",
                    class = "btn btn-danger",
                    'data-toggle' = "popover",
                    title = "Why is it disabled?",
                    'data-content' = "The heatmap can now be resized by dragging the little triangle at the right bottom corner. Figured that's a better UI..",
                    'data-placement' = "bottom",
                    'data-trigger' ="focus",
                    "Why is it disabled?"
                )
            )
        )
    )
)

body = dashboardBody(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
        tags$script(src = "scripts.js")
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
    )
)