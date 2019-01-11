pkgs = c("shiny", "dplyr", "reshape2", "stringr", "tibble", 
         "DT", "plotly", "Metabase", "shinydashboard")
for(pkg in pkgs) {
    library(pkg, character.only = T, quietly = T, verbose = F, warn.conflicts = F)
}

load("data/data.rda")
# source()

# Define UI for application that draws a histogram
ui <- dashboardPage(
    header = dashboardHeader(
        title = "Responder vs Non-responder",
        titleWidth = "30%"
    ),
    sidebar = dashboardSidebar(disable = TRUE),
    body = dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        fluidRow(
            column(
                width = 12,
                box(
                    width = NULL,
                    column(
                        width = 4,
                        selectInput(
                            "responders","Select Responders", 
                            choices = levels(data$fct$Subject),
                            selected = c(101, 104, 105, 107, 108, 110, 111, 113, 114, 119, 123, 124),
                            multiple = TRUE,
                            selectize = TRUE
                        )
                    ),
                    column(
                        width = 4,
                        selectInput(
                            'lpd_level', "Lipid class, species, or summarized values",
                            choices = names(data$lpd), selected = names(data$lpd)[1]
                        )
                    ),
                    column(
                        width = 4,
                        uiOutput('lpdNormSelect')
                    )
                )
            )
        ),
        fluidRow(
            column(
                width = 6,
                box(width = NULL,
                    DT::DTOutput('lpd_stat')
                )
            ),
            column(
                width = 6,
                box(width = NULL,
                    plotlyOutput('lpd_boxplot', height = "500px")
                )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # lpd norm method selector
    output$lpdNormSelect = renderUI({
        choices = names(data$lpd[[input$lpd_level]])
        selectInput(
            "lpd_norm", "Select a Normalization Method",
            choices = choices, selected = choices[1]
        )
    })
    
    # Create a new mset object for the lpd data. Will be used for both statistic
    # test and visualization
    lpd_data = reactive({
        responders = ifelse(data$fct$Subject %in% input$responders, 
                            "Responder", "Non-responder")
        responders = factor(responders, levels = c("Non-responder", "Responder"))
        names(responders) = data$fct$Subject
        mset = data$lpd[[input$lpd_level]][[input$lpd_norm]]
        mset$sample_table$Responder = responders[mset$sample_table$Subject]
        mset
    })
    
    # stat table
    stat_table = reactive({
        mset =  t(lpd_data()$conc_table) %>%
            as.data.frame() %>%
            cbind(lpd_data()$sample_table[,c("Subject", "Timepoint", "Treatment", "Responder")]) %>%
            melt(id.vars=c("Subject", "Treatment", "Timepoint", "Responder"),
                 variable.name = "Lipid") %>%
            dcast(Subject + Treatment + Responder + Lipid ~ Timepoint) %>%
            mutate(value = post - pre) %>%
            dcast(Subject + Responder + Lipid ~ Treatment, value.var = "value") %>%
            mutate(value = egg - sub) %>%
            dcast(Subject + Responder ~ Lipid, value.var = "value")
        rownames(mset) = mset$Subject
        mset = LipidomicsSet(
            conc_table = conc_table(t(mset[,-(1:2)])),
            sample_table = sample_table(mset[,1:2])
        )
        design = model.matrix(data = as(mset$sample_table, "data.frame"),~ Responder)
        mSet_limma(mset, design, coef = 2, p.value = 2) %>%
            rownames_to_column("feature") %>%
            arrange(pvalue) %>%
            column_to_rownames("feature")
    })
    
    output$lpd_stat = DT::renderDT(
        stat_table() %>%
            datatable(selection = list(mode = "single", selected = 1)) %>%
            formatRound(1:5, 2), 
        server=T)
    
    lpd_stat_selector = reactive({
        rownames(stat_table())[input$lpd_stat_rows_selected]
    })
    
    output$lpd_boxplot = renderPlotly({
        plot_boxplot(lpd_data(), feature = lpd_stat_selector(), x = "Timepoint",
                     cols = "Treatment", rows = "Responder", line = "Subject", 
                     color = "Subject") +
            labs(title = lpd_stat_selector())
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

