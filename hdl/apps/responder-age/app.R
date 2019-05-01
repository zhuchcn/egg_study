pkgs = c("shiny", "dplyr", "Metabase", 'DT', 'plotly')
for(pkg in pkgs){
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

load('../responder/data/data.rda')
lpd = data$lpd$feature$Proportion
responders = c(101, 104, 105, 107, 108, 110, 111, 113, 114, 119, 123, 124)
lpd$sample_table$respond = ifelse(
    lpd$sample_table$Subject %in% responders, "responder", 'non-responder'
)
lpd$sample_table$age_category = ifelse(
    data$cli$sample_table$age < 59, '< 59', '>= 59'
)
lpd = subset_samples(lpd, lpd$sample_table$Treatment == 'egg')

design = model.matrix(
    ~ Timepoint * age_category + Subject, data = as(lpd$sample_table, 'data.frame')
)
lm = mSet_limma(lpd, design, coef = 23)

ui <- fluidPage(
    tags$style(HTML("
    .datatables{
        overflow: auto;
    }
    table {
      white-space: nowrap;
    }    
    ")),
    tags$h3(
        'Responders vs Non-responders seperated by Age at 59'
    ),
    tags$br(),
    tags$div(
        class = 'col-sm-6',
        DTOutput('table')
    ),
    tags$div(
        class = 'col-sm-6',
        plotlyOutput('plot')
    )
)

server <- function(input, output, session) {
    
    output$table = renderDT(
        datatable(
            lm, 
            selection = list(mode = "single", selected = 1),
            options = list(
                order = list(4, "asc")
            ) 
        )%>%
            formatSignif(columns = 1:5, digits = 3)
    )
    
    output$plot = renderPlotly({
        feature = featureNames(lpd)[input$table_rows_selected]
        plot_boxplot(
            lpd, x = "Timepoint", feature = feature, cols = 'respond',
            rows = 'age_category', line = 'Subject', color = 'Subject'
        )
    })
    
}

shinyApp(ui, server)