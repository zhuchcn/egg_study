output$fct_cli_Selector = renderUI({
    selectInput("fct_cli", "Select HDL Function Variable",
                choices = featureNames(data$data$cli),
                selected = featureNames(data$data$cli)[1])
})

fct_cli_dt = reactive({
    data$corr$fct$cli[[input$fct.method]][[input$fct_cli]] %>%
        rownames_to_column("Feature") %>%
        arrange(pval) %>%
        sapply(function(col){
            if(!is.numeric(col)) return(col)
            round(col, digits = 3)
        }) %>%
        as.data.frame %>%
        column_to_rownames("Feature")
})

output$fct_cli_dt = renderDT(
    fct_cli_dt(), 
    selection = list(mode = "single", selected = 1),
    server=T
)
fct_cli_selector = reactive({
    rownames(fct_cli_dt())[input$fct_cli_dt_rows_selected]
})

output$fct_cli_scatter = renderPlotly({
    df = data.frame(
        x = data$data$fct$conc_table[fct_cli_selector(),],
        y = data$data$cli$conc_table[input$fct_cli,],
        Treatment = data$data$fct$sample_table$Treatment,
        Timepoint = data$data$fct$sample_table$Timepoint,
        Subject = data$data$fct$sample_table$Subject
    )
    p = ggscatterplot(df, "x", "y", color = "Subject", color.pal = pal_jama()(7)) +
        labs(x = paste0(fct_cli_selector()), 
             y = input$fct_cli)
    ggplotly(p)
})