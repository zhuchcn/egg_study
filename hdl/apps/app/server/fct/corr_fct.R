output$fct_fct_Selector = renderUI({
    selectInput("fct_fct", "Select HDL Function Variable",
                choices = featureNames(data$data$fct),
                selected = featureNames(data$data$fct)[1])
})

fct_fct_dt = reactive({
    data$corr$fct$fct[[input$fct.method]][[input$fct_fct]] %>%
        rownames_to_column("Feature") %>%
        arrange(pval) %>%
        sapply(function(col){
            if(!is.numeric(col)) return(col)
            round(col, digits = 3)
        }) %>%
        as.data.frame %>%
        column_to_rownames("Feature")
})

output$fct_fct_dt = renderDT(
    fct_fct_dt(), 
    selection = list(mode = "single", selected = 1),
    server=T
)
fct_fct_selector = reactive({
    rownames(fct_fct_dt())[input$fct_fct_dt_rows_selected]
})

output$fct_fct_scatter = renderPlotly({
    df = data.frame(
        x = data$data$fct$conc_table[fct_fct_selector(),],
        y = data$data$fct$conc_table[input$fct_fct,],
        Treatment = data$data$fct$sample_table$Treatment,
        Timepoint = data$data$fct$sample_table$Timepoint,
        Subject = data$data$fct$sample_table$Subject
    )
    p = ggscatterplot(df, "x", "y", color = "Subject", color.pal = pal_jama()(7)) +
        labs(x = paste0(fct_fct_selector()), 
             y = input$fct_fct)
    ggplotly(p)
})