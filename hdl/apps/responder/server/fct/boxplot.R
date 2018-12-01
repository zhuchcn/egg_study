fct_limma = data$lm$fct %>%
    rownames_to_column("Feature") %>%
    arrange(pvalue) %>%
    sapply(function(col){
        if(!is.numeric(col)) return(col)
        round(col, digits = 3)
    }) %>%
    as.data.frame %>%
    column_to_rownames("Feature")

output$fct_limma = renderDT(
    fct_limma, 
    selection = list(mode = "single", selected = 1),
    server=T
)

fct_boxplot_selector = reactive({
    rownames(fct_limma)[input$fct_limma_rows_selected]
})

output$fct_boxplot = renderPlotly({
    mset = data$data$fct
    p = plot_boxplot(mset, 
                     x = "Timepoint", 
                     feature = fct_boxplot_selector(),
                     cols = "Responding",
                     line = "Subject",
                     color = "Subject",
                     color.pal = pal_jama()(7)) +
        labs(x = "")
    ggplotly(p)
})