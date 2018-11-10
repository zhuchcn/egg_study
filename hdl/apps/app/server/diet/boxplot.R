diet_limma = data$limma$diet %>%
    rownames_to_column("Feature") %>%
    arrange(pvalue) %>%
    sapply(function(col){
        if(!is.numeric(col)) return(col)
        round(col, digits = 3)
    }) %>%
    as.data.frame %>%
    column_to_rownames("Feature")

output$diet_limma = renderDT(
    diet_limma, 
    selection = list(mode = "single", selected = 1),
    server=T
)

diet_boxplot_selector = reactive({
    rownames(diet_limma)[input$diet_limma_rows_selected]
})

output$diet_boxplot = renderPlotly({
    mset = data$data$diet
    p = plot_boxplot(mset, 
                     x = "Timepoint", 
                     feature = diet_boxplot_selector(),
                     cols = "Treatment",
                     line = "Subject",
                     color = "Subject",
                     color.pal = pal_jama()(7)) +
        labs(x = "")
    ggplotly(p)
})