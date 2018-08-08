cli_limma = data$limma$cli %>%
    rownames_to_column("Feature") %>%
    arrange(P.Value) %>%
    sapply(function(col){
        if(!is.numeric(col)) return(col)
        round(col, digits = 3)
    }) %>%
    as.data.frame %>%
    column_to_rownames("Feature")

output$cli_limma = renderDT(
    cli_limma, 
    selection = list(mode = "single", selected = 1),
    server=T
)

cli_boxplot_selector = reactive({
    rownames(cli_limma)[input$cli_limma_rows_selected]
})

output$cli_boxplot = renderPlotly({
    mset = data$data$cli
    p = plot_boxplot(mset, 
                     x = "Timepoint", 
                     feature = cli_boxplot_selector(),
                     cols = "Treatment",
                     line = "Subject",
                     color = "Subject",
                     color.pal = pal_jama()(7)) +
        labs(x = "")
    ggplotly(p)
})