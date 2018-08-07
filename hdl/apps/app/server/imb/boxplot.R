imb_limma = data$limma$imb %>%
    rownames_to_column("Feature") %>%
    arrange(P.Value) %>%
    sapply(function(col){
        if(!is.numeric(col)) return(col)
        round(col, digits = 3)
    }) %>%
    as.data.frame %>%
    column_to_rownames("Feature")

output$imb_limma = renderDT(
    imb_limma, 
    selection = list(mode = "single", selected = 1),
    server=T
)

imb_boxplot_selector = reactive({
    rownames(imb_limma)[input$imb_limma_rows_selected]
})

output$imb_boxplot = renderPlotly({
    mset = data$data$imb
    p = plot_boxplot(mset, 
                     x = "Timepoint", 
                     feature = imb_boxplot_selector(),
                     cols = "Treatment",
                     line = "Subject",
                     color = "Subject",
                     color.pal = pal_jama()(7)) +
        labs(x = "")
    ggplotly(p)
})