prt_limma_table = reactive({
    data$lm$prt[[input$prt.level]]
})

prt_limma = reactive({
    prt_limma_table()%>%
        rownames_to_column("Feature") %>%
        arrange(pvalue) %>%
        sapply(function(col){
            if(!is.numeric(col)) return(col)
            round(col, digits = 3)
        }) %>%
        as.data.frame %>%
        column_to_rownames("Feature")
}) 

output$prt_limma = renderDT(
    prt_limma(), 
    selection = list(mode = "single", selected = 1),
    server=T
)

prt_boxplot_selector = reactive({
    rownames(prt_limma())[input$prt_limma_rows_selected]
})

output$prt_boxplot = renderPlotly({
    mset = data$data$prt[[input$prt.level]]
    p = plot_boxplot(mset, 
                     x = "Timepoint", 
                     feature = prt_boxplot_selector(),
                     cols = "Responding",
                     line = "Subject",
                     color = "Subject",
                     color.pal = pal_jama()(7)) +
        labs(x = "", y = "")
    ggplotly(p)
})