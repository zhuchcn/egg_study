lpd_limma_table = reactive({
    data$limma$lpd[[input$lpd.level]][[input$lpd.norm]]
})

lpd_limma = reactive({
    lpd_limma_table()%>%
        rownames_to_column("Feature") %>%
        arrange(pvalue) %>%
        sapply(function(col){
            if(!is.numeric(col)) return(col)
            round(col, digits = 3)
        }) %>%
        as.data.frame %>%
        column_to_rownames("Feature")
}) 

output$lpd_limma = renderDT(
    lpd_limma(), 
    selection = list(mode = "single", selected = 1),
    server=T
)

lpd_boxplot_selector = reactive({
    rownames(lpd_limma())[input$lpd_limma_rows_selected]
})

output$lpd_boxplot = renderPlotly({
    mset = data$data$lpd[[input$lpd.level]][[input$lpd.norm]]
    p = plot_boxplot(mset, 
                     x = "Timepoint", 
                     feature = lpd_boxplot_selector(),
                     cols = "Treatment",
                     line = "Subject",
                     color = "Subject",
                     color.pal = pal_jama()(7)) +
        labs(x = "")
    ggplotly(p)
})