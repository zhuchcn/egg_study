prt_lpd_dt = reactive({
    data$corr$prt$lpd[[input$prt.level]][[input$prt.lpd_level]][[input$prt.lpd_norm]][[input$prt.method]][[input$prt_lpd]] %>%
        rownames_to_column("Feature") %>%
        arrange(pval) %>%
        sapply(function(col){
            if(!is.numeric(col)) return(col)
            round(col, digits = 3)
        }) %>%
        as.data.frame %>%
        column_to_rownames("Feature")
})

output$prt_lpd_dt = renderDT(
    prt_lpd_dt(), 
    selection = list(mode = "single", selected = 1),
    server=T
)
prt_lpd_selector = reactive({
    rownames(prt_lpd_dt())[input$prt_lpd_dt_rows_selected]
})

output$prt_lpd_scatter = renderPlotly({
    df = data.frame(
        x = data$data$prt[[input$prt.level]]$conc_table[prt_lpd_selector(),],
        y = data$data$lpd[[input$prt.lpd_level]][[input$prt.lpd_norm]]$conc_table[input$prt_lpd,],
        Subject = data$data$prt$iBAQ$sample_table$Subject
    )
    p = ggscatterplot(df, "x", "y", color = "Subject", color.pal = pal_jama()(7)) +
        labs(x = paste0(prt_lpd_selector(), " [", input$prt.norm, "]"), 
             y = input$prt_lpd)
    ggplotly(p)
})