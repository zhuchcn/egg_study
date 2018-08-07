output$lpd_imb_Selector = renderUI({
    selectInput("lpd_imb", "Select HDL Function Variable",
                choices = featureNames(data$data$imb),
                selected = featureNames(data$data$imb)[1])
})

lpd_imb_dt = reactive({
    data$corr$lpd$imb[[input$lpd.level]][[input$lpd.norm]][[input$lpd.method]][[input$lpd_imb]] %>%
        rownames_to_column("Feature") %>%
        arrange(pval) %>%
        sapply(function(col){
            if(!is.numeric(col)) return(col)
            round(col, digits = 3)
        }) %>%
        as.data.frame %>%
        column_to_rownames("Feature")
})

output$lpd_imb_dt = renderDT(
    lpd_imb_dt(), 
    selection = list(mode = "single", selected = 1),
    server=T
)
lpd_imb_selector = reactive({
    rownames(lpd_imb_dt())[input$lpd_imb_dt_rows_selected]
})

output$lpd_imb_scatter = renderPlotly({
    df = data.frame(
        x = data$data$lpd[[input$lpd.level]][[input$lpd.norm]]$conc_table[lpd_imb_selector(),],
        y = data$data$imb$conc_table[input$lpd_imb,],
        Treatment = data$data$imb$sample_table$Treatment,
        Timepoint = data$data$imb$sample_table$Timepoint,
        Subject = data$data$imb$sample_table$Subject
    )
    p = ggscatterplot(df, "x", "y", color = "Subject", color.pal = pal_jama()(7)) +
        labs(x = paste0(lpd_imb_selector(), " [", input$lpd.norm, "]"), 
             y = input$lpd_imb)
    ggplotly(p)
})