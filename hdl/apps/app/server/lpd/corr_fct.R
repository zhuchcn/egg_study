output$lpd_fct_Selector = renderUI({
    selectInput("lpd_fct", "Select HDL Function Variable",
                choices = featureNames(data$data$fct),
                selected = featureNames(data$data$fct)[1])
})

lpd_fct_dt = reactive({
    data$corr$lpd$fct[[input$lpd.level]][[input$lpd.norm]][[input$lpd.method]][[input$lpd_fct]] %>%
        rownames_to_column("Feature") %>%
        arrange(pval) %>%
        sapply(function(col){
            if(!is.numeric(col)) return(col)
            round(col, digits = 3)
        }) %>%
        as.data.frame %>%
        column_to_rownames("Feature")
})

output$lpd_fct_dt = renderDT(
    lpd_fct_dt(), 
    selection = list(mode = "single", selected = 1),
    server=T
)
lpd_fct_selector = reactive({
    rownames(lpd_fct_dt())[input$lpd_fct_dt_rows_selected]
})

output$lpd_fct_scatter = renderPlotly({
    df = data.frame(
        x = data$data$lpd[[input$lpd.level]][[input$lpd.norm]]$conc_table[lpd_fct_selector(),],
        y = data$data$fct$conc_table[input$lpd_fct,],
        Treatment = data$data$fct$sample_table$Treatment,
        Timepoint = data$data$fct$sample_table$Timepoint,
        Subject = data$data$fct$sample_table$Subject
    )
    p = ggscatterplot(df, "x", "y", color = "Subject", color.pal = pal_jama()(7)) +
        labs(x = paste0(lpd_fct_selector(), " [", input$lpd.norm, "]"), 
             y = input$lpd_fct)
    ggplotly(p)
})