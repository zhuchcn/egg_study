output$imb_fct_Selector = renderUI({
    selectInput("imb_fct", "Select HDL Function Variable",
                choices = featureNames(data$data$fct),
                selected = featureNames(data$data$fct)[1])
})

imb_fct_dt = reactive({
    data$corr$imb$fct[[input$imb.method]][[input$imb_fct]] %>%
        rownames_to_column("Feature") %>%
        arrange(pval) %>%
        sapply(function(col){
            if(!is.numeric(col)) return(col)
            round(col, digits = 3)
        }) %>%
        as.data.frame %>%
        column_to_rownames("Feature")
})

output$imb_fct_dt = renderDT(
    imb_fct_dt(), 
    selection = list(mode = "single", selected = 1),
    server=T
)
imb_fct_selector = reactive({
    rownames(imb_fct_dt())[input$imb_fct_dt_rows_selected]
})

output$imb_fct_scatter = renderPlotly({
    df = data.frame(
        x = data$data$imb$conc_table[imb_fct_selector(),],
        y = data$data$fct$conc_table[input$imb_fct,],
        Treatment = data$data$imb$sample_table$Treatment,
        Timepoint = data$data$imb$sample_table$Timepoint,
        Subject = data$data$imb$sample_table$Subject
    )
    p = ggscatterplot(df, "x", "y", color = "Subject", color.pal = pal_jama()(7)) +
        labs(x = paste0(imb_fct_selector()), 
             y = input$imb_fct)
    ggplotly(p)
})