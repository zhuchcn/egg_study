output$imb_diet_Selector = renderUI({
    selectInput("imb_diet", "Select Diet Variable",
                choices = featureNames(data$data$diet),
                selected = featureNames(data$data$diet)[1])
})

imb_diet_dt = reactive({
    data$corr$imb$diet[[input$imb.method]][[input$imb_diet]] %>%
        rownames_to_column("Feature") %>%
        arrange(pval) %>%
        sapply(function(col){
            if(!is.numeric(col)) return(col)
            round(col, digits = 3)
        }) %>%
        as.data.frame %>%
        column_to_rownames("Feature")
})

output$imb_diet_dt = renderDT(
    imb_diet_dt(), 
    selection = list(mode = "single", selected = 1),
    server=T
)
imb_diet_selector = reactive({
    rownames(imb_diet_dt())[input$imb_diet_dt_rows_selected]
})

output$imb_diet_scatter = renderPlotly({
    df = data.frame(
        x = data$data$imb$conc_table[imb_diet_selector(),],
        y = data$data$diet$conc_table[input$imb_diet,],
        Treatment = data$data$imb$sample_table$Treatment,
        Timepoint = data$data$imb$sample_table$Timepoint,
        Subject = data$data$imb$sample_table$Subject
    )
    p = ggscatterplot(df, "x", "y", color = "Subject", color.pal = pal_jama()(7)) +
        labs(x = paste0(imb_diet_selector()), 
             y = input$imb_diet)
    ggplotly(p)
})