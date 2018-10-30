output$imb_cli_Selector = renderUI({
    selectInput("imb_cli", "Select Clinical Variable",
                choices = featureNames(data$data$cli),
                selected = featureNames(data$data$cli)[1])
})

imb_cli_dt = reactive({
    data$corr$imb$cli[[input$imb.method]][[input$imb_cli]] %>%
        rownames_to_column("Feature") %>%
        arrange(pval) %>%
        sapply(function(col){
            if(!is.numeric(col)) return(col)
            round(col, digits = 3)
        }) %>%
        as.data.frame %>%
        column_to_rownames("Feature")
})

output$imb_cli_dt = renderDT(
    imb_cli_dt(), 
    selection = list(mode = "single", selected = 1),
    server=T
)
imb_cli_selector = reactive({
    rownames(imb_cli_dt())[input$imb_cli_dt_rows_selected]
})

output$imb_cli_scatter = renderPlotly({
    df = data.frame(
        x = data$data$imb$conc_table[imb_cli_selector(),],
        y = data$data$cli$conc_table[input$imb_cli,],
        Treatment = data$data$imb$sample_table$Treatment,
        Timepoint = data$data$imb$sample_table$Timepoint,
        Subject = data$data$imb$sample_table$Subject
    )
    p = ggscatterplot(df, "x", "y", color = "Subject", color.pal = pal_jama()(7)) +
        labs(x = paste0(imb_cli_selector()), 
             y = input$imb_cli)
    ggplotly(p)
})