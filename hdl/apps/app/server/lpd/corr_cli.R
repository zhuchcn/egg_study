output$lpd_cli_Selector = renderUI({
    selectInput("lpd_cli", "Select HDL Function Variable",
                choices = featureNames(data$data$cli),
                selected = featureNames(data$data$cli)[1])
})

lpd_cli_dt = reactive({
    data$corr$lpd$cli[[input$lpd.level]][[input$lpd.norm]][[input$lpd.method]][[input$lpd_cli]] %>%
        rownames_to_column("Feature") %>%
        arrange(pval) %>%
        sapply(function(col){
            if(!is.numeric(col)) return(col)
            round(col, digits = 3)
        }) %>%
        as.data.frame %>%
        column_to_rownames("Feature")
})

output$lpd_cli_dt = renderDT(
    lpd_cli_dt(), 
    selection = list(mode = "single", selected = 1),
    server=T
)
lpd_cli_selector = reactive({
    rownames(lpd_cli_dt())[input$lpd_cli_dt_rows_selected]
})

output$lpd_cli_scatter = renderPlotly({
    df = data.frame(
        x = data$data$lpd[[input$lpd.level]][[input$lpd.norm]]$conc_table[lpd_cli_selector(),],
        y = data$data$cli$conc_table[input$lpd_cli,],
        Treatment = data$data$cli$sample_table$Treatment,
        Timepoint = data$data$cli$sample_table$Timepoint,
        Subject = data$data$cli$sample_table$Subject
    )
    p = ggscatterplot(df, "x", "y", color = "Subject", color.pal = pal_jama()(7)) +
        labs(x = paste0(lpd_cli_selector(), " [", input$lpd.norm, "]"), 
             y = input$lpd_cli)
    ggplotly(p)
})