# output$prt_fct_Selector = renderUI({
#     selectInput("prt_fct", "Select HDL Function Variable",
#                 choices = featureNames(data$data$fct),
#                 selected = featureNames(data$data$fct)[1])
# })

prt_fct_dt = reactive({
    data$corr$prt$fct[[input$prt.level]][[input$prt.method]][[input$prt_fct]] %>%
        rownames_to_column("Feature") %>%
        arrange(pval) %>%
        sapply(function(col){
            if(!is.numeric(col)) return(col)
            round(col, digits = 3)
        }) %>%
        as.data.frame %>%
        column_to_rownames("Feature")
})

output$prt_fct_dt = renderDT(
    prt_fct_dt(), 
    selection = list(mode = "single", selected = 1),
    server=T
)
prt_fct_selector = reactive({
    rownames(prt_fct_dt())[input$prt_fct_dt_rows_selected]
})

output$prt_fct_scatter = renderPlotly({
    df = data.frame(
        x = data$data$prt[[input$prt.level]]$conc_table[prt_fct_selector(),],
        y = data$data$fct$conc_table[input$prt_fct,],
        Treatment = data$data$fct$sample_table$Treatment,
        Timepoint = data$data$fct$sample_table$Timepoint,
        Subject = data$data$fct$sample_table$Subject
    )
    p = ggscatterplot(df, "x", "y", color = "Subject", color.pal = pal_jama()(7)) +
        labs(x = paste0(prt_fct_selector(), " [", input$prt.norm, "]"), 
             y = input$prt_fct)
    ggplotly(p)
})