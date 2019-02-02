output$imb_diet_Selector = renderUI({
    tagList(
        selectInput("imb_diet_level", "Normalize the diet data or not?",
                    choices = names(data$data$diet),
                    selected = names(data$data$diet)[1]),
        uiOutput("imb_diet_Selector2")
    )
})

output$imb_diet_Selector2 = renderUI({
    selectInput("imb_diet", "Select Diet Variable",
                choices = featureNames(data$data$diet[[input$imb_diet_level]]),
                selected = featureNames(data$data$diet[[input$imb_diet_level]])[1])
})

imb_diet_dt = reactive({
    data$corr$imb$diet[[input$imb_diet_level]][[input$imb.method]][[input$imb_diet]] %>%
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
        x = data$data$diet[[input$imb_diet_level]]$conc_table[input$imb_diet,],
        y = data$data$imb$conc_table[imb_diet_selector(),],
        Treatment = data$data$imb$sample_table$Treatment,
        Timepoint = data$data$imb$sample_table$Timepoint,
        Subject = data$data$imb$sample_table$Subject
    )
    p = ggscatterplot(df, x = "x", y = "y", color = "Subject", color.pal = pal_jama()(7)) +
        labs(x = input$imb_diet, 
             y = paste0(imb_diet_selector()))
    ggplotly(p)
})