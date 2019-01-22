# Create a new mset object for the lpd data. Will be used for both statistic
# test and visualization

diet_data1 = getData1(input, data, "diet")

diet_data2 = reactive({
    mset = diet_data1()
    subset_samples(mset, mset$sample_table$Treatment == "egg")
})

# stat table
diet_stat1 = reactive({
    fitStatModel1(diet_data1(), "diets")
})

diet_stat2 = reactive({
    fitStatModel2(diet_data2(), "diet")
})

output$diet_stat1 = DT::renderDT(
    diet_stat1() %>%
        datatable(selection = list(mode = "single", selected = 1)) %>%
        formatRound(1:5, 3), 
    server=T)

diet_stat1_selector = reactive({
    rownames(diet_stat1())[input$diet_stat1_rows_selected]
})

output$diet_boxplot1 = renderPlotly({
    plot_boxplot(diet_data1(), feature = diet_stat1_selector(), x = "Timepoint",
                 cols = "Treatment", rows = "Responder", line = "Subject", 
                 color = "Subject") +
        labs(title = diet_stat1_selector())
})

output$diet_stat2 = DT::renderDT(
    diet_stat2() %>%
        datatable(selection = list(mode = "single", selected = 1)) %>%
        formatRound(1:5, 3), 
    server=T)

diet_stat2_selector = reactive({
    rownames(diet_stat2())[input$diet_stat2_rows_selected]
})

output$diet_boxplot2 = renderPlotly({
    plot_boxplot(diet_data2(), feature = diet_stat2_selector(), x = "Timepoint",
                 cols = "Responder", line = "Subject", 
                 color = "Subject") +
        labs(title = diet_stat2_selector())
})