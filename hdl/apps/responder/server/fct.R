# Create a new mset object for the lpd data. Will be used for both statistic
# test and visualization

fct_data1 = getData1(input, data, "fct")

fct_data2 = reactive({
    mset = fct_data1()
    subset_samples(mset, mset$sample_table$Treatment == "egg")
})

# stat table
fct_stat1 = reactive({
    fitStatModel1(fct_data1(), "fct")
})

fct_stat2 = reactive({
    fitStatModel2(fct_data2(), "fct")
})

output$fct_stat1 = DT::renderDT(
    fct_stat1() %>%
        datatable(selection = list(mode = "single", selected = 1)) %>%
        formatRound(1:5, 3), 
    server=T)

fct_stat1_selector = reactive({
    rownames(fct_stat1())[input$fct_stat1_rows_selected]
})

output$fct_boxplot1 = renderPlotly({
    plot_boxplot(fct_data1(), feature = fct_stat1_selector(), x = "Timepoint",
                 cols = "Treatment", rows = "Responder", line = "Subject", 
                 color = "Subject") +
        labs(title = fct_stat1_selector())
})

output$fct_stat2 = DT::renderDT(
    fct_stat2() %>%
        datatable(selection = list(mode = "single", selected = 1)) %>%
        formatRound(1:5, 3), 
    server=T)

fct_stat2_selector = reactive({
    rownames(fct_stat2())[input$fct_stat2_rows_selected]
})

output$fct_boxplot2 = renderPlotly({
    plot_boxplot(fct_data2(), feature = fct_stat2_selector(), x = "Timepoint",
                 cols = "Responder", line = "Subject", 
                 color = "Subject") +
        labs(title = fct_stat2_selector())
})