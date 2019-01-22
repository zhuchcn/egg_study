# Create a new mset object for the lpd data. Will be used for both statistic
# test and visualization

lpd_data1 = getData1(input, data, "lpd")

lpd_data2 = reactive({
    mset = lpd_data1()
    subset_samples(mset, mset$sample_table$Treatment == "egg")
})

lpd_stat1 = reactive({
    fitStatModel1(lpd_data1(), type = "lpd")
})

lpd_stat2 = reactive({
    fitStatModel2(lpd_data2(), "lpd")
})

output$lpd_stat1 = DT::renderDT(
    lpd_stat1() %>%
        datatable(selection = list(mode = "single", selected = 1)) %>%
        formatRound(1:5, 3), 
    server=T)

lpd_stat1_selector = reactive({
    rownames(lpd_stat1())[input$lpd_stat1_rows_selected]
})

output$lpd_boxplot1 = renderPlotly({
    plot_boxplot(lpd_data1(), feature = lpd_stat1_selector(), x = "Timepoint",
                 cols = "Treatment", rows = "Responder", line = "Subject", 
                 color = "Subject") +
        labs(title = lpd_stat1_selector())
})

output$lpd_stat2 = DT::renderDT(
    lpd_stat2() %>%
        datatable(selection = list(mode = "single", selected = 1)) %>%
        formatRound(1:5, 3), 
    server=T)

lpd_stat2_selector = reactive({
    rownames(lpd_stat2())[input$lpd_stat2_rows_selected]
})

output$lpd_boxplot2 = renderPlotly({
    plot_boxplot(lpd_data2(), feature = lpd_stat2_selector(), x = "Timepoint",
                 cols = "Responder", line = "Subject", 
                 color = "Subject") +
        labs(title = lpd_stat2_selector())
})