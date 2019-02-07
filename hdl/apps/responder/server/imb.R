# Create a new mset object for the lpd data. Will be used for both statistic
# test and visualization

imb_data1 = getData1(input, data, "imb")

imb_data2 = reactive({
    mset = imb_data1()
    subset_samples(mset, mset$sample_table$Treatment == "egg")
})

# stat table
imb_stat1 = reactive({
    fitStatModel1(imb_data1(), "imb")
})

imb_stat2 = reactive({
    fitStatModel2(imb_data2(), "imb")
})

output$imb_stat1 = DT::renderDT(
    imb_stat1() %>%
        datatable(selection = list(mode = "single", selected = 1)) %>%
        formatRound(1:5, 3), 
    server=T)

imb_stat1_selector = reactive({
    rownames(imb_stat1())[input$imb_stat1_rows_selected]
})

output$imb_boxplot1 = renderPlotly({
    plot_boxplot(imb_data1(), feature = imb_stat1_selector(), x = "Timepoint",
                 cols = "Treatment", rows = "Responder", line = "Subject", 
                 color = "Subject") +
        labs(title = imb_stat1_selector())
})

output$imb_stat2 = DT::renderDT(
    imb_stat2() %>%
        datatable(selection = list(mode = "single", selected = 1)) %>%
        formatRound(1:5, 3), 
    server=T)

imb_stat2_selector = reactive({
    rownames(imb_stat2())[input$imb_stat2_rows_selected]
})

output$imb_boxplot2 = renderPlotly({
    plot_boxplot(imb_data2(), feature = imb_stat2_selector(), x = "Timepoint",
                 cols = "Responder", line = "Subject", 
                 color = "Subject") +
        labs(title = imb_stat2_selector())
})