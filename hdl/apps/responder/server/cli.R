# Create a new mset object for the lpd data. Will be used for both statistic
# test and visualization

cli_data1 = getData1(input, data, "cli")

cli_data2 = reactive({
    mset = cli_data1()
    subset_samples(mset, mset$sample_table$Treatment == "egg")
})

# stat table
cli_stat1 = reactive({
    fitStatModel1(cli_data1(), "cli")
})

cli_stat2 = reactive({
    fitStatModel2(cli_data2(), "cli")
})

output$cli_stat1 = DT::renderDT(
    cli_stat1() %>%
        datatable(selection = list(mode = "single", selected = 1)) %>%
        formatRound(1:5, 3), 
    server=T)

cli_stat1_selector = reactive({
    rownames(cli_stat1())[input$cli_stat1_rows_selected]
})

output$cli_boxplot1 = renderPlotly({
    plot_boxplot(cli_data1(), feature = cli_stat1_selector(), x = "Timepoint",
                 cols = "Treatment", rows = "Responder", line = "Subject", 
                 color = "Subject") +
        labs(title = cli_stat1_selector())
})

output$cli_stat2 = DT::renderDT(
    cli_stat2() %>%
        datatable(selection = list(mode = "single", selected = 1)) %>%
        formatRound(1:5, 3), 
    server=T)

cli_stat2_selector = reactive({
    rownames(cli_stat2())[input$cli_stat2_rows_selected]
})

output$cli_boxplot2 = renderPlotly({
    plot_boxplot(cli_data2(), feature = cli_stat2_selector(), x = "Timepoint",
                 cols = "Responder", line = "Subject", 
                 color = "Subject") +
        labs(title = cli_stat2_selector())
})

## Genearate a Table in Word
output$cli_rmd1 = downloadHandler(
    filename = "cli_table.docx",
    content = function(file) {
        tempFile = file.path(tempdir(), "cli_table.Rmd")
        file.copy("rmd/cli_table.Rmd", tempFile, overwrite = TRUE)
        
        params = list(data = cli_data1(),
                      pval = cli_stat1()$pvalue,
                      model = "Model 1")
        
        rmarkdown::render(tempFile, output_format = "word_document", 
                          output_file = file, params = params,
                          envir = new.env(parent = globalenv()))
    }
)

output$cli_rmd2 = downloadHandler(
    filename = "cli_table.docx",
    content = function(file) {
        tempFile = file.path(tempdir(), "cli_table.Rmd")
        file.copy("rmd/cli_table.Rmd", tempFile, overwrite = TRUE)
        
        params = list(data = cli_data2(),
                      pval = cli_stat2()$pvalue,
                      model = "Model 2")
        
        rmarkdown::render(tempFile, output_format = "word_document", 
                          output_file = file, params = params,
                          envir = new.env(parent = globalenv()))
    }
)
