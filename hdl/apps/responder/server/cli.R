# Create a new mset object for the lpd data. Will be used for both statistic
# test and visualization
cli_data1 = reactive({
    responders = ifelse(data$fct$sample_table$Subject %in% input$responders, 
                        "Responder", "Non-responder")
    responders = factor(responders, levels = c("Non-responder", "Responder"))
    names(responders) = data$fct$sample_table$Subject
    mset = data$cli
    mset$sample_table$Responder = responders[as.character(mset$sample_table$Subject)]
    mset
})

cli_data2 = reactive({
    mset = cli_data1()
    subset_samples(mset, mset$sample_table$Treatment == "egg")
})

# stat table
cli_stat1 = reactive({
    mset = cli_data1()
    mset =  t(mset$conc_table) %>% 
        as.data.frame() %>%
        cbind(mset$sample_table[,c("Subject", "Timepoint", "Treatment", "Responder")]) %>%
        melt(id.vars=c("Subject", "Treatment", "Timepoint", "Responder"),
             variable.name = "Lipid") %>%
        dcast(Subject + Treatment + Responder + Lipid ~ Timepoint) %>%
        mutate(value = Post - Pre) %>%
        dcast(Subject + Responder + Lipid ~ Treatment, value.var = "value") %>%
        mutate(value = egg - sub) %>%
        dcast(Subject + Responder ~ Lipid, value.var = "value")
    rownames(mset) = mset$Subject
    mset = LipidomicsSet(
        conc_table = conc_table(t(mset[,-(1:2)])),
        sample_table = sample_table(mset[,1:2])
    )
    design = model.matrix(data = as(mset$sample_table, "data.frame"),~ Responder)
    mSet_limma(mset, design, coef = 2, p.value = 2) %>%
        rownames_to_column("feature") %>%
        arrange(pvalue) %>%
        column_to_rownames("feature")
})

cli_stat2 = reactive({
    mset = cli_data2()
    design = model.matrix(data = as(mset$sample_table, "data.frame"),
                          ~ Responder * Timepoint + 1)
    mSet_limma(mset, design, coef = 2, p.value = 2) %>%
        rownames_to_column("feature") %>%
        arrange(pvalue) %>%
        column_to_rownames("feature")
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