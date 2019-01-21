# Create a new mset object for the lpd data. Will be used for both statistic
# test and visualization
fct_data1 = reactive({
    responders = ifelse(data$fct$sample_table$Subject %in% input$responders, 
                        "Responder", "Non-responder")
    responders = factor(responders, levels = c("Non-responder", "Responder"))
    names(responders) = data$fct$sample_table$Subject
    mset = data$fct
    mset = subset_features(mset, 2:9)
    mset$sample_table$Responder = responders[as.character(mset$sample_table$Subject)]
    mset
})

fct_data2 = reactive({
    mset = fct_data1()
    subset_samples(mset, mset$sample_table$Treatment == "egg")
})

# stat table
fct_stat1 = reactive({
    mset = fct_data1()
    mset =  t(mset$conc_table) %>% 
        as.data.frame() %>%
        cbind(mset$sample_table[,c("Subject", "Timepoint", "Treatment", "Responder")]) %>%
        melt(id.vars=c("Subject", "Treatment", "Timepoint", "Responder"),
             variable.name = "Lipid") %>%
        dcast(Subject + Treatment + Responder + Lipid ~ Timepoint) %>%
        mutate(value = post - pre) %>%
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

fct_stat2 = reactive({
    mset = fct_data2()
    
    df = mset$conc_table %>%
        t %>% as.data.frame %>%
        cbind(mset$sample_table[,c("Subject", "Timepoint", "Responder")]) %>%
        melt(id.vars = c("Subject", "Timepoint", "Responder")) %>%
        dcast(Subject + variable + Responder ~ Timepoint) %>%
        mutate(value = post - pre) %>%
        dcast(Subject + Responder ~ variable)
    
    edata = df[,-2] %>%
        column_to_rownames("Subject") %>% t %>%
        conc_table
    
    pdata = df[,1:2]
    rownames(pdata) = pdata$Subject
    pdata = sample_table(pdata)
    
    mset = MultxSet(conc_table = edata, sample_table = pdata)
    
    design = model.matrix(~ Responder, data = as(mset$sample_table, "data.frame"))
    mSet_limma(mset, design, coef = 2, p.value = 2) %>%
        rownames_to_column("feature") %>%
        arrange(pvalue) %>%
        column_to_rownames("feature")
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