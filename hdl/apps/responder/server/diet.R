# Create a new mset object for the lpd data. Will be used for both statistic
# test and visualization
diet_data1 = reactive({
    responders = ifelse(data$fct$sample_table$Subject %in% input$responders, 
                        "Responder", "Non-responder")
    responders = factor(responders, levels = c("Non-responder", "Responder"))
    names(responders) = data$fct$sample_table$Subject
    mset = data$diet[[input$diet_level]]
    mset$sample_table$Responder = responders[as.character(mset$sample_table$Subject)]
    mset
})

diet_data2 = reactive({
    mset = diet_data1()
    subset_samples(mset, mset$sample_table$Treatment == "egg")
})

# stat table
diet_stat1 = reactive({
    mset = diet_data1()
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

diet_stat2 = reactive({
    mset = diet_data2()
    
    df = mset$conc_table %>%
        t %>% as.data.frame %>%
        cbind(mset$sample_table[,c("Subject", "Timepoint", "Responder")]) %>%
        melt(id.vars = c("Subject", "Timepoint", "Responder")) %>%
        dcast(Subject + variable + Responder ~ Timepoint) %>%
        mutate(value = Post - Pre) %>%
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