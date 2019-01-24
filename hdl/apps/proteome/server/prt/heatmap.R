output$ui_prt_heatmap = renderUI({
    plotlyOutput("prt_heatmap", height = paste0(input$prt.hm_ht, "px"))
})

prtDataForHeatMap = reactive({
    mset = data$data$prt[[input$prt.level]]
    lm = data$lm$prt[[input$prt.level]]
    mset = subset_features(mset, lm$pvalue <= input$prt.cutoff)
    
    if (input$prt.collapse) {
        cbind(
            t(mset$conc_table),
            mset$sample_table[,c("Subject", "Timepoint", "Responding")]
        ) %>%
            melt(c("Subject", "Timepoint", "Responding")) %>%
            dcast(Subject + variable + Responding ~ Timepoint) %>%
            mutate(value = Post - Pre) %>%
            dcast(Subject + Responding ~ variable, value.var = "value") ->
            mdata
        conc_table = mdata[, -2] %>% column_to_rownames("Subject") %>% 
            t %>% conc_table
        sample_table = mdata[, 1:2]
        rownames(sample_table) = sample_table$Subject
        sample_table = sample_table(sample_table)
        
        mset = ProteomicsSet(conc_table, sample_table)
    }
    
    abs_scale = function(x){
        max = max(x, na.rm = T)
        min = min(x, na.rm = T)
        ((x - min) / (max- min) - 1/2) * 2
    }
    
    mylog = function(x){
        sign(x) * log(abs(x) + 1)
    }
    
    scale_fun = switch(
        input$prt.scale,
        "log" = mylog,
        "z-score" = scale,
        "log z-score" = function(x) scale(mylog(x)),
        "abs-scale" = abs_scale,
        "log abs-scale" = function(x) abs_scale(mylog(x))
    )
    
    mat = t(apply(mset$conc_table, 1, scale_fun))
    colnames(mat) = sampleNames(mset)
    
    list(
        mat = mat,
        metadata = as(mset$sample_table[,c("Subject", "Responding")], "data.frame")
    )
})

output$prt_heatmap = renderPlotly({
    mat = prtDataForHeatMap()$mat
    metadata = prtDataForHeatMap()$metadata
    
    feature_names = rownames(mat)
    rownames(mat) = make.unique(str_trunc(feature_names, 30))
    heatmaply(mat, col_side_colors = metadata$Responding, height = input$prt.hm_ht)
})


output$prt_pca = renderPlotly({
    mat = prtDataForHeatMap()$mat
    metadata = prtDataForHeatMap()$metadata
    
    pca = prcomp(t(mat))
    df = cbind(
        as.data.frame(pca$x[,1:2]),
        metadata
    )
    
    sdevs = pca$sdev / sum(pca$sdev)
    
    ggplot(df, aes(PC1, PC2, Subject = Subject)) +
        geom_hline(yintercept = 0, linetype = "dashed", size = 0.3, color = "darkgrey") +
        geom_vline(xintercept = 0, linetype = "dashed", size = 0.3, color = "darkgrey") +
        geom_point(aes(color = Responding)) +
        labs(x = glue("PC1 [{round(sdevs[1]*100, 1)} %]"), 
             y = glue("PC2 [{round(sdevs[3]*100, 1)} %]")) +
        theme_bw()
})