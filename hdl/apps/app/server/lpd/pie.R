output$lpd_pie_tbl = function(){
    lpd = data$data$lpd$class$Concentration
    lpd$conc_table %>%
        t %>% as.data.frame %>%
        mutate(
            Treatment = lpd$sample_table$Treatment,
            Timepoint = lpd$sample_table$Timepoint
        ) %>%
        melt(id.var = c("Treatment", "Timepoint")) %>%
        group_by(Treatment, Timepoint, variable) %>% 
        dplyr::summarize(value = str_c(round(mean(value)/10, 2), " (", 
                                       round(sd(value)/10, 2), ")")) %>%
        # need to fix the unit!!
        dcast(variable ~ Treatment + Timepoint) %>%
        kable(col.names=(c("Lipid Class","Pre (mg/dL)", "Post (mg/dL)", "Pre (mg/dL)", "Post (mg/dL)"))) %>%
        kable_styling(c("striped")) %>%
        add_header_above(c(" " = 1, "Yolk-Free" = 2, "Whole Egg" = 2))
}

output$lpd_pie = renderPlot({
    lpd = data$data$lpd$class$Proportion
    df = data.frame(
        class = featureNames(lpd),
        prop = rowMeans(lpd$conc_table),
        stringsAsFactors = F
    ) %>%
        mutate(class = str_c(class, ": ", round(prop*100,2), "%"))
    set.seed(36)
    color_pal = colorRampPalette(pal_npg()(10))(11)[sample(1:11)]
    ggplot(df,aes(x=1, y=prop, fill = class)) +
        geom_bar(stat = "identity", color = "white")+
        coord_polar("y") + 
        scale_fill_manual(values = color_pal) +
        theme_bw() +
        theme(axis.line = element_blank(),
              panel.border = element_blank(),
              axis.title = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              panel.grid = element_blank())
})