gghist = function(data, x){
    ggplot(data) +
        geom_histogram(aes_string(x), breaks = seq(0,1,0.025),
                       color = "white") +
        geom_vline(xintercept = 0.05, color = "red", linetype = "dashed",
                   size = 1) +
        theme_bw()
}

output$lpd_hist_pval = renderPlotly({
    p = gghist(lpd_limma_table(), "pvalue")
    ggplotly(p)
})

output$lpd_hist_padj = renderPlotly({
    p = gghist(lpd_limma_table(), "padj")
    ggplotly(p)
})

output$lpd_volcano = renderPlotly({
    p = lpd_limma_table() %>%
        rownames_to_column("Feature") %>%
        mutate(`P < 0.05` = pvalue < 0.05) %>%
        ggplot(aes(x = logFC, y = baseMean, 
                   Feature = Feature, pvalue = pvalue,
                   padj = padj)) +
        geom_point(aes(color = `P < 0.05`)) +
        scale_color_lancet() +
        theme_bw()
    ggplotly(p)
})