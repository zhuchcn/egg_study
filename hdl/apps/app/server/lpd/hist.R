gghist = function(data, x){
    ggplot(data) +
        geom_histogram(aes_string(x), breaks = seq(0,1,0.025),
                       color = "white") +
        geom_vline(xintercept = 0.05, color = "red", linetype = "dashed",
                   size = 1) +
        theme_bw()
}

output$lpd_hist_pval = renderPlotly({
    p = gghist(lpd_limma_table(), "P.Value")
    ggplotly(p)
})

output$lpd_hist_padj = renderPlotly({
    p = gghist(lpd_limma_table(), "adj.P.Val")
    ggplotly(p)
})

output$lpd_volcano = renderPlotly({
    p = lpd_limma_table() %>%
        rownames_to_column("Feature") %>%
        mutate(`P < 0.05` = P.Value < 0.05) %>%
        ggplot(aes(x = logFC, y = AveExpr, 
                   Feature = Feature, P.Value = P.Value,
                   adj.P.Val = adj.P.Val)) +
        geom_point(aes(color = `P < 0.05`)) +
        scale_color_lancet() +
        theme_bw()
    ggplotly(p)
})