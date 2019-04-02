setwd(dirname(parent.frame(2)$ofile))
source("global.R")

pkgs = c("mixOmics", "zheatmap", "RColorBrewer")
for(pkg in pkgs){
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

mcb = data$mcb$percent$data$genus
bga = subset_samples(data$bga$data, sampleNames(mcb))
featureNames(bga) = gsub("7.alpha.-Hydroxy-3-oxo-4-cholestenoic acid", "7-HOCA", featureNames(bga))
X = t(mcb$conc_table)
Y = t(bga$conc_table)
X = apply(X, 2, function(x) scale(x, center = TRUE, scale = TRUE))
Y = apply(Y, 2, function(x) scale(x, center = TRUE, scale = TRUE))

fit_rcc = rcc(X, Y, ncomp = 3, method = "shrinkage")

# plotLoadingFromRCCA = function(fit, mat, cutoff){
#     (
#         data.frame(
#             loading = fit$loadings[[mat]][,1],
#             var = rownames(fit$loadings[[mat]])
#         ) %>%
#             mutate(index = 1:length(loading)) %>%
#             ggplot(aes(x = index, y = loading, var = var)) +
#             geom_point(aes(color = !between(loading, cutoff[1], cutoff[2]))) +
#             geom_hline(yintercept = cutoff[1], color = "salmon", 
#                        linetype = "dashed") +
#             geom_hline(yintercept = cutoff[2], color = "salmon", 
#                        linetype = "dashed") +
#             scale_color_manual(values = c("grey18", "steelblue")) +
#             labs(title = glue::glue("Comp1 loading values for {mat}")) +
#             theme(legend.position = "none")
#     ) 
# }
# 
# plotLoadingFromRCCA(fit_rcc, "X", c(-0.079,0.0805))
# plotLoadingFromRCCA(fit_rcc, "Y", c(-0.095,0.0975))

p = zheatmap(
    t(cor(
        X[,!between(fit_rcc$loadings$X[,1], -0.079,0.0805)], 
        Y[,!between(fit_rcc$loadings$Y[,1], -0.095,0.0975)]
    )),
    colors = rev(brewer.pal(11, name = "RdBu")), 
    xtext = TRUE, xtext.angle = 45, xtext.vjust = 1,
    print = FALSE
)

ggsave("../png/fig4.png", p, width = 8, height = 7, units = "in", dpi = 300)
