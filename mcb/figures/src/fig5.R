setwd(dirname(parent.frame(2)$ofile))
source("global.R")

pkgs = c("space")
for(pkg in pkgs){
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

mcb = data$mcb$percent$data$genus
bga = data$bga$data
# bac = data$data$bac$raw
# cli = data$data$cli
bga = subset_samples(bga, sampleNames(mcb))

myscale = function(X) apply(t(X$conc_table), 2, function(x) scale(x))
Data = do.call(cbind, lapply(list(mcb, bga), myscale))

# (function(params){
#     sapply(params$l, function(lambda){
#         spn = space.joint(Data, lambda, iter = 2)  
#         as.numeric(spn$ParCor)
#     }) %>% as.data.frame %>%
#         data.table::setnames(as.character(params$l)) %>%
#         melt() %>%
#         ggplot(aes(x = variable, y = value)) +
#         geom_boxplot()
# })(params = list(l = c(0.1)))

spn = space.joint(Data, 0.1, iter = 2)
spn_cor = spn$ParCor
colnames(spn_cor) = colnames(Data)
rownames(spn_cor) = colnames(Data)
for(i in seq_len(nrow(spn_cor))){
    spn_cor[seq_len(i),i] = 0
}

# library(qgraph); library(igraph)
# networkTunning = function(mat, coef_cutoff, min_corr, layout){
#     ids = apply(mat, 2, function(x) {sum(!between(x, -coef_cutoff, coef_cutoff)) > min_corr})
#     mat = mat[ids, ids]
#     #groups = groups[ids]
#     mat[between(mat, -coef_cutoff, coef_cutoff)] = 0
#     qg = qgraph(mat, DoNotPlot=TRUE)
#     if (is.function(layout)) {
#         ig = as.igraph(qg)
#         layout = layout(ig)
#     }
#     qgraph(qg, layout = layout, DoNotPlot = FALSE)
# }
# networkTunning(mat = spn_cor, coef_cutoff = 0.2,
#                min_corr = 0, layout = layout_with_mds)

id = apply(spn_cor, 2, function(x) {sum(!between(x, -0.2, 0.2)) > 0})
mat = spn_cor[id, id]
groups = c(rep("mcb", nfeatures(mcb)), rep("bga", nfeatures(bga)))[id]
mat[between(mat, -0.2, 0.2)] = 0

node_list = data.frame(
    node = colnames(mat),
    group = groups
)
mat2 = mat
edge_list = melt(mat) %>% filter(value != 0) %>% data.table::setnames(c("x1", "x2", "value"))

network = data.frame(
    x1 = edge_list$x1,
    interaction = ifelse(edge_list$value > 0, "positive", "negative"),
    x2 = edge_list$x2
)
edge_list$shared_name = apply(network, 1, function(x) glue::glue("{x[1]} ({x[2]}) {x[3]}"))
edge_list$value = abs(edge_list$value)
write.csv(node_list, "../ext/node.csv")
write.csv(edge_list, "../ext/edge.csv")
write.table(network, "../ext/network.sif", sep = "\t", quote = FALSE, row.names = FALSE, col.names = FALSE)