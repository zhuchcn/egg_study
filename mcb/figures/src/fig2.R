setwd(dirname(parent.frame(n = 2)$ofile))
source("global.R")
load("../data/data.rda")

# Figure 2
# Cladogram

library(microbiomeViz)
library(phylox)

otu = as_phyloseq(data$mcb$percent$data$otu)
args = data$mcb$percent$lm
names(args) = paste0(names(args), "_table")
args$Class = "SummarizedPhyloStats"
lm = do.call(new, args)
tr = parsePhyloseq(otu, use_abundance = F, node.size.scale = 1.25)
p = tree.backbone(tr, size=1)
anno.data = create_annodata(lm, coef = "pvalue", cutoff = 0.1)
p = clade.anno(p, anno.data)
p = p + geom_point(
    data=data.frame(x=1:2,  y=1:2, color=c("Yolk-free Substitute","Whole Egg")),
    aes(x=x, y=y, color=color), size=0, stroke=0) +
    guides(
        color = guide_legend(
            override.aes = list(size=3, color= rev(c("#ED0000FF", "#00468BFF")))
        )) +
    theme(
        legend.text = element_text(size = 11),
        plot.margin = margin(l=0, r=200, t=0, b=0)
    )

ggsave("../png/fig2.png", p, width = 8, height = 6, units = "in", dpi = 300)

