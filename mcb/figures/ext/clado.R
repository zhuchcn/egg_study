pkgs = c('dplyr','stringr','reshape2','tibble', 'phyloseq', 'Metabase', 
         'ggsci', "ggplot2", "ggrepel", "grid", "gridExtra", "cowplot")
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}
setwd(dirname(parent.frame(2)$ofile))
load("../apps/app/data/data.rda")
source("styles.R")
## -------- cladogram ----------------------------------------------------------
mcb = data$data$mcb$proportion
otu = mcb$otu
design = model.matrix(
    data = as(otu$sample_table, "data.frame"),
    ~ Timepoint*Treatment + Subject + 1
)
otu = otu %>%
    Metabase::as_phyloseq() %>%
    phylox::fix_duplicate_tax()

spy = phylox::summarizeFromPhyloseq(otu)
spy_stats = phylox::spy_to_limma(spy, design, coef = 13, p.value = 13)

tr = microbiomeViz::parsePhyloseq(otu,use_abundance = F, 
                                  node.size.scale = 1)
p = microbiomeViz::tree.backbone(tr, size = 1)
anno.data = phylox::create_annodata(spy_stats, coef = "pvalue", 
                                    colors = pal_lancet()(2))
anno.data = anno.data[-27,]
p.clado = microbiomeViz::clade.anno(p, anno.data) +
    geom_point(
        data=data.frame(x=1:2,  y=1:2, color=factor(c("Med","FF"), levels = c("Med", "FF"))),
        aes(x=x, y=y, color=color), size=0, stroke=0) +
    guides(
        color = guide_legend(
            override.aes = list(size=3, color= c("#00468BFF", "#ED0000FF"))
        )) +
    theme(
        legend.text = element_text(size = title.size - 2),
        plot.margin = margin(l=0, r=160, t=0, b=0),
        panel.background = element_rect(color = NA, fill = "transparent"),
        plot.background = element_rect(color = NA, fill = "transparent")
    )
## -------- save ---------------------------------------------------------------
ggsave(p.clado, file = 'fig2-mcb-clado.png',
       height = 7, width = 10, dpi = 300, units = "in")