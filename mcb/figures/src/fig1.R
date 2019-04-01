setwd(dirname(parent.frame(n = 2)$ofile))
source("global.R")
load("../data/data.rda")

# Figure 1A
# alpha diversity boxplot before and after egg and white
library(Metabase)
library(phyloseq)
otu = as_phyloseq(data$mcb$count$data$otu)
alpha_div = estimate_richness(otu, measures = "Shannon")
alpha_div$Timepoint = sample_data(otu)$Timepoint
alpha_div$Treatment = sample_data(otu)$Treatment
alpha_div$Subject = sample_data(otu)$StudyID

p1A = ggplot(alpha_div, aes(x = Timepoint, y = Shannon))+
    geom_boxplot()+
    geom_point(aes(color = Subject), size = point.size)+
    geom_line(aes(group = Subject, color = Subject))+
    scale_color_manual(values = colorRampPalette(pal_npg()(10))(19))+
    facet_grid(cols = vars(Treatment))+
    labs(title = "Shannon Diversity")+
    theme_boxplot()+
    theme(legend.position = "none")
    
# Figure 1B
# beta diversity PCoA plot with weighted unifrac
ps = as_phyloseq(data$mcb$percent$data$otu)
ps_tree = merge_phyloseq(ps, tree)
ps_ord = ordinate(ps_tree, "PCoA", "wunifrac")
ps_df = data.frame(ps_ord$vectors[,1:2],
                   StudyID = sample_data(ps_tree)$StudyID,
                   Treatment = sample_data(ps_tree)$Treatment,
                   Timepoint = sample_data(ps_tree)$Timepoint) %>%
    mutate(StudyID = gsub("EG", "", StudyID))

p1B = ggplot(data = ps_df)+
    geom_point(aes(x = Axis.1, y = Axis.2, color = StudyID), size = point.size)+
    scale_color_manual(values = colorRampPalette(pal_npg()(10))(19)) +
    guides(color = guide_legend(nrow = 2))+
    labs(title = "Weighted Unifrac PCoA")+
    theme_scatter()

legend = cowplot::get_legend(p1B)
p1B = p1B + theme(legend.position = "none")
   
# Figure 1C
# stacked taxonomy bar plot at phylum level before and after egg and white
row_sums = data.frame(
    phylum = featureNames(data$mcb$percent$data$phylum),
    value = rowMeans(conc_table(data$mcb$percent$data$phylum))
) %>%
    arrange(desc(value)) %>%
    mutate(
        phylum = phylum %+% " [" %+% ifelse(
            value > 10^-4, round(value *100, 2), "<0.01"
        ) %+% "%]"
    ) %>%
    mutate(
        phylum = factor(phylum, levels = phylum)
    )

set.seed(19)

p1C = ggplot(row_sums, aes(x = "", y = value, fill = phylum))+
    geom_bar(width = 1, stat = "identity")+
    scale_fill_manual(
        values = colorRampPalette(pal_npg()(10))(12)[sample(1:12,12)]
    )+
    coord_polar("y")+
    guides(fill = guide_legend(title = ""))+
    my_theme()+
    theme(
        axis.line = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        legend.text = element_text(size = title.size - 8),
        legend.key.size = unit(0.4, "inches")
        
    )
p1C

p = plot_grid(
    plot_grid(
        NULL, p1C, p1A, p1B, labels = c("A", "B", "C", "D"), ncol = 2,
        label_size = title.size
    ),
    legend,
    nrow = 2,
    rel_heights = c(1, 0.1)
)

ggsave("../png/fig1.png", p, width = 18, height = 16, units = "in", dpi = 300)
