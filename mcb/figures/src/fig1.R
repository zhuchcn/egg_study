setwd(dirname(parent.frame(n = 2)$ofile))
source("global.R")

# Figure 1A
# alpha diversity boxplot before and after egg and white
library(Metabase)
library(phyloseq)
otu = as_phyloseq(mcb$otu)
estimate_richness(otu, measures = "Shannon")


# Figure 1B
# beta diversity PCoA plot with weighted unifrac

# Figure 1C
# stacked taxonomy bar plot at phylum level before and after egg and white