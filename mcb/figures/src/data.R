# This script organize the microbiome, biogenic amine data, and their linear 
# model result (perform linear modeling if not done), and then save into a 
# nested list named data, finally save into ../data/data.rda for scripts to use
# to generate figures. The structure of the data is below:
# 
# ../data/data.rda
# +-- data: list
#     +-- bga
#     |   +-- data: mSet
#     |   +-- lm: data.frame
#     +-- mcb
#         +-- count: list
#         |   +-- data: list
#         |   |   +-- kingdom: mSet
#         |   |   +-- phylum: mSet
#         |   |   +-- ...
#         |   |   +-- otu: mSet
#         |   +-- lm: list
#         |   |   +-- kingdom: data.frame
#         |   |   +-- phylum: data.frame
#         |   |   +-- ...
#         |   |   +-- otu: data.frame
#         +-- percent: list
#         |   +-- data: list
#         |   |   +-- kingdom: mSet
#         |   |   +-- phylum: mSet
#         |   |   +-- ...
#         |   |   +-- otu: mSet
#         |   +-- lm: list
#         |   |   +-- kingdom: data.frame
#         |   |   +-- phylum: data.frame
#         |   |   +-- ...
#         |   |   +-- otu: data.frame
#         +-- tree: phylo

library(Metabase)
library(phylox)
library(phyloseq)
library(ape)

# Set working directory
setwd(dirname(parent.frame(2)$ofile))

## -------- MCB COUNT DATA LIST(mcb_ct) ----------------------------------------
load("../../../data/mcb.rda")

mcb = Metabase::subset_features(mcb, rowSums(mcb$conc_table !=0 ) > 1)

tree = keep.tip(tree, featureNames(mcb))

# create a copy of mcb, adjust it by adding each otu 1 count, in order for DESeq2
mcb_adj = mcb
mcb_adj$conc_table = mcb_adj$conc_table + 1

# convert MicrobiomeSet to SumamrizedPhyloseq
mcb = summarizeFromPhyloseq(Metabase::as_phyloseq(mcb))
mcb_adj = summarizeFromPhyloseq(Metabase::as_phyloseq(mcb_adj))

# MCB COUNT LINEAR MODEL with DESEQ2 (mcb_ct_lm)
design = model.matrix(data = as(mcb$sam_data, "data.frame"),
                      ~Treatment * Timepoint + Subject)
mcb_spy = spy_to_deseq2(
    mcb_adj, design, resultsName = "Treatmentegg:TimepointPost"
)

mcb = as_MicrobiomeSetList(mcb)

# convert the summarizedPhyloStats to list
lm = lapply(slotNames(mcb_spy), function(slotName){
    slot(mcb_spy,slotName)
})
names(lm) = gsub("_table", "", slotNames(mcb_spy))

count = list(
    data = mcb, 
    lm = lm
)


## -------- MCB PERCENT DATA LIST(mcb_pct) -------------------------------------
precalc = new.env()
load("../../Rdata/mcb_precalc.rda", envir = precalc)

## -------- bga ----------------------------------------------------------------
bga_precalc= new.env()
load("../../Rdata/bga_precalc.rda", envir = bga_precalc)
sampleNames(bga_precalc$bga) = gsub("-", "", sampleNames(bga_precalc$bga))
sampleNames(bga_precalc$bga) = gsub("Egg", "EGG", sampleNames(bga_precalc$bga))

## -------- data object constructor --------------------------------------------
data = list(
    bga = list(
        data = bga_precalc$bga,
        lm = bga_precalc$limma
    ),
    mcb = list(
        count = count,
        percent = list(
            data = precalc$mcb,
            lm = precalc$lm
        ),
        tree = tree
    )
)
save(data, file = "../data/data.rda")

