## -------------------- load packages -----------------------
pkgs = c('plyr', 'dplyr','stringr','reshape2','tibble',"Metabase")
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}

## --------------------- load data -------------------------
rm(list=ls())
setwd("~/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Egg Study/Result/Analysis/analysis/data")
load("diet.Rdata")
pdata = as(clinical$sample_table, "data.frame")

## -------------------- linear model ----------------------
design = model.matrix(data = pdata, ~Treatment * Timepoint + Subject + 1)
limma_result = mSet_limma(
    transform_by_sample(clinical, function(x) log2(x+1)), 
    design, coef = 23, p.value = 23
)

## --------------------- save ----------------------------
setwd("~/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Egg Study/Result/Analysis/analysis/diet/Rdata")
save(clinical, limma_result,
     file = "clinical_precalc.Rdata")