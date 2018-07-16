## -------------------- load packages -----------------------
pkgs = c('dplyr','stringr','reshape2','tibble',"Metabase")
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}

## --------------------- load data -------------------------
rm(list=ls())
setwd("/Users/chenghaozhu/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Egg Study/Result/Analysis/data")
load("hdl.Rdata")

## --------------------- limma ----------------------------
design = model.matrix(
    data = as(sample_table(ion_morbility), "data.frame"), 
    ~ Treatment * Timepoint + Subject + 1
)
limma_result = mSet_limma(ion_morbility, design, coef = 23, p.value = 23)

## -------------------- save ----------------------------
setwd("/Users/chenghaozhu/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Egg Study/Result/Analysis/hdl/Rdata")
save(ion_morbility,limma_result, file="imb_precalc.Rdata")
