## ------------------- loading librarys ------------------------
pkgs = c('plyr', 'dplyr','stringr','reshape2','tibble',
         'limma','data.table')
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}
rm(list=ls())

## ----------------------- load data -----------------------------
setwd('/Users/chenghaozhu/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Egg Study/Result/Analysis/mcb')
load('../data/mcb.Rdata')

edata_list = Biogenic_Amines$edata_list
pdata = Biogenic_Amines$pdata
fdata = Biogenic_Amines$fdata

rownames(edata_list$raw) = fdata$Annotation

## ---------------------- limma ------------------------------
design = model.matrix(data=pdata, ~TX + Day + TX*Day + Subj + 1)
limma_lmFit = function(data, design){
    fit = lmFit(data, design)
    fit_ebayes = eBayes(fit)
    fit_top = topTable(fit_ebayes, coef=13, number=nrow(data), sort.by = "none",
                       p.value = 13)
}
limma_list = lapply(edata_list, function(data) limma_lmFit(data, design))

## ---------------------- save -------------------------
save(edata_list, limma_list, fdata, pdata,
     file = "Rdata/bga_precalc.Rdata")
