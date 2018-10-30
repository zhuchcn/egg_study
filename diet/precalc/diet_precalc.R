## -------------------- load packages -----------------------
pkgs = c('plyr', 'dplyr','stringr','reshape2','tibble',"Metabase")
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}

## --------------------- load data -------------------------
load("../../data/diet.Rdata")
pdata = as(diet$sample_table, "data.frame")

## -------------------- linear model ----------------------
design = model.matrix(data = pdata, ~Treatment * Timepoint + Subject + 1)
limma_result = mSet_limma(diet, design, coef = 23, p.value = 23)

## --------------------- save ----------------------------
save(diet, limma_result,
     file = "../Rdata/diet_precalc.Rdata")