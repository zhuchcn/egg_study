## ------------------- loading librarys ------------------------
pkgs = c('dplyr','stringr','reshape2','tibble',
         'Metabase')
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}
load("../../data/mcb.Rdata")

## -------- statistic analysis -------------------------------------------------
design = model.matrix(
    data = as(bga$sample_table, "data.frame"),
    ~ Treatment * Timepoint + Subject + 1
)
limma = mSet_limma(bga, design, coef = 23, p.value = 23)

## -------- save ---------------------------------------------------------------
save(bga,limma, file = "../Rdata/bga_precalc.Rdata")