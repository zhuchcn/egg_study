## -------------------- load packages -----------------------
pkgs = c('plyr', 'dplyr','stringr','reshape2','tibble',"limma", "Metabase",
         "MatCorR")
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}
setwd(dirname(parent.frame(2)$oflie))
## --------------------- load data -------------------------
load("../../data/diet.Rdata"); load("../../data/hdl.Rdata")

## --------------------- limma ----------------------------
design = model.matrix(
    data = as(sample_table(hdl_function), "data.frame"),
    ~ Treatment * Timepoint + Subject + 1
)
limma_result = mSet_limma(hdl_function, design, coef = 23, p.value = 23)

## ----------------------- corr  --------------------------
methods = c("pearson", "spearman", "kendall", "lm")
design2 = model.matrix(data = as(sample_table(lipidome), "data.frame"), 
                       ~Subject + 1)

corr_fct = MatCorPack(X=conc_table(hdl_function), Y=conc_table(hdl_function),
                      methods = methods, design = design2)
corr_clinical = MatCorPack(X=conc_table(clinical), Y=conc_table(hdl_function),
                           methods = methods, design = design2)
corr_diet = MatCorPack(X=conc_table(diet), Y=conc_table(hdl_function),
                       methods = methods, design = design2)

## -------------------- save ----------------------------
setwd("../Rdata")
save(hdl_function,limma_result, corr_fct, clinical, corr_clinical, diet, 
     corr_diet, file="fct_precalc.Rdata")
