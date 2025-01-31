## -------------------- load packages -----------------------
pkgs = c('dplyr','stringr','reshape2','tibble',"Metabase", "MatCorR")
for(pkg in pkgs){
    library(pkg, character.only=TRUE)
}
setwd(dirname(parent.frame(2)$ofile))
## --------------------- load data -------------------------
load("../../data/hdl.Rdata");  
load("../../data/diet.Rdata")
load("../../diet/Rdata/diet_precalc.Rdata")

## --------------------- limma ----------------------------
design = model.matrix(
    data = as(sample_table(ion_morbility), "data.frame"), 
    ~ Treatment * Timepoint + Subject + 1
)
limma_result = mSet_limma(ion_morbility, design, coef = 23, p.value = 23)

## -------- corr ---------------------------------------------------------------
methods = c("pearson", "spearman", "kendall", "lm")
design2 = model.matrix(data = as(sample_table(lipidome), "data.frame"), 
                       ~Subject + 1)

corr_fct = MatCorPack(X=conc_table(hdl_function), Y=conc_table(ion_morbility),
                      methods = methods, design = design2)
corr_clinical = MatCorPack(X=conc_table(clinical), Y=conc_table(ion_morbility),
                           methods = methods, design = design2)
corr_diet = lapply(diet, function(li){
    MatCorPack(X=conc_table(li), Y=conc_table(ion_morbility),
               methods = methods, design = design2)
})

## -------------------- save ----------------------------
save(ion_morbility,limma_result, corr_fct, corr_clinical, corr_diet,
     hdl_function, clinical, diet,
     file="../Rdata/imb_precalc.Rdata")
