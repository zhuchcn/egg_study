## -------------------- load packages -----------------------
pkgs = c('dplyr','stringr','reshape2','tibble',"limma", 'Metabase', 'MatCorR')
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}

## --------------------- load data -------------------------
setwd("/Users/chenghaozhu/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Egg Study/Result/Analysis/analysis/data")
load("hdl.Rdata"); load("diet.Rdata")

lpd_conc = lipidome
lpd_prop = transform_by_sample(lipidome, function(x) x/sum(x))
lpd_prt  = transform_by_feature(lipidome, function(x)
    x/conc_table(hdl_function)["hdl_protein",])
lpd_hdl_apoa1 = transform_by_feature(lipidome, function(x)
    x/hdl_function$conc_table["ApoA1-HDL",])
lpd_ttl_apoa1 = transform_by_feature(lipidome, function(x)
    x/hdl_function$conc_table["ApoA1-HDL",] * clinical$conc_table["Apo A1 mg/dl",])

lpd_class_conc = summarize_features(lpd_conc, feature_var = "class")
lpd_class_prop = summarize_features(lpd_prop, feature_var = "class")
lpd_class_prt  = transform_by_feature(lpd_class_conc, function(x) 
    x/conc_table(hdl_function)["hdl_protein",])
lpd_class_hdl_apoa1 = transform_by_feature(lpd_class_conc, function(x) 
    x/conc_table(hdl_function)["ApoA1-HDL",])
lpd_class_ttl_apoa1 = transform_by_feature(lpd_class_conc, function(x)
    x/hdl_function$conc_table["ApoA1-HDL",] * clinical$conc_table["Apo A1 mg/dl",])


lipidome_set = list(
    class = list(
        Concentration = lpd_class_conc,
        Proportion    = lpd_class_prop,
        Adj_protein   = lpd_class_prt,
        Adj_hdl_apoa1 = lpd_class_hdl_apoa1,
        Adj_ttl_apoa1 = lpd_class_ttl_apoa1
    ),
    feature = list(
        Concentration = lpd_conc,
        Proportion    = lpd_prop,
        Adj_protein   = lpd_prt,
        Adj_hdl_apoa1 = lpd_hdl_apoa1,
        Adj_ttl_apoa1 = lpd_ttl_apoa1
    )
)

## ----------------------- limma ---------------------------
# limma modeling
design = model.matrix(data=as(sample_table(lipidome), "data.frame"), 
                      ~Treatment * Timepoint + Subject + 1)

limma_list = lapply(lipidome_set, function(sublist){
    lapply(sublist, function(data) 
        mSet_limma(data, design, coef = 23, p.value = 23))
})

## --------------------- correlation ----------------------
design2 = model.matrix(data=as(sample_table(lipidome), "data.frame"), 
                       ~Subject + 1)
corr_func = function(covar){
    message(substitute(covar))
    lapply(lipidome_set, function(sublist){
        lapply(sublist, function(lipidome){
            message("I'm working...")
            MatCorPack(X = conc_table(covar), Y = conc_table(lipidome), 
                       design = design2)
        })
    })
}
corr_fct = corr_func(hdl_function)
corr_imb = corr_func(ion_morbility)
corr_diet = corr_func(diet)
corr_clinical = corr_func(clinical)

## ---------------------- save ----------------------------
setwd("/Users/chenghaozhu/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Egg Study/Result/Analysis/analysis/hdl/Rdata")
save(lipidome_set, limma_list, corr_fct, corr_imb, corr_diet, corr_clinical,
     hdl_function, ion_morbility, diet, clinical,
     file="lpd_precalc.Rdata")
