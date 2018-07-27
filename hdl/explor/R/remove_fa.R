pkgs = c("dplyr", "reshape2", "tibble", "stringr", "Metabase", "limma", "MatCorR")
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}
## -------- load data ----------------------------------------------------------
load("../../Rdata/lpd_precalc.Rdata")
## -------- constract data list ------------------------------------------------
lpd_fa_conc = subset_features(lipidome_set$feature$Concentration, 
                                lipidome_set$feature$Concentration$feature_data$class != "FA")
lpd_fa_prop = transform_by_sample(lpd_fa_conc, function(x) x/sum(x))
lpd_fa_prt = transform_by_feature(lpd_fa_conc, function(x) 
    x/hdl_function$conc_table["hdl_protein",])
lpd_fa_hdl_apoa1 = transform_by_feature(lpd_fa_conc, function(x)
    x/hdl_function$conc_table["ApoA1-HDL",])
lpd_fa_ttl_apoa1 = transform_by_feature(lpd_fa_conc, function(x)
    x/hdl_function$conc_table["ApoA1-HDL",] * clinical$conc_table["Apo A1 mg/dl",])

lpd_fa_class_conc = summarize_features(lpd_fa_conc, "class")
lpd_fa_class_prop = transform_by_sample(lpd_fa_class_conc, function(x) x/sum(x))
lpd_fa_class_prt = transform_by_feature(lpd_fa_class_conc, function(x)
    x/hdl_function$conc_table["hdl_protein",])
lpd_fa_class_hdl_apoa1 = transform_by_feature(lpd_fa_class_conc, function(x)
    x/hdl_function$conc_table["ApoA1-HDL",])
lpd_fa_class_ttl_apoa1 = transform_by_feature(lpd_fa_class_conc, function(x)
    x/hdl_function$conc_table["ApoA1-HDL",] * clinical$conc_table["Apo A1 mg/dl",])

lpd_li = list(
    "All Features" = lipidome_set,
    "FA Removed" = list(
        class = list(
            Concentration = lpd_fa_class_conc,
            Proportion    = lpd_fa_class_prop,
            Adj_protein   = lpd_fa_class_prt,
            Adj_hdl_apoa1 = lpd_fa_class_hdl_apoa1,
            Adj_ttl_apoa1 = lpd_fa_class_ttl_apoa1
        ),
        feature = list(
            Concentration = lpd_fa_conc,
            Proportion    = lpd_fa_prop,
            Adj_protein   = lpd_fa_prt,
            Adj_hdl_apoa1 = lpd_fa_hdl_apoa1,
            Adj_ttl_apoa1 = lpd_fa_ttl_apoa1
        )
    )
)
## -------- limma --------------------------------------------------------------
design = model.matrix(data=as(sample_table(hdl_function), "data.frame"), 
                      ~Treatment * Timepoint + Subject + 1)
limma_li = lapply(lpd_li, function(sublist){
    lapply(sublist, function(subsub){
        lapply(subsub, function(data)
            mSet_limma(data, design, coef = 23, p.value = 23))
    })
})
## -------- correlation --------------------------------------------------------
design2 = model.matrix(data=as(sample_table(hdl_function), "data.frame"), 
                       ~Subject + 1)
corr_func = function(covar){
    message(substitute(covar))
    lapply(lpd_li, function(sublist){
        lapply(sublist, function(subsub){
            lapply(subsub, function(lipidome){
                message("I'm working...")
                MatCorPack(X = conc_table(covar), Y = conc_table(lipidome), 
                           design = design2)
            })
        })
    })
}
corr_fct = corr_func(hdl_function)
corr_clinical = corr_func(clinical)
## -------- save ---------------------------------------------------------------
save(lpd_li, limma_li, corr_fct, corr_clinical, hdl_function, clinical,
     file = "../Rdata/precalc.Rdata")
