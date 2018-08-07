## -------------------- load packages -----------------------
pkgs = c('dplyr','stringr','reshape2','tibble',"limma", 'Metabase', 'MatCorR')
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}

## --------------------- load data -------------------------
setwd("/Users/chenghaozhu/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Egg Study/Result/Analysis/analysis/data")
load("hdl.Rdata"); load("diet.Rdata")
## -------- get molecular weight -----------------------------------------------
data("wcmc_adduct")
molwt = as.numeric(rep(NA, nfeatures(lipidome)))
for(i in 1:nfeatures(lipidome)){
    species = str_split(lipidome$feature_data$Species[i], "\\_")[[1]]
    species = gsub("\\[", "", species)
    species = gsub("\\]", "", species)
    species = gsub("\\+$", "", species)
    species = gsub("\\-$", "", species)
    species = species[species %in% rownames(wcmc_adduct)]
    species = species[which.min(1 * wcmc_adduct[species,]$Mult + wcmc_adduct[species,]$Mass)]
    if(length(species) == 0) next
    mz = as.numeric(str_split(lipidome$feature_data$`m/z`[i], "\\_")[[1]])
    mz = mz[which.min(mz)]
    molwt[i] = mz2molwt(species, mz)
}
lipidome$feature_data$molwt = molwt
## -------- summarize ----------------------------------------------------------
lpd_conc = subset_features(lipidome, lipidome$feature_data$class != "FA")
lpd_prop = transform_by_sample(lpd_conc, function(x) x/sum(x))
lpd_prt  = transform_by_feature(lpd_conc, function(x)
    x/conc_table(hdl_function)["hdl_protein",])

lpd_class_conc = summarize_features(lpd_conc, feature_var = "class")
lpd_class_prop = summarize_features(lpd_prop, feature_var = "class")
lpd_class_prt  = transform_by_feature(lpd_class_conc, function(x) 
    x/conc_table(hdl_function)["hdl_protein",])

lpd_mol = transform_by_sample(lpd_conc, function(x) x/lpd_conc$feature_data$molwt)
lpd_eod = summarize_EOD(lpd_mol, name = "Annotation", class = "class")
lpd_acl = summarize_ACL(lpd_mol, name = "Annotation", class = "class")
lpd_odd = summarize_odd_chain(lpd_mol, name = "Annotation", class = "class")
lpd_ratio = summarize_lipid_ratios(lpd_mol, name = "Annotation", class = "class")

lipidome_set = list(
    class = list(
        Concentration = lpd_class_conc,
        Proportion    = lpd_class_prop,
        Adj_protein   = lpd_class_prt
    ),
    feature = list(
        Concentration = lpd_conc,
        Proportion    = lpd_prop,
        Adj_protein   = lpd_prt
    ),
    summarize = list(
        EOD = lpd_eod,
        ACL = lpd_acl,
        "Odd Chain" = lpd_odd,
        "Lipid Ratios" = lpd_ratio
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
