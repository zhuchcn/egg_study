## -------------------- load packages -----------------------
pkgs = c('plyr', 'dplyr','stringr','reshape2','tibble',"Metabase")
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}

## --------------------- load data -------------------------
load("../../data/diet.Rdata")
pdata = as(diet$sample_table, "data.frame")

diet_norm = subset_features(diet, 3:13)
edata = sapply(1:nfeatures(diet_norm), function(i){
    if(grepl("Fat",featureNames(diet_norm)[i])) {
        diet_norm$conc_table[i,] * 9 / diet$conc_table["Cals (kcal)",]
    } else if (featureNames(diet_norm)[i] %in% c("Prot (g)", "Carb (g)", "Disacc (g)")) {
        diet_norm$conc_table[i,] * 4 / diet$conc_table["Cals (kcal)",]
    } else {
        diet_norm$conc_table[i,] / diet$conc_table["Cals (kcal)",]
    }
}) %>% t 
rownames(edata) = featureNames(diet_norm)
diet_norm$conc_table = conc_table(edata)
featureNames(diet_norm) = c('Prot (kcal/kcal)', 
                            'Carb (kcal/kcal)', 
                            'Fib (g/kcal)', 
                            'Fat (kcal/kcal)', 
                            'SatFat (kcal/kcal)', 
                            'MonoFat (kcal/kcal)', 
                            'PolyFat (kcal/kcal)', 
                            'TransFat (kcal/kcal)', 
                            'Chol (mg/kcal)', 
                            'Alcohol (g/kcal)', 
                            'Disacc (kcal/kcal)')

diet = list(
    raw = diet,
    normalized = diet_norm
)

## -------------------- linear model ----------------------
design = model.matrix(data = pdata, ~Treatment * Timepoint + Subject + 1)
limma_result = lapply(diet, function(mset){
    mSet_limma(mset, design, coef = 23, p.value = 23)
})

## --------------------- save ----------------------------
save(diet, limma_result,
     file = "../Rdata/diet_precalc.Rdata")