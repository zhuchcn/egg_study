## -------- load packages ------------------------------------------------------
pkgs = c('dplyr','stringr','reshape2','tibble', "readr","limma", 'Metabase',
         'MatCorR')
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}

## -------- load data ----------------------------------------------------------
setwd(dirname(parent.frame(2)$ofile))
load("../../data/hdl.Rdata")

## -------- limma --------------------------------------------------------------
design = model.matrix(
    data = as(proteome$intensity$sample_table, "data.frame"),
    ~Responding * Timepoint + Subject + 1
)

limma_list = lapply(proteome, function(mset){
    mSet_limma(mset, design, coef = 11, p.value = 11)
})

## -------- correlation --------------------------------------------------------
load_lpd = function(){
    load("../Rdata/lpd_precalc.Rdata")
    return(lipidome_set)
}
lipidome = load_lpd()

lipidome = lapply(lipidome, function(li) {
    lapply(li, function(mset){
        mset = subset_samples(mset, sampleNames(proteome$iBAQ))
        mset$sample_table$Responding = proteome$iBAQ$sample_table$Responding
        return(mset)
    })
})

corr_lpd = lapply(proteome, function(prt){
    lapply(lipidome, function(lpd_li){
        lapply(lpd_li, function(lpd){
            message("Calculating the next item...")
            MatCorPack(X = conc_table(lpd), Y = conc_table(prt), 
                       methods = c("pearson", "spearman", "kendall"))
        })
    })
})

hdl_function = subset_samples(hdl_function, sampleNames(proteome$iBAQ))
hdl_function$sample_table$Responding = proteome$iBAQ$sample_table$Responding

corr_fct = lapply(proteome, function(prt) {
    message("HDL Function")
    message("Calculating the next item")
    MatCorPack(X = conc_table(hdl_function), Y = conc_table(prt),
               methods = c("pearson", "spearman", "kendall"))
})

## -------- more limma ---------------------------------------------------------
limma_lpd = lapply(lipidome, function(li) {
    lapply(li, function(mset){
        mSet_limma(mset, design, coef = 11, p.value = 11)
    })
})
limma_fct = mSet_limma(hdl_function, design, coef = 11, p.value = 11)

## -------- save ---------------------------------------------------------------
save(proteome, lipidome, hdl_function, limma_list, corr_lpd, corr_fct, limma_lpd, limma_fct,
     file = "../Rdata/prt_precalc.Rdata")
