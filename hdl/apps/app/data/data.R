setwd("~/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Egg Study/Result/Analysis/analysis/hdl/apps/app/data")
lpd = new.env()
load("../../../Rdata/lpd_precalc.Rdata", envir = lpd)
imb = new.env()
load("../../../Rdata/imb_precalc.Rdata", envir = imb)
fct = new.env()
load("../../../Rdata/fct_precalc.Rdata", envir = fct)

data = list(
    data = list(
        lpd = lpd$lipidome_set,
        imb = imb$ion_morbility,
        fct = fct$hdl_function,
        cli = lpd$clinical
    ),
    limma = list(
        lpd = lpd$limma_list,
        imb = imb$limma_result,
        fct = fct$limma_result
    ),
    corr = list(
        lpd = list(
            fct = lpd$corr_fct,
            imb = lpd$corr_imb,
            cli = lpd$corr_clinical
        ),
        imb = list(
            fct = imb$corr_fct,
            cli = imb$corr_clinical
        ),
        fct = list(
            fct = fct$corr_fct,
            cli = fct$corr_clinical
        )
    )
)

save(data, file = "data.rda")