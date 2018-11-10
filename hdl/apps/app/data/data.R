lpd = new.env()
load("../../../Rdata/lpd_precalc.Rdata", envir = lpd)
imb = new.env()
load("../../../Rdata/imb_precalc.Rdata", envir = imb)
fct = new.env()
load("../../../Rdata/fct_precalc.Rdata", envir = fct)
cli = new.env()
load("../../../../diet/Rdata/clinical_precalc.Rdata", envir = cli)
diet = new.env()
load("../../../../diet/Rdata/diet_precalc.Rdata", envir = diet)

data = list(
    data = list(
        lpd  = lpd$lipidome_set,
        imb  = imb$ion_morbility,
        fct  = fct$hdl_function,
        cli  = cli$clinical,
        diet = diet$diet
    ),
    limma = list(
        lpd  = lpd$limma_list,
        imb  = imb$limma_result,
        fct  = fct$limma_result,
        cli  = cli$limma_result,
        diet = diet$limma_result
    ),
    corr = list(
        lpd = list(
            fct = lpd$corr_fct,
            imb = lpd$corr_imb,
            cli = lpd$corr_clinical
        ),
        imb = list(
            fct = imb$corr_fct,
            cli = imb$corr_clinical,
            diet = imb$corr_diet
        ),
        fct = list(
            fct = fct$corr_fct,
            cli = fct$corr_clinical
        )
    )
)

save(data, file = "data.rda")