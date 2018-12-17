setwd(dirname(parent.frame(2)$ofile))

load("../../../Rdata/prt_precalc.Rdata")

data = list(
    data = list(
        prt = proteome,
        lpd = lipidome,
        fct = hdl_function
    ),
    lm = list(
        prt = limma_list,
        lpd = limma_lpd,
        fct = limma_fct
    ),
    corr = list(
        prt = list(
            lpd = corr_lpd,
            fct = corr_fct
        )
    )
)

save(data, file = "data.rda")