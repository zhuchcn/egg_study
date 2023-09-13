#' This script is used to generate post-treatment TMAO and Choline values for a meta-analysis conducted
#' by Nessmah Sultan et.al. from Monash University

# Metabase can be installed using `remotes::install_github('zhuchcn/Metabase')`
library(Metabase)
library(dplyr)

setwd(usethis::proj_path())
MCB_RDA_PATH = 'data/mcb.rda'
OUTPUT_PATH = 'data/tmao_choline_post_treatment.tsv'

METABOLITE_ISTD_MAP = list(
    'Choline' = list(
        'spike' = 'D9-TMAO',
        'analysis' = '1_D9-Choline iSTD'
    ),
    'TMAO' = list(
        'spike' = 'D9-TMAO',
        'analysis' = '1_D9-TMAO iSTD'
    )
)
UNIT_MAP = list(
    'mg/L' = 'ng/mL',
    'umol/L' = 'pmol/mL'
)

#' Load the biogenic amines data from the microbiome-metabolome dataset.
#' @returns a MetabolomicSet object.
load_bga = function() {
    data = environment()
    load(MCB_RDA_PATH, envir = data)
    return(data$bga)
}

#' Internal standard calibration of a given list of metabolites.
#' @param mset MetabolomicSet. The biogenic amines dataset.
#' @param metabolites Character. List of metabolites to calibrate.
#' @param unit Character. Either mg/L or umol/L
#' @returns a matrix of abundance of the specified metabolites.
calibrate_bga = function(mset, metabolites, unit) {
    stopifnot(unit %in% names(UNIT_MAP))
    stopifnot(all(metabolites %in% names(METABOLITE_ISTD_MAP)))
    
    res = lapply(metabolites, function(m) {
        istd_spike = METABOLITE_ISTD_MAP[[m]][['spike']]
        istd_analysis = METABOLITE_ISTD_MAP[[m]][['analysis']]
        x = conc_table(mset)[m,]
        xi = sample_table(mset)[,istd_analysis]
        spike_data = experiment_data(mset)$istd_spiked
        res = experiment_data(mset)$istd_calibrate(x, xi, istd_spike, spike_data, UNIT_MAP[[unit]])
        return(res$value)
    }) %>%
        do.call(cbind, .)
    colnames(res) = metabolites
    return(res)
}

main = function() {
    bga = load_bga()
    abund_table = calibrate_bga(bga, c('Choline', 'TMAO'), 'mg/L')
    abund_table = as.data.frame(abund_table)
    abund_table = cbind(sample_table(bga)[,c('Subject', 'Treatment', 'Timepoint')], abund_table)
    abund_table = filter(abund_table, Timepoint == 'Post')
    write.table(abund_table, OUTPUT_PATH, quote = FALSE, col.names = TRUE, row.names = FALSE)
}

if (!interactive()) {
    main()
}
