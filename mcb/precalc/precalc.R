setwd(dirname(parent.frame(2)$ofile))

pkgs=c("dplyr", "reshape2", "tibble", "Metabase", "limma", "MatCorR")
for(pkg in pkgs){
    suppressPackageStartupMessages(library(pkg, character.only=TRUE))
}

load("../../data/mcb.rda")
load("../../data/diet.Rdata")

mcb = list(
    count = mcb,
    precent = mcb %>%
        transform_by_sample(function(x) {x = x + 0.5; x/sum(x, na.rm = TRUE)}) %>%
        as_phyloseq() %>%
        phylox::summarizeFromPhyloseq() %>%
        phylox::as_MicrobiomeSetList()
)

cli = MultxSet(
    conc_table   = clinical$conc_table,
    sample_table = clinical$sample_table
)

diet = MultxSet(
    conc_table = diet$conc_table,
    sample_table = diet$sample_table
)

# linear model
make_design = function(mset){
    model.matrix(
        ~ Treatment * Timepoint + Subject,
        data = as(mset$sample_table, "data.frame")
    )
}
lm = list(
    mcb = lapply(mcb$precent, function(mset){
        mSet_limma(
            mset, 
            make_design(mset), 
            transform = function(x){log(x + 0.5)}, 
            coef = "Treatmentegg:TimepointPost"
        )
    }),
    bga = mSet_limma(
        bga,
        make_design(bga),
        transform = log,
        coef = "Treatmentegg:TimepointPost"
    ),
    bac = mSet_limma(
        bac,
        make_design(bac),
        transform = log,
        coef = "Treatmentegg:TimepointPost"
    )
)

data = list(
    data = list(
        mcb = mcb,
        bga = bga,
        bac = bac,
        cli = cli,
        diet = diet,
        tree = tree
    ),
    lm = lm
)
saveRDS(data, file = "../Rdata/precalc.rds")
