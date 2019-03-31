library(DESeq2)
library(Metabase)
library(phylox)
library(phyloseq)

# MCB COUNT DATA LIST(mcb_ct)
load("../../../data/mcb.rda")

mcb = Metabase::subset_features(mcb, rowSums(mcb$conc_table !=0 ) > 1)
mcb$conc_table = mcb$conc_table + 1

ps = Metabase::as_phyloseq(mcb)
# summarized phyloseq, count
mcb_spy_ct = summarizeFromPhyloseq(ps)

# MCB LINEAR MODEL LIST(mcb_ct_lm)
design = model.matrix(data = as(mcb$sample_table, "data.frame"),
                      ~Treatment * Timepoint + Subject)
mcb_spys_ct = spy_to_deseq2(
    mcb_spy_ct, design, resultsName = "Treatmentegg:TimepointPost"
)

mcb_ct = as_MicrobiomeSetList(mcb_spy_ct)

mcb_ct_lm = lapply(slotNames(mcb_spys_ct), function(slotName){
    slot(mcb_spys_ct,slotName)
})
level_name = gsub("_table", "", slotNames(mcb_spys_ct))
names(mcb_ct_lm) = level_name

mcb_ct_data = list(mcb_ct, mcb_ct_lm)

# MCB PERCENT DATA LIST(mcb_pct)
precalc = new.env()
load("../../Rdata/mcb_precalc.rda", envir = precalc)

mcb_pct = precalc$mcb
mcb_pct_lm = precalc$lm

mcb_pct_data = list(mcb_pct, mcb_pct_lm)

mcb_data = list(mcb_ct_data, mcb_pct_data)
