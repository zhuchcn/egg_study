pkgs = c("dplyr", "stringr", "reshape2", "tibble", "phyloseq", "phylox", 
         "Metabase", "DESeq2")
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}

load("../../data/mcb.rda")

## remove subjects with only one treatment not the other
rm_2sub = mcb$sample_table %>% group_by(Subject) %>% summarize(Nsample = length(Subject))
rm_2subID = rm_2sub[rm_2sub$Nsample ==2,]$Subject

## remvoe the subject with extreme low total count
# colSums(mcb$conc_table)
# Sample 111C only has 7 reads

mcb = Metabase::subset_samples(mcb, ! mcb$sample_table$Subject %in% c(rm_2subID, 111) )

mcb = as_phyloseq(mcb)
mcb = phylox::fix_duplicate_tax(mcb)
mcb_count = summarizeFromPhyloseq(mcb) %>%
    as_MicrobiomeSetList()
mcb_prop = transform_sample_counts(mcb, function(x) x/sum(x, na.rm = TRUE)) %>%
    summarizeFromPhyloseq() %>%
    as_MicrobiomeSetList()

mcb = list(
    count = mcb_count,
    proportion = mcb_prop
)

mcb = lapply(
    mcb, function(li){
        lapply(li, function(mset) 
            subset_features(mset, Metabase::featureNames(mset) != "NA"))
    }
)

design = model.matrix(data = as(mcb$count$kingdom$sample_table, "data.frame"), 
                      ~Treatment*Timepoint + Subject)
ds = lapply(mcb$count, function(x) {
    mSet_deseq(x, design, 22)
    cat("test")
})
lm = lapply(mcb$proportion, function(x) mSet_limma(x, design, coef = 13, p.value = 13))
lm = list(
    deseq2 = ds,
    limma = lm
)