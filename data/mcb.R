pkgs = c("dplyr", "stringr", "reshape2", "tibble", "data.table", "readxl", 
         "tidyr", "Metabase")
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}
setwd(dirname(parent.frame(2)$ofile))

################################################################################
##########                    M I C R O B I O M E                     ##########
################################################################################
otu_table = read.table('../16s_processing/dada2/feature_table.tsv', 
                       sep = '\t', header=T, stringsAsFactor=F, row.names = 1)
tax_table = read.table('../16s_processing/dada2/taxonomy.tsv', 
                       sep='\t', header=T, stringsAsFactor=F, row.names = 1)
sample_data = read.table('../16s_processing/dada2/sample_metadata.tsv', 
                         sep='\t', header=T, stringsAsFactors = F, row.names = 1)
rownames(otu_table) = str_c(
    "MCB", str_pad(rownames(otu_table), width = 4, pad = "0")
)
otu_table = otu_table[,rownames(sample_data)]
rownames(tax_table) = str_c(
    "MCB", str_pad(rownames(tax_table), width = 4, pad = "0")
)

otu_table = otu_table %>% as.matrix %>% conc_table()
sample_data$Timepoint = factor(sample_data$Timepoint, 
                               levels = c("Pre", "Post"))
sample_data = sample_data %>% 
    mutate(Treatment = ifelse(Treatment == "Egg", "egg", "sub")) %>%
    mutate(Treatment = factor(Treatment, levels = c("sub", "egg")))

colnames(otu_table) = paste0("Egg",sample_data$SubjectID)
rownames(sample_data) = paste0("Egg",sample_data$SubjectID)

sample_data$Subject = gsub("^EG", "", sample_data$StudyID)
sample_data = sample_table(sample_data)
tax_table = feature_data(tax_table)
mcb = MicrobiomeSet(otu_table, sample_data, tax_table)

sampleNames(mcb) = str_c("EGG",mcb$sample_table$SubjectID)

save(mcb, file = "mcb.rda")
