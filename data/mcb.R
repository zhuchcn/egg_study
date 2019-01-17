## -------- load packages ------------------------------------------------------
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

################################################################################
##########               B I O G E N I C   A M I N E S                ##########
################################################################################

## -------- read data ----------------------------------------------------------

file = "../raw_data/biogenic_amines/mx 349859_Zhu_HILIC-QTOF MSMS_11-2017_submit.xlsx"
bga = import_wcmc_excel(
    file = file,
    sheet = "Submit",
    conc_range = "I8:CS1296",
    sample_range = "H1:CS7",
    feature_range = "A7:H1296",
    InChIKey = "InChI Key"
)
## -------- quality control samples summarized into mean, sd, and cv -----------
bga = collapse_QC(bga, qc_names = paste0("Biorec00", 1:9))
## -------- remove features-----------------------------------------------------
bga = subset_features(bga, !is.na(feature_data(bga)$InChIKey))
bga = subset_features(bga, !grepl("iSTD$", feature_data(bga)$Annotation))
## -------- remove NAs as required and fill in new values ----------------------
bga = subset_features(
    bga, apply(conc_table(bga), 1, function(x) sum(is.na(x)) < 21) )
bga = transform_by_feature(
    bga, function(x) ifelse(is.na(x), min(x, na.rm = TRUE)/2, x)
)

## -------- metadata -----------------------------------------------------------
metadata = read_excel(
    "../raw_data/clinical_data/1-LSK Egg Study Clincal & Diet Data w_ApoA1-HDL.6.27.2018.xlsx",
    sheet = 1, 
    range = "A1:F81"
) %>%
    as.data.frame %>%
    mutate(Timepoint = ifelse(grepl("pre", Treatment), "Pre", "Post"),
           Treatment = TX,
           Subject = `Study ID`) %>%
    select(-c(`Study ID`, `Subject Initials`, TX, `TX Code`)) %>%
    mutate(sample_id = str_c("Egg", Subject, Visit, sep = "-"),
           Treatment = factor(Treatment, levels = c("white", "egg")),
           Timepoint = factor(Timepoint, levels = c("Pre", "Post")),
           Subject = factor(Subject)) %>%
    column_to_rownames("sample_id")

metadata = metadata[sampleNames(bga),]

bga$sample_table = sample_table(
    cbind(bga$sample_table[,colnames(bga$sample_table) != "Treatment"], 
          metadata)
)
bga$feature_data$Annotation <- as.character(bga$feature_data$Annotation)
bga$feature_data$Annotation[bga$feature_data$InChIKey == "DKZBBWMURDFHNE-NSCUHMNNSA-N"] = "4-Hydroxy-3-methoxycinnamaldehyde"
bga$feature_data$Annotation[bga$feature_data$InChIKey == "IYRMWMYZSQPJKC-UHFFFAOYSA-N"] = "Kaempferol"
bga$feature_data$Annotation[bga$feature_data$InChIKey == "ZFXYFBGIUFBOJW-UHFFFAOYSA-N"] = "Theophylline"

featureNames(bga) = bga$feature_data$Annotation

################################################################################
##########                     B I L E   A C I D                      ##########
################################################################################

path = "../raw_data/bile_acids/mx 349941 Zhu_bile acids_human plasma_09-2018 submit.xlsx"
conc_range = "T9:CU31"
sample_range = "S2:CU6"
feature_range = "A8:S31"
bac = import_wcmc_excel(
    file = path, 
    sheet = "Final Submit", 
    conc_range = conc_range, 
    sample_range = sample_range, 
    feature_range = feature_range, 
    InChIKey = "InChIKey"
)
sampleNames(bac) = gsub("\\d{2,3}-\\d{1,2}-Egg-(\\d{3})-([A-D]{1})", "EGG\\1\\2", sampleNames(bac))
featureNames(bac) = bac$feature_data$`short key`
bac$sample_table$Subject = bga$sample_table$Subject
bac$sample_table$Timepoint = bga$sample_table$Timepoint
bac$sample_table$Treatment = bga$sample_table$Treatment

## remove features with more than 10 observations smaller than the LOQ
# bac = subset_features(
#     bac, 
#     sapply(1:nfeatures(bac), function(i)
#         sum(bac$conc_table[i,] <= bac$feature_data$`LOQ (nM)`[i]) <= 10)
# )
bac = subset_features(bac, !grepl('MCA$', featureNames(bac)))
bac = subset_features(
    bac, sapply(1:nfeatures(bac), function(i)
        sum(bac$conc_table[i,] <= bac$feature_data$`LOQ (nM)`[i]) < 80)
)

################################################################################
##########                          S A V E                           ##########
################################################################################
save(mcb, bga, bac, file = "mcb.rda")
