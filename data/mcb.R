## -------- load packages ------------------------------------------------------
pkgs = c("dplyr", "stringr", "reshape2", "tibble", "data.table", "readxl", 
         "tidyr", "Metabase", "ape")
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}
setwd(dirname(parent.frame(2)$ofile))

################################################################################
##########                    M I C R O B I O M E                     ##########
################################################################################

otu_table = read.table('../raw_data/microbiome/feature_table.tsv', 
                       sep = '\t', header=T, stringsAsFactor=F, row.names = 1)
tax_table = read.table('../raw_data/microbiome/taxonomy.tsv', 
                       sep='\t', header=T, stringsAsFactor=F, row.names = 1)
sample_data = read.table('../raw_data/microbiome/sample_metadata.tsv', 
                         sep='\t', header=T, stringsAsFactors = F, row.names = 1)

otu_table = otu_table[,rownames(sample_data)]

otu_table = otu_table %>% as.matrix %>% conc_table()
sample_data$Timepoint = factor(sample_data$Timepoint, 
                               levels = c("Pre", "Post"))
sample_data = sample_data %>% 
    mutate(Treatment = ifelse(Treatment == "Egg", "egg", "sub")) %>%
    mutate(Treatment = factor(Treatment, levels = c("sub", "egg")))

colnames(otu_table) = paste0("Egg", sample_data$SubjectID)
rownames(sample_data) = paste0("Egg",sample_data$SubjectID)

sample_data$Subject = gsub("^EG", "", sample_data$StudyID)
sample_data = sample_table(sample_data)
tax_table = feature_data(tax_table)
mcb = MicrobiomeSet(otu_table, sample_data, tax_table)

mcb = subset_samples(mcb, ! mcb$sample_table$Subject %in% c(111, 118, 120))
mcb = subset_features(mcb, apply(mcb$conc_table, 1, function(x) sum(x != 0) != 0))

tree = read.tree(file = "../raw_data/microbiome/tree.nwk")
tree$tip.label = paste0("MCB",tree$tip.label)

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

## -------- read in interal standard result data separatly ---------------------
istd_data = read_excel(
    file, sheet = "Submit", range = "I8:CJ24", 
    col_names = sampleNames(bga)
) %>%
    as.data.frame %>%
    cbind(read_excel(file, sheet = "Submit", range = "B7:B24")) %>%
    column_to_rownames("Annotation") %>%
    t %>% as.data.frame

bga$sample_table = sample_table(cbind(bga$sample_table, istd_data))

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

bga$sample_table = sample_table(cbind(
    bga$sample_table[,colnames(bga$sample_table) != "Treatment"], 
    metadata
))
bga$feature_data$Annotation <- as.character(bga$feature_data$Annotation)
bga$feature_data$Annotation[bga$feature_data$InChIKey == "DKZBBWMURDFHNE-NSCUHMNNSA-N"] = "4-Hydroxy-3-methoxycinnamaldehyde"
bga$feature_data$Annotation[bga$feature_data$InChIKey == "IYRMWMYZSQPJKC-UHFFFAOYSA-N"] = "Kaempferol"
bga$feature_data$Annotation[bga$feature_data$InChIKey == "ZFXYFBGIUFBOJW-UHFFFAOYSA-N"] = "Theophylline"

featureNames(bga) = bga$feature_data$Annotation
sampleNames(bga) = gsub("-", "", sampleNames(bga))

# read in the internal standard data
file = "../raw_data/biogenic_amines/HILIC_ISTD_mix_2016-11-15 (1).xlsx"
spiked = read_excel(file,sheet = "SOP", range = "I2:J26") %>% 
    as.data.frame %>%
    cbind(read_excel(file, sheet = "SOP", range = "A2:A26")) %>%
    column_to_rownames("ISTD short name") %>%
    `colnames<-`(c("ng/mL", "pmol/mL"))

bga$experiment_data = list(
    istd_spiked = spiked,
    istd_calibrate = function(x, xi, istd, spiked, unit){
        res = spiked[istd, unit] * (x/xi) * (100 / 110) * (413/20) * 10^-3
        if(unit == "ng/mL"){
            unit = "mg/L"
        } else {
            unit = "umol/L"
        }
        return(list(value = res, unit = unit))
    }
)

# tmao = spiked["D9-TMAO", "pmol/mL"] *
#     (bga$conc_table["TMAO",] / bga$sample_table[,"1_D9-TMAO iSTD"])  *
#     (100 / 110) * (413 /20) * 10^-3

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
sampleNames(bac) = gsub("\\d{2,3}-\\d{1,2}-Egg-(\\d{3})-([A-D]{1})", "Egg\\1\\2", sampleNames(bac))
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
save(mcb, bga, bac, tree, file = "mcb.rda")
