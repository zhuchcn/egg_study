setwd(dirname(parent.frame(2)$ofile))

################################################################################
##########               B I O G E N I C   A M I N E S                ##########
################################################################################

## -------- load packages ------------------------------------------------------
pkgs = c("dplyr", "reshape2", "tibble","stringr","Metabase")
for(pkg in pkgs){
    library(pkg, verbose = FALSE, warn.conflicts = FALSE,
            character.only = TRUE, quietly = TRUE)
}

## -------- read data ----------------------------------------------------------
library(Metabase)
file = "../raw_data/biogenic_amines/mx 349859_Zhu_HILIC-QTOF MSMS_11-2017_submit.xlsx"
bga = import_wcmc_excel(
    file = file,
    sheet = "Submit",
    conc_range = "I8:CS1296",
    sample_range = "H1:CS7",
    feature_range = "A7:H1296",
    InChIKey = "InChI Key"
)
## -------- quality control samples summarized into mean, sd, and cv ----------------------------------------------------------
bga = collapse_QC(bga, qc_names = paste0("Biorec00", 1:9))
## -------- remove features----------------------------------------------------------
bga = subset_features(bga, !is.na(feature_data(bga)$InChIKey))
bga = subset_features(bga, !grepl("iSTD$", feature_data(bga)$Annotation))
## -------- remove NAs as required and fill in new values ------------------------------------------------------
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

bga$sample_table = sample_table(cbind(bga$sample_table[,colnames(bga$sample_table) != "Treatment"], 
                                      metadata))
bga$feature_data$Annotation <- as.character(bga$feature_data$Annotation)
bga$feature_data$Annotation[bga$feature_data$InChIKey == "DKZBBWMURDFHNE-NSCUHMNNSA-N"] = "4-Hydroxy-3-methoxycinnamaldehyde"
bga$feature_data$Annotation[bga$feature_data$InChIKey == "IYRMWMYZSQPJKC-UHFFFAOYSA-N"] = "Kaempferol"
bga$feature_data$Annotation[bga$feature_data$InChIKey == "ZFXYFBGIUFBOJW-UHFFFAOYSA-N"] = "Theophylline"

################################################################################
##########                          S A V E                           ##########
################################################################################
save(bga, file = "mcb.Rdata")