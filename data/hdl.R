## ---------------------- load packages -------------------------
library(dplyr);library(reshape2);library(stringr);library(readr)
library(tibble);library(data.table);library(readxl); 
library(Metabase)

################################################################################
##########                      L I P I D O M E                       ##########
################################################################################
setwd(dirname(parent.frame(2)$ofile))
file = "../raw_data/lipidome/mx 348391_Zhu_CSH-QTOF MSMS_lipidomics_isolated HDL particles_11-2017_submit.xlsx"
lipidome = import_wcmc_excel(
    file            = file,
    sheet           = "submit",
    conc_range      = "I8:CS3382",
    sample_range    = "H1:CS7",
    feature_range   = "A7:H3382",
    InChIKey        = "InChI Key",
    experiment_type = "lipidomics"
)
lipidome = subset_features(lipidome, !is.na(feature_data(lipidome)$InChIKey))
lipidome = collapse_QC(lipidome, qc_names = paste0("Biorec00",1:9))
lipidome = transform_by_feature(lipidome, function(x){
    ifelse(is.na(x), min(x,na.rm = TRUE)/2, x)
})
standards = read.csv("../raw_data/lipidome/wcmc_lipidomics_standards.csv")
feature_data(lipidome)$ESI = ifelse(
    grepl("\\+$", feature_data(lipidome)$Species), "pos", "neg"
)
feature_data(lipidome)$class = assign_lipid_class(
    feature_data(lipidome)$Annotation)
lipidome = subset_features(lipidome, feature_data(lipidome)$class != "AC")
experiment_data(lipidome)$sample_volumn_ul = 20
experiment_data(lipidome)$internal_standards = standards
lipidome = calibrate_lipidomics_wcmc(
    lipidome, cid = "InChIKey", class = "class", ESI = "ESI"
)
lipidome = filter_by_cv(lipidome, cv = "qc_cv", cid = "InChIKey")
feature_data(lipidome)$Annotation = lipid_name_formater(feature_data(lipidome)$Annotation)
feature_data(lipidome)$Annotation = make.unique(feature_data(lipidome)$Annotation, sep = " ")
featureNames(lipidome) = feature_data(lipidome)$Annotation

design = read_excel(
    path = "../raw_data/clinical_data/1-LSK Master Egg Data FR merged w Clinical & IM-122917.xlsx",
    sheet = "Master",
    range = "A1:C81"
) %>% as.data.frame %>%
    mutate(Timepoint = ifelse(grepl("^pre-", Treatment), "pre", "post"),
           Treatment = gsub("^pre-", "", Treatment)) %>%
    mutate(Treatment = gsub("white", "sub", Treatment)) %>%
    mutate(sample_id = paste("Egg",gsub("(\\d{3})", "\\1",Subject), sep = ""),
           Subject = gsub("[ABCD]{1}$", "", Subject)) %>%
    mutate(Subject = factor(Subject),
           Treatment = factor(Treatment, levels = c("sub","egg")),
           Timepoint = factor(Timepoint, levels = c("pre","post")))
sample_table(lipidome)$Treatment = design$Treatment
sample_table(lipidome)$Timepoint = design$Timepoint
sample_table(lipidome)$tx_order = design$`TX Order`
sample_table(lipidome)$Subject = design$Subject
sampleNames(lipidome) = gsub("-","",sampleNames(lipidome))
################################################################################
##########                 I O N   M O R B I L I T Y                  ##########
################################################################################
file = "../raw_data/ion_morbility/1-Corrected IM categories.9.5.18.xlsx"
ep_data = read_excel(
    path = file,
    sheet = 1, range = "A1:U81", col_names = T
)
# fdata = read_excel(
#     path = file,
#     sheet = "Variables", range = "A3:E19"
# ) %>% as.data.frame
pdata = ep_data[,c(1:3,5)] %>% 
    as.data.frame %>%
    setNames(c("Subject", "Visit", "Treatment", "Timepoint")) %>%
    mutate(Timepoint = ifelse(grepl("^pre-", Timepoint), "Pre", "Post")) %>%
    mutate(Subject = factor(Subject),
           Treatment = factor(Treatment, levels = c("white", "egg")),
           Timepoint = factor(Timepoint, levels = c("Pre", "Post"))) %>%
    mutate(sample_id = str_c("Egg", Subject, Visit)) %>% 
    column_to_rownames("sample_id")
edata = ep_data[,c(7:21)] %>% as.data.frame %>% t
colnames(edata) = rownames(pdata)

# fdata$Variable = gsub("_IM","",fdata$Variable)
# fdata = column_to_rownames(fdata,"Variable")
# rownames(edata) = rownames(fdata)

# pdata$Timepoint = design$Timepoint
# pdata$Treatment = design$Treatment
# pdata$Subject = design$Subject
# rownames(pdata) = design$sample_id
# colnames(edata) = rownames(pdata)

ion_morbility = MultxSet(
    conc_table = conc_table(edata),
    sample_table = sample_table(pdata),
    experiment_data = MultiExperimentData(experiment_type = "Ion Morbility")
)

## -------------------------------------------------------
## ------------- C H R O M A T O G R A M -----------------
## -------------------------------------------------------



################################################################################
##########                  H D L   F U N C T I O N                   ##########
################################################################################
# Cholesterol Efflux
file = "../raw_data/function_data/20180213 Egg Study Cholesterol Efflux Assay (ApoB Depleted Serum 1%).xlsx"
fct_data = read_excel(
    path = file,
    sheet = "Results", range = "A1:B80", col_names = F
) %>% as.data.frame() %>%
    setnames(c("SampleID","Chol Efflux (ApoB Depleted Serum 1%)")) %>%
    mutate(
        SampleID = gsub("(\\d{3})(\\w{1})","Egg\\1\\2", SampleID)
    ) %>%
    arrange(SampleID) %>% 
    column_to_rownames("SampleID")

# CETP, LCAT, PON1, SAA, oxLDL, and CRP
file = "../raw_data/function_data/Egg_data_STA260_051418.csv"
fct_data_2 = read.csv(
    file=file,
    stringsAsFactors = F
)[,c("samplename","cetp","lcat", "pon1", "saa", "oxldl")] %>%
    mutate(
        samplename = gsub("(\\d{3})([ABCD]{1})", "Egg\\1\\2",
                          samplename, perl=T)
    ) %>%
    column_to_rownames("samplename")

# conjugated diene
file = "../raw_data/function_data/Egg_data_CDassay_070918.csv"
fct_data_3 = read.csv(file = file, stringsAsFactors = FALSE) %>%
    mutate(sample_id = paste0("Egg", samplename)) %>%
    column_to_rownames("sample_id")

# hdl proteins lowry method, by Jody
file = "../raw_data/function_data/Egg HDL protein concentration- Lowry method 06292017.xlsx"
protein = read_excel(
    path = file, sheet = "Study samples", range = "A1:C81"
) %>%
    tidyr::fill(column=Subject, .direction = "down") %>%
    mutate(sample_id = paste0("Egg", Subject, `Time point`)) %>%
    as.data.frame %>%
    column_to_rownames("sample_id")

# HDL apo A1 
file = "../raw_data/clinical_data/1-LSK Egg Study Clincal & Diet Data w_ApoA1-HDL.6.27.2018.xlsx"
apoa1 = read_excel(path = file, sheet = "Sheet1", range = "A1:R81") %>%
    select(c("Study ID", "Visit", "ApoA1-HDL")) %>%
    mutate(sample_id = paste0("Egg", `Study ID`, `Visit`)) %>%
    as.data.frame %>%
    column_to_rownames("sample_id")
    
# order it
fct_data_2 = fct_data_2[rownames(fct_data),]
fct_data_3 = fct_data_3[rownames(fct_data),]
fct_data = cbind(fct_data, fct_data_2, fct_data_3[,"change_ox"])
colnames(fct_data)[7] = "conjugated_diene"
fct_data$hdl_protein = protein[rownames(fct_data), 3]
fct_data$`ApoA1-HDL` = apoa1[rownames(fct_data), "ApoA1-HDL"]

hdl_function = MultxSet(
    conc_table = conc_table(t(fct_data)),
    sample_table = sample_table(column_to_rownames(design, "sample_id")),
    experiment_data = MultiExperimentData(experiment_type = "HDL Functions")
)

################################################################################
##########                      P R O T E O M E                       ##########
################################################################################
## -------- read data ----------------------------------------------------------
file = "../raw_data/proteome/proteinGroups.txt"
data = read_tsv(file)
data = data[!is.na(data$`Protein names`),] %>%
    filter(!grepl("Keratin", `Fasta headers`))
data = as.data.frame(data)
rownames(data) = paste0(
    "Feat",
    str_pad(1:nrow(data), pad = "0", width = 4)
)
## -------- split data ---------------------------------------------------------
fdata = data[c('Protein IDs', 
               'Majority protein IDs', 
               'Peptide counts (all)', 
               'Peptide counts (razor+unique)', 
               'Peptide counts (unique)', 
               'Protein names', 
               'Gene names', 
               'Fasta headers', 
               'Unique sequence coverage [%]',
               'Mol. weight [kDa]', 
               'Sequence length', 
               'Sequence lengths', 
               'Q-value', 
               'Score')]
iBAQ = data[grepl("^iBAQ [03]_Lisa_\\d{3}[ABCDc]$", colnames(data))]
intensity = data[grepl("^Intensity [03]_Lisa_\\d{3}[ABCDc]$", colnames(data))]
LFQ = data[grepl("^LFQ intensity [03]_Lisa_\\d{3}[ABCDc]$", colnames(data))]
## -------- clean feature data -------------------------------------------------
fdata$accession = str_split_fixed(fdata$`Fasta headers`, " ", n=2)[,1]
fdata$protein = str_split_fixed(
    str_split_fixed(fdata$`Fasta headers`, "OS=", n=2)[,1],
    " ", n=2)[,2]
fdata$species = gsub("^.+OS=(.+?) [A-Z]{2}=.+$", "\\1", fdata$`Fasta headers`)
## -------- make sample data ---------------------------------------------------
sample_id = toupper(gsub(".+Lisa_(\\d{3}[ABCDc])","\\1", colnames(intensity)))
pdata = data.frame(
    row.names = paste0("Egg", sample_id),
    Subject = str_sub(sample_id, 1,3),
    Timepoint = ifelse(grepl("\\d{3}[AC]", sample_id), "Pre", "Post") %>%
        factor(levels = c("Pre", "Post")),
    Responding = c("Non-responder", "Responder")[c(1,1,2,2,2,2,1,1,1,1,2,2,1,1,1,1,2,2,2,2)]
)

## -------- mSet ---------------------------------------------------------------
colnames(iBAQ) = rownames(pdata)
iBAQ = ProteomicsSet(
    conc_table = conc_table(as.matrix(iBAQ)),
    sample_table = sample_table(pdata),
    feature_data = feature_data(fdata)
)

colnames(LFQ) = rownames(pdata)
LFQ = ProteomicsSet(
    conc_table = conc_table(as.matrix(LFQ)),
    sample_table = sample_table(pdata),
    feature_data = feature_data(fdata)
)

colnames(intensity) = rownames(pdata)
intensity = ProteomicsSet(
    conc_table = conc_table(as.matrix(intensity)),
    sample_table = sample_table(pdata),
    feature_data = feature_data(fdata)
)

iBAQ = subset_samples(iBAQ, !iBAQ$sample_table$Subject %in% c(115, 119))
LFQ = subset_samples(LFQ, !LFQ$sample_table$Subject %in% c(115, 119))
intensity = subset_samples(intensity, !intensity$sample_table$Subject %in% c(115, 119))

## -------- Filter by Q value --------------------------------------------------
intensity = subset_features(intensity, intensity$feature_data$`Q-value` <= 0.001)
LFQ = subset_features(LFQ, LFQ$feature_data$`Q-value` <= 0.001)
iBAQ = subset_features(iBAQ, iBAQ$feature_data$`Q-value` <= 0.001)

## -------- Filter by Score ----------------------------------------------------
intensity = subset_features(intensity, intensity$feature_data$Score >= 322)
LFQ = subset_features(LFQ, featureNames(intensity))
iBAQ = subset_features(iBAQ, featureNames(intensity))

intensity = subset_features(
    intensity, apply(intensity$conc_table, 1, function(x) sum(x == 0) == 0))
iBAQ = subset_features(iBAQ, featureNames(intensity))
LFQ = subset_features(LFQ, featureNames(intensity))

intensity$sample_table$Subject = factor(intensity$sample_table$Subject)
iBAQ$sample_table$Subject = factor(iBAQ$sample_table$Subject)
LFQ$sample_table$Subject = factor(LFQ$sample_table$Subject)

featureNames(intensity) = intensity$feature_data$protein
featureNames(iBAQ) = iBAQ$feature_data$protein
featureNames(LFQ) = LFQ$feature_data$protein

proteome = list(
    iBAQ = iBAQ, 
    LFQ = LFQ, 
    intensity = intensity)

################################################################################
##########                          S A V E                           ##########
################################################################################

path = "hdl.Rdata"
save(lipidome, ion_morbility, hdl_function, proteome,
     file = path)
