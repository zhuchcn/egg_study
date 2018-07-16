## ---------------------- load packages -------------------------
library(dplyr);library(reshape2);library(stringr)
library(tibble);library(data.table);library(readxl); 
library(Metabase)

##%######################################################%##
#                                                          #
####                  Part1. Lipidome                   ####
#                                                          #
##%######################################################%##
rm(list = ls())
setwd("~/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Egg Study/Result/Analysis")
file = "raw_data/lipidome/mx 348391_Zhu_CSH-QTOF MSMS_lipidomics_isolated HDL particles_11-2017_submit.xlsx"
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
standards = read.csv("raw_data/lipidome/wcmc_lipidomics_standards.csv")
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
    path = "raw_data/clinical_data/1-LSK Master Egg Data FR merged w Clinical & IM-122917.xlsx",
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
##%######################################################%##
#                                                          #
####                Part2. Ion Morbility                ####
#                                                          #
##%######################################################%##
file = "raw_data/ion_morbility/X3549 Final IM Data Egg Study w order.xls"
ep_data = read_excel(
    path = file,
    sheet = "X3549 Final IM Data", range = "A1:V81", col_names = T
)
fdata = read_excel(
    path = file,
    sheet = "Variables", range = "A3:E19"
) %>% as.data.frame
pdata = ep_data[,c(1:4,21,22)] %>% as.data.frame
edata = ep_data[,c(5:20)] %>% as.data.frame %>% t 

fdata$Variable = gsub("_IM","",fdata$Variable)
fdata = column_to_rownames(fdata,"Variable")
rownames(edata) = rownames(fdata)

pdata$Timepoint = design$Timepoint
pdata$Treatment = design$Treatment
pdata$Subject = design$Subject
rownames(pdata) = design$sample_id
colnames(edata) = rownames(pdata)

ion_morbility = MultiSet(
    conc_table = conc_table(edata),
    sample_table = sample_table(pdata),
    feature_data = feature_data(fdata),
    experiment_data = MultiExperimentData(experiment_type = "Ion Morbility")
)

## -------------------------------------------------------
## ------------- C H R O M A T O G R A M -----------------
## -------------------------------------------------------



##%######################################################%##
#                                                          #
####               Part 3. HDL Functions                ####
#                                                          #
##%######################################################%##


# Cholesterol Efflux
file = "raw_data/function_data/20180213 Egg Study Cholesterol Efflux Assay (ApoB Depleted Serum 1%).xlsx"
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
file = "raw_data/function_data/Egg_data_STA260_051418.csv"
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
file = "raw_data/function_data/Egg_data_CDassay_070918.csv"
fct_data_3 = read.csv(file = file, stringsAsFactors = FALSE) %>%
    mutate(sample_id = paste0("Egg", samplename)) %>%
    column_to_rownames("sample_id")

# hdl proteins lowry method, by Jody
file = "raw_data/function_data/Egg HDL protein concentration- Lowry method 06292017.xlsx"
protein = read_excel(
    path = file, sheet = "Study samples", range = "A1:C81"
) %>%
    tidyr::fill(column=Subject, .direction = "down") %>%
    mutate(sample_id = paste0("Egg", Subject, `Time point`)) %>%
    as.data.frame %>%
    column_to_rownames("sample_id")

# order it
fct_data_2 = fct_data_2[rownames(fct_data),]
fct_data_3 = fct_data_3[rownames(fct_data),]
fct_data = cbind(fct_data, fct_data_2, fct_data_3[,"change_ox"])
colnames(fct_data)[7] = "conjugated_diene"
fct_data$hdl_protein = protein[rownames(fct_data), 3]

hdl_function = MultiSet(
    conc_table = conc_table(t(fct_data)),
    sample_table = sample_table(column_to_rownames(design, "sample_id")),
    experiment_data = MultiExperimentData(experiment_type = "HDL Functions")
)

##%######################################################%##
#                                                          #
####                        Save                        ####
#                                                          #
##%######################################################%##

path = "data/hdl.Rdata"
save(lipidome, ion_morbility, hdl_function,
     file = path)
