library(dplyr); library(reshape2); library(stringr); library(tibble);
library(data.table);library(readxl); library(Metabase)
setwd("~/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Egg Study/Result/Analysis/analysis")
##%######################################################%##
#                                                          #
####                        Diet                        ####
#                                                          #
##%######################################################%##
file = "raw_data/diet_data/Diet Data.7.3.18xlsx.xlsx"
diet_data = read_excel(
    path = file,
    sheet="Sheet1", range="A1:Q81"
) 
edata = diet_data[,6:ncol(diet_data)] %>% t
fdata = tibble(feature_id = rownames(edata)) %>%
    mutate(Nutrient = str_split(feature_id, " ", n = 2, simplify = T)[,1]) %>%
    mutate(Unit = gsub(".*\\((.*)\\).*", "\\1", feature_id)) %>%
    as.data.frame %>%
    column_to_rownames("feature_id")
pdata = diet_data[,1:5] %>%
    setnames(old = "Study ID", new = "Subject") %>%
    mutate(Timepoint = ifelse(grepl("^pre-", Treatment), "Pre", "Post")) %>%
    mutate(Treatment = gsub("white", "sub", TX)) %>%
    mutate(sample_id = paste0("Egg", Subject, Visit)) %>%
    mutate(
        Timepoint = factor(Timepoint, levels = c("Pre", "Post")),
        Treatment = factor(Treatment, levels = c("sub", "egg")),
        Subject = factor(Subject)
    ) %>%
    as.data.frame %>%
    column_to_rownames("sample_id")
colnames(edata) = rownames(pdata)
diet = MultxSet(
    conc_table = conc_table(edata),
    sample_table = sample_table(pdata),
    feature_data = feature_data(fdata),
    experiment_data = MultiExperimentData(experiment_type = "Dietary Nutrient")
)
##%######################################################%##
#                                                          #
####                  Clinical Values                   ####
#                                                          #
##%######################################################%##
file = "raw_data/clinical_data/1-LSK Egg Study Clincal & Diet Data w_ApoA1-HDL.6.27.2018.xlsx"
clinical_data = read_excel(path = file, sheet="Sheet1", range="A1:Z81")
pdata = clinical_data[,1:9] %>%
    mutate(
        Timepoint = ifelse(grepl("^pre-", Treatment), "Pre", "Post"),
        Treatment = gsub("white", "sub", TX),
        sample_id = paste0("Egg", `Study ID`, Visit)
    ) %>%
    as.data.frame %>%
    setnames(old = "Study ID", "Subject") %>%
    mutate(
        Timepoint = factor(Timepoint, level = c("Pre", "Post")),
        Treatment = factor(Treatment, level = c("sub", "egg")),
        Subject = factor(Subject)
    ) %>%
    column_to_rownames("sample_id") 
edata = clinical_data[,-(1:9)] %>% t
edata = edata[rownames(edata) != "ApoA1-HDL",]
colnames(edata) = rownames(pdata)

crp = read.csv(
    file="raw_data/function_data/Egg_data_STA260_051418.csv",
    stringsAsFactors = F
) %>%
    select(samplename, crp) %>%
    mutate(samplename = paste0("Egg", samplename)) %>%
    column_to_rownames("samplename")

crp = crp[rownames(pdata),]
edata = rbind(edata, crp)

clinical = MultxSet(
    conc_table = conc_table(edata),
    sample_table = sample_table(pdata),
    experiment_data = MultiExperimentData(experiment_type = "Clinical Values")
)

##%######################################################%##
#                                                          #
####                      S A V E                       ####
#                                                          #
##%######################################################%##
save(diet, clinical, file = "data/diet.Rdata")
