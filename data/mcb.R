## ---------------------- load packages -------------------------
library(dplyr);library(plyr);library(reshape2);library(stringr)
library(tibble);library(data.table);library(readxl)

## -------------------------------------------------------------
## -------------- B I O G E N I C   A M I N E S ----------------
## -------------------------------------------------------------
rm(list=ls())
setwd("/Users/chenghaozhu/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Egg Study/Result/Analysis/raw_data/biogenic_amines/")
ef_data = read_excel(
    path = "mx 349859_Zhu_HILIC-QTOF MSMS_11-2017_submit.xlsx",
    sheet="Submit", range = "A8:CS1296"
)
fdata = ef_data[,1:8] %>%
    setnames(
        as.character(as.data.frame(
            read_excel(
                path = "mx 349859_Zhu_HILIC-QTOF MSMS_11-2017_submit.xlsx",
                sheet="Submit", range="A7:H7", col_names = F
            )
        ))
    )%>% as.data.frame
edata = ef_data[,-(1:8)] %>%
    setnames(
        as.character(as.data.frame(
            read_excel(
                path = "mx 349859_Zhu_HILIC-QTOF MSMS_11-2017_submit.xlsx",
                sheet="Submit", range="I1:CS1", col_names = F
            )
        ))
    ) %>% as.data.frame
design = read_excel(
    path = "../clinical_data/1-LSK Master Egg Data FR merged w Clinical & IM-122917.xlsx",
    sheet="Master", range = "A1:C81"
) %>%
    as.data.frame %>%
    mutate(
        Subj = str_sub(Subject, 1, 3),
        Timepoint = str_sub(Subject,4,5),
        TX = gsub("pre-","", Treatment),
        Day = ifelse(grepl("pre-",Treatment), "Pre", "Post"),
        SampleID = str_c("Egg",Subj,Timepoint, sep="_")
    ) %>% 
    select(c("SampleID","Subj","TX","Day","Timepoint","TX Order")) %>%
    mutate(
        Subj = factor(Subj),
        TX = factor(TX, levels=(c("white","egg"))),
        Day = factor(Day, levels = c("Pre","Post")),
        Timepoint = factor(Timepoint),
        `TX Order` = factor(`TX Order`)
    ) %>%
    arrange(SampleID)

pdata = read_excel(
    path = "mx 349859_Zhu_HILIC-QTOF MSMS_11-2017_submit.xlsx",
    sheet="Submit", range = "H1:CS7"
) %>% as.data.frame %>%
    column_to_rownames("Label") %>%
    t %>%
    as.data.frame %>%
    rownames_to_column("SampleID") %>%
    mutate(SampleID = gsub("-","_",SampleID)) %>%
    arrange(SampleID)

pdata = merge(design, pdata, by = "SampleID", all=T) %>%
    column_to_rownames("SampleID")

## move qc samples to fdata
fdata = cbind(fdata, edata[,grep("Biorec",colnames(edata))])
edata = edata[,!grepl("Biorec",colnames(edata))]
pdata = pdata[!grepl("Biorec", rownames(pdata)),]

colnames(edata) = gsub("-","_", colnames(edata))
edata = edata[,rownames(pdata)]

# separate istd to is_data
is_data = cbind(fdata[grep("iSTD$",fdata$Annotation),],
            edata[grep("iSTD$",fdata$Annotation),])
edata = edata[!grepl("iSTD$",fdata$Annotation),]
fdata = fdata[!grepl("iSTD$",fdata$Annotation),]

# clean up the iSTD names

is_data$Annotation = gsub(" iSTD","",is_data$Annotation)
is_data$Annotation = gsub("1_", "", is_data$Annotation)
is_data$Annotation = gsub("D\\d{1}\\-", "",is_data$Annotation)
is_data$Annotation = gsub("D{0,1}L-","",is_data$Annotation)
is_data$Annotation[7] = "N-methyl-histamine" 

# remove the unannotated features
edata = edata[!is.na(fdata$Annotation),]
fdata = fdata[!is.na(fdata$Annotation),]

# read the standards data
standards = read_excel(
    path = "HILIC_ISTD_mix_2016-11-15 (1).xlsx",
    sheet = "SOP", range="A2:J26"
) %>% as.data.frame
standards = select(standards,-X__1)
standards$`ISTD short name` = gsub("D\\d{1}\\-","",standards$`ISTD short name`)

# calculate
edata_cali = edata[fdata$Annotation %in% standards$`ISTD short name`,]
fdata_cali = fdata[fdata$Annotation %in% standards$`ISTD short name`,]  
rownames(edata_cali) = fdata_cali$Annotation

istd_mat = is_data %>%
    remove_rownames() %>%
    column_to_rownames("Annotation")
rownames(istd_mat)[rownames(istd_mat) == "N-methyl-histamine"] = "1-Methylnicotinamide"
rownames(istd_mat)[rownames(istd_mat) == "Carnitine"] = "L-Carnitine"
istd_mat = istd_mat[rownames(edata_cali),colnames(edata_cali)]

std_mat = standards %>%
    remove_rownames() %>%
    column_to_rownames("ISTD short name")
std_mat = std_mat[rownames(edata_cali),"c (pmol/mL)"]

# unit: umol/mL
edata_cali = edata_cali / istd_mat * std_mat * ((100/110) * ((225 + 188)/20)) / 1000
#fdata_cali$unit = "umol/mL"

edata_list = list(
    "umol/mL" = edata_cali,
    "raw" = edata
)

Biogenic_Amines = list(
    edata_list = edata_list, 
    fdata      = fdata, 
    pdata      = pdata
)

## ----------------------------------------------------
setwd("/Users/chenghaozhu/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Egg Study/Result/Analysis/data")
save(Biogenic_Amines, file = "mcb.Rdata")

    
    
    
    
    
    











