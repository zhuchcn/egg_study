## -------- load packages ------------------------------------------------------
library(dplyr);library(plyr);library(reshape2);library(stringr)
library(tibble);library(data.table);library(readxl); library(parallel)

## -------- read chromatogram --------------------------------------------------
paths = list.files("../../raw_data/chromatograms/", full.names = TRUE)
cmg_data = lapply(paths, function(path){
    table = read.delim(path, header = T, skip=2, stringsAsFactors = F,
                       strip.white = T, blank.lines.skip=T)
})
names(cmg_data) = gsub("(Egg_\\d{3}_)ABCD00([1-4]).+","\\1\\2",paths)
names(cmg_data) = str_replace(
    names(cmg_data),
    "[1-4]$", LETTERS[as.integer(str_extract(names(cmg_data), "[1-4]$"))]
)

## -------- load ion mability data ---------------------------------------------
setwd("~/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Egg Study/Result/Analysis/data")
load("hdl.Rdata")
hdl_lg = as.numeric(ion_morbility$edata["HDL_2b",])
hdl_sm = as.numeric(ion_morbility$edata["HDL_3_2a",])

# normalize by total HDL
# hdl_tt = hdl_lg + hdl_sm
# hdl_lg = hdl_lg / hdl_tt
# hdl_sm = hdl_sm / hsl_tt

## -------- auc calculation ----------------------------------------------------
cmg_list = lapply(cmg_data, function(tbl){
    tbl[1:2] %>%
        na.omit()
})
auc_list = lapply(cmg_list, function(cmg){
    # cmg = cmg %>%
    #     filter(ml >= 7.7)
    auc_tbl = NULL
    for(i in 1:(nrow(cmg)-1)){
        ml1  = mean(cmg$ml[i:i+1])
        auc1 = 1/2 * (sum(cmg$mAU[i:i+1])) * (cmg$ml[i+1]-cmg$ml[i])
        if(is.null(auc_tbl)){
            auc_tbl = data.frame(ml = ml1, auc = auc1)
            next
        }
        auc_tbl = rbind(auc_tbl, c(ml1, auc1))
    }
    auc_tbl
})
# calculate the sum of AUC
#tAUC = sapply(auc_list, sum)

## -------- optimization -------------------------------------------------------
## small hdl
lowers = seq(10, 14, by = 0.1)
uppers = seq(12, 16, by = 0.1)

no_cores <- detectCores() - 1
cl <- makeCluster(no_cores, type="FORK")

smhdl_mat = parSapply(cl, lowers, function(lower){
    sapply(uppers, function(upper){
        if(lower >= upper){
        return(0)
    }
    auc = sapply(auc_list, function(auc1){
        auc1 %>%
            filter(between(ml, lower, upper)) %>%
            sum
        })
    cor(auc, hdl_sm)
    # cor(auc/tAUC, hdl_sm)
    })
})
rownames(smhdl_mat) = uppers
colnames(smhdl_mat) = lowers

## large hdl
lowers = seq(9, 12, by = 0.1)
uppers = seq(11, 14, by = 0.1)

no_cores <- detectCores() - 1
cl <- makeCluster(no_cores, type="FORK")

lghdl_mat = parSapply(cl, lowers, function(lower){
    sapply(uppers, function(upper){
        if(lower >= upper){
            return(0)
        }
        auc = sapply(auc_list, function(auc1){
            auc1 %>%
                filter(between(ml, lower, upper)) %>%
                sum
        })
        # cor(auc/tAUC, hdl_lg)
        cor(auc, hdl_lg)
    })
})
rownames(lghdl_mat) = uppers
colnames(lghdl_mat) = lowers

## -------- save ---------------------------------------------------------------
save(lghdl_mat, smhdl_mat, hdl_lg, hdl_sm, auc_list, cmg_data, #hdl_tt,
     file = "../explor/Rdata/cmg_explor.Rdata")



# the goal of the codes below is to find a timepoint/volumn to cut the
# chromatogram to keep it only after LDL peak. 
# 
# It appears that 7.7 is a good cut off for all samples.

# cmg_list = lapply(cmg_data, function(table){
#     table[,1:2] %>%
#         na.omit()
# })
# 
# shinyApp(
#     ui = basicPage(
#         h4("Current values:"),
#         selectInput("sample","Select a Sample",
#                     choices = names(cmg_list), selected = names(cmg_list)[1]),
#         plotlyOutput("cmg")
#         
#     ),
#     server = function(input, output, session) {
#         
#         output$cmg = renderPlotly(
#             (cmg_list[[input$sample]] %>%
#                 ggplot(aes(ml, mAU)) +
#                 geom_line() +
#                 geom_vline(xintercept = 7.7, color = "red", 
#                            linetype = "dashed") +
#                 theme_bw()) %>%
#                 ggplotly
#         )
#     }
# )

