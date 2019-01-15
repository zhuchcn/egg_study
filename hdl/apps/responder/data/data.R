library(dplyr); library(reshape2); library(tibble); library(stringr)

setwd(dirname(parent.frame(2)$ofile))

load("../../app/data/data.rda")

data = list(
    lpd = data$data$lpd,
    fct = data$data$fct,
    cli = data$data$cli,
    diet = data$data$diet
)

data$lpd$class = data$lpd$class[c("Concentration", "Proportion")]
data$lpd$feature = data$lpd$feature[c("Concentration", "Proportion")] 

save(data, file = "data.rda")