library(dplyr); library(reshape2); library(tibble); library(stringr)

setwd(dirname(parent.frame(2)$ofile))

load("../../app/data/data.rda")

data = list(
    lpd = data$data$lpd,
    fct = data$data$fct
)

data$lpd$class = data$lpd$class[c("Concentration", "Proportion")]
data$lpd$feature = data$lpd$feature[c("Concentration", "Proportion")] 

data$fct = data.frame(
    value = data$fct$conc_table['Chol Efflux (ApoB Depleted Serum 1%)',],
    Subject = data$fct$sample_table$Subject,
    Treatment = data$fct$sample_table$Treatment,
    Timepoint = data$fct$sample_table$Timepoint
) %>% 
    dcast(Treatment + Subject ~ Timepoint) %>%
    mutate(value = post - pre) %>%
    dcast(Subject ~ Treatment, value.var = "value") %>%
    mutate(chol_efflux_change = egg - sub) %>%
    select(Subject, chol_efflux_change)

save(data, file = "data.rda")