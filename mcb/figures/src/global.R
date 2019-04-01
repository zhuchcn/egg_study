pkgs = c('dplyr', 'reshape2', 'tibble', 'stringr', 'ggplot2', 'ggsci')
for(pkg in pkgs){
    suppressPackageStartupMessages(
        library(pkg, character.only = TRUE)
    )
}
load("../data/data.rda")

## -------- themes -------------------------------------------------------------
`%+%` = function(x,y){
    paste0(x,y)
}

point.size = 5
text.size = 10
title.size = 28
my_theme = function(){
    theme_bw() +
        theme(
            axis.title = element_text(size = title.size),
            axis.text = element_text(size = title.size - 2),
            legend.title = element_text(size = title.size - 1),
            legend.text = element_text(size = title.size - 2),
            legend.key.size = unit(1, "line"),
            plot.title = element_text(hjust=0.5, size = title.size),
            strip.text = element_text(size = title.size)
        )
}
theme_scatter = function(){
    my_theme() +
        theme(axis.title.y = element_text(vjust = -1.5))
}
theme_boxplot = function(){
    my_theme() +
        theme(
            axis.title.x = element_blank(),
            axis.text.x = element_text(colour = "black")
        )
}

## -------- boxplot ------------------------------------------------------------
#' @param mset the data object must be mSet class
#' @param feature string, the feature to plot
#' @param statsRes data.frame, the limma result
#' @param ylab string, the y axis label
#' @param featureNameToDisplay string, the name of the feature to display on title
#' @param show_legend boolean

my_boxplot = function(mset, 
                      feature, 
                      statsRes, 
                      ylab = "", 
                      featureNameToDisplay = feature, 
                      show_legend = TRUE){
    pval = statsRes[feature, "pvalue"]
    if(pval < 0.001) {
        title = glue("{featureNameToDisplay} (P < 0.001)")
    } else {
        title = glue("{featureNameToDisplay} (P = {round(pval, 3)})")   
    }
    p = plot_boxplot(mset, "Timepoint", feature, cols = "Treatment", 
                     line = "Subject", color = "Subject", color.pal = pal_npg()(10)) +
        labs(title = title,
             y = ylab) +
        theme_boxplot()
    if(!show_legend) {
        p = p + theme(legend.position = "none")
    }
    return(p)
}



