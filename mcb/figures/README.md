## Project structure

This is where we are going to write all the scripts to generate our figures for this manuscript. This project should be structured as below.

```
.
+-- src
|   +-- global.R
|   +-- data.R
|   +-- fig1.R
|   +-- fig2.R
+-- data
|   +-- data.rda
+-- png
|   +-- fig1.png
|   +-- fig2.png
+-- pdf
|   +-- fig1.pdf
|   +-- fig2.pdf
+-- supplement
|   +-- supplement.Rmd
|   +-- supplement.pdf
+-- tables
|   +-- table1.Rmd
|   +-- table1.doc
|   +-- table2.Rmd
|   +-- table2.doc
+-- ext
```

### src

The `src` folder should contain all the scripts each generate one figure for the manuscript. 

#### global.R

The `global.R` contains any parameters and functions definitions that is shared by all figures (don't forget to `source` it in each `figure.R`). It usually contains three portions.

+ packages

```
pkgs = c('dplyr', 'reshape2', 'tibble', 'stringr', 'ggplot2')
for(pkg in pkgs){
    suppressPackageStartupMessages(
        library(pkg, character.only = TRUE)
    )
}
```

+ themes

Define any ggplot2 themes that will be shared in all figures. The reason to do so is for easier style adjust later on because you only need to change one place and it applies everywhere.

Below is my usual themes.

```
text.size = 3.5
title.size = 11
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
```

+ functions

Define any functions that will be used repeatedly, and document them with a `roxygen2` style on the top of each function. Below are two examples.

```
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
## -------- scatterplot --------------------------------------------------------
#' @param mset_x the mset for x axis variable
#' @param mset_y the mset for y axis variable
#' @param feature_x string, the x variable in mset_x
#' @param feature_y string, the y variable in mset_y
#' @param show_legend boolean, whether to show legend
#' @param xlab stirng, the x axis label
#' @param ylab string, the y axis label
#' @param label_x numeric, the x position for label, must between 0 and 1
#' @param label_y numeric, the y position for label, must between 0 and 1
my_scatterplot = function(mset_x, mset_y, feature_x, feature_y, 
                          show_legend = TRUE, 
                          xlab = feature_x,
                          ylab = feature_y, 
                          title = "", 
                          label_x = 0.8, label_y = 0.8,
                          cor.method = "pearson"){
    # prepare the data frame
    df = data.frame(
        x = mset_x$conc_table[feature_x, ],
        y = mset_y$conc_table[feature_y, ],
        color = mset_x$sample_table$Subject
    )
    # get the P and R values
    cor_res = cor.test(df$x, df$y, method = cor.method)
    pval = cor_res$p.value
    pval = if(pval < 0.001) "P < 0.001" else glue("P = {round(pval, 3)}")
    rval = cor_res$estimate
    rval = glue("R = {round(rval, 3)}")
    label = glue("{rval}\n{pval}")
    # get label position
    range_x = range(df$x, na.rm = TRUE)
    range_y = range(df$y, na.rm = TRUE)
    label_x = label_x * (range_x[2] - range_x[1]) + range_x[1]
    label_y = label_y * (range_y[2] - range_y[1]) + range_y[1]
    # make the plot
    p = ggscatterplot(df, "x", "y", color = "color", trendline.color = "steelblue",
                  color.pal = pal_npg()(10), point.size = 2) +
        annotate(geom = "text", x = label_x, y = label_y, label = label, 
                 size = text.size) +
        labs(x = xlab, y = ylab, title = title) +
        theme_scatter()
    if(!show_legend) {
        p = p + theme(legend.position = "none")
    }
    return(p)
}
```

#### data.R

This script reads and cleans up the data, and then `save` into the `./data/data.rda` file, into a single nested `list` called `data`. The `data.rda` can then be loaded whenever it is needed.

### data

Contain the `.rda` file(s), which is saved by the `src/data.R`, and will be loaded in any scripts if needed.

### png & pdf

The `png` folder contains all figures in `png` format while the pdf contains `pdf`. We usually only need to make figures in the `png` format during the first submission and `pdf` format is usually required during the second.

### supplement

This folder contains the `.Rmd` script which generates the supplemental data for the manuscript in `pdf` format.

### tables

This folder contains the `.Rmd` script each generates a table for the manuscript in `doc` format.

### ext

This folder contains any extra files/scripts.

## Figures

### Fig 1

+ **sub figure A:** alpha diversity boxplot before and after egg and white
+ **sub figure B:** beta diversity PCoA plot with weighted unifrac. Lets start by using the percentage data (so you won't need rarifying it), and coloring the points with treatment * teimpoint. If that does not look good, we'll color the points with subject IDs to show that individual variance is dominating.
+ **sub figure C:** stacked taxonomy bar plot at phylum level, before and after egg and white.

### Fig 2

Cladogram. See this [vignette](https://zhuchcn.github.io/docs/packages/phylox/basic_usage.html). An example is in `./ext/clado.R`.

### Fig 3

Box plot of choline, betaine, and TMAO values before and after egg and white.

### Fig 4

Find microbiome genera and biogenic amines that are mostly correlated with each other using `RCCA`, and make a correlation heatmap using selected genera and biogenic amines. And example can be found in `./ext/integrative-analysis.Rmd.`

### Fig 5

Partial correlation network using microbiome genera and biogenic amines data together. An example can be found in `./ext/network.Rmd`. This script generates the partial correlation matrix, and save into `node.csv` and `edge.csv` files, to be imported into [cytoscape](https://cytoscape.org/)
