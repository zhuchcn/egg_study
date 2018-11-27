## ------------------- loading librarys ------------------------
pkgs = c('dplyr','stringr','reshape2','tibble',
         'Metabase')
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}
setwd(dirname(parent.frame(2)$ofile))
load("../../data/mcb.rda")

## -------- statistic analysis -------------------------------------------------

design = model.matrix(
    data = as(bga$sample_table, "data.frame"),
    ~ Treatment * Timepoint + Subject + 1
)
limma = mSet_limma(bga, design, coef = 23, p.value = 23)

## -------- save ---------------------------------------------------------------
save(bga,limma, file = "../Rdata/bga_precalc.rda")

# shiny::shinyApp(
#     ui = basicPage(
#         fluidPage(
#             fluidRow(
#                 column(
#                     width = 6,
#                     DT::dataTableOutput('lmTable')
#                 ),
#                 column(
#                     width = 6,
#                     plotly::plotlyOutput('boxplot')
#                 )
#             )
#         )
#     ),
#     server = function(input, output, session){
#         
#         lmTable = limma %>%
#             as.data.frame %>%
#             rownames_to_column("Feature") %>%
#             arrange(pvalue) %>%
#             sapply(function(x){
#                 if(is.numeric(x))return(round(x, 3))
#                 else return(x)
#             }) %>% as.data.frame %>%
#             column_to_rownames("Feature")
#         
#         output$lmTable = DT::renderDataTable(
#             lmTable, 
#             server = T, selection = "single")
#         
#         output$boxplot = renderPlotly({
#             feature = rownames(lmTable)[input$lmTable_rows_selected]
#             plot_boxplot(bga, x = "Timepoint", feature = feature, cols = "Treatment", line = "Subject", color = "Subject")
#         })
#     }
# )