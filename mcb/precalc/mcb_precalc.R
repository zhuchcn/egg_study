pkgs = c("dplyr", "stringr", "reshape2", "tibble", "phyloseq", "phylox", 
         "Metabase", "DESeq2")
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}
setwd(dirname(parent.frame(2)$ofile))
load("../../data/mcb.rda")

## remove subjects with only one treatment not the other
rm_2sub = mcb$sample_table %>% group_by(Subject) %>% summarize(Nsample = length(Subject))
rm_2subID = rm_2sub[rm_2sub$Nsample ==2,]$Subject

## remvoe the subject with extreme low total count
# colSums(mcb$conc_table)
# Sample 111C only has 7 reads

mcb = Metabase::subset_samples(mcb, ! mcb$sample_table$Subject %in% c(rm_2subID, 111) )

# Remove features with all zeros. This is produced because of the 3 subjects removed.
mcb = Metabase::subset_features(mcb, rowSums(mcb$conc_table !=0 ) > 1)

## -------- Summarize to each taxa levels --------------------------------------
mcb = as_phyloseq(mcb)
mcb = phylox::fix_duplicate_tax(mcb)
# mcb_count = summarizeFromPhyloseq(mcb) %>%
#     as_MicrobiomeSetList()
mcb = transform_sample_counts(mcb, function(x) x/sum(x, na.rm = TRUE)) %>%
    summarizeFromPhyloseq() %>%
    as_MicrobiomeSetList()

# mcb = list(
#     count = mcb_count,
#     proportion = mcb_prop
# )

mcb = lapply(
    mcb, function(mset) 
            subset_features(mset, Metabase::featureNames(mset) != "NA")
)

design = model.matrix(data = as(mcb$kingdom$sample_table, "data.frame"), 
                      ~Treatment*Timepoint + Subject + 1)
# ds = lapply(mcb$count, function(x) {
#     mSet_deseq(x, design, 22)
#     #cat("test")
# })
lm = lapply(mcb, function(x) mSet_limma(x, design, coef = 22, p.value = 22))
 
save(mcb, lm, file = '../Rdata/mcb_precalc.rda')

# shinyApp(
#     ui = basicPage(
#         fluidPage(
#             fluidRow(
#                 column(
#                     width = 6,
#                     selectInput('level', "MCB Level",
#                                 choices = names(mcb), selected = 'genus'),
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
#         lmTable = reactive({
#             lm[[input$level]] %>%
#                 as.data.frame %>%
#                 rownames_to_column("Feature") %>%
#                 arrange(pvalue) %>%
#                 sapply(function(x){
#                     if(is.numeric(x))return(round(x, 3))
#                     else return(x)
#                 }) %>% as.data.frame %>%
#                 column_to_rownames("Feature")
#         })
#         output$lmTable = DT::renderDataTable(
#             lmTable(),
#             server = T, selection = "single")
#         output$boxplot = renderPlotly({
#             feature = rownames(lmTable())[input$lmTable_rows_selected]
#             plot_boxplot(mcb[[input$level]], x = "Timepoint", feature = feature, cols = "Treatment", line = "Subject", color = "Subject")
#         })
#     }
# )

# genus = mcb$genus
# data.frame(
#     ratio = mcb$phylum$conc_table["Firmicutes",] / mcb$phylum$conc_table["Bacteroidetes",],
#     Treatment = genus$sample_table$Treatment,
#     Timepoint = genus$sample_table$Timepoint,
#     Subject = genus$sample_table$Subject
# ) %>%
#     ggplot(aes(Timepoint, ratio)) +
#     geom_boxplot() +
#     geom_point(aes(color = Subject)) +
#     geom_line(aes(group = Subject, color = Subject)) +
#     facet_grid(.~Treatment)
