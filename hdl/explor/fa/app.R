pkgs = c('dplyr','stringr','reshape2','tibble', 'plotly', 'DT', 'Metabase','ggsci',
         "shiny", "shinydashboard", "ggmetaplots")
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}

load('../Rdata/precalc.Rdata')

ui <- dashboardPage(
    dashboardHeader(title = "HDL Lipidome"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Home", icon = icon("home"), newtab = FALSE,
                     href = "http://www.chenghaozhu.net/studies/egg/docs/hdl.html")
        ),
        checkboxInput("fa", "Exclude FA?", value = FALSE),
        selectInput("level", "Lipid Class or Feature",
                    choices = c("class", "feature"),
                    selected = "class"),
        selectInput("norm", "How to normalize the data",
                    choices = names(lpd_li$`All Features`$class),
                    selected = "Concentration"),
        selectInput("method", "Select a Correlation Method",
                    choices = names(corr_fct[[1]][[1]][[1]]),
                    selected = "pearson"),
        sidebarMenu(
            menuItem("Boxplot", tabName = "boxplot"),
            menuItem("Histograms", tabName = "hist"),
            menuItem("vs HDL Function", tabName = "func"),
            menuItem("vs Clinical Values", tabName = "clinical")
        )
    ),
    dashboardBody(
        tabItems(
            ## Boxplot
            tabItem(
                tabName = "boxplot",
                fluidRow(
                    column(
                        width = 6,
                        box(width = NULL,
                            DTOutput("limma_dt"), height = "90%")
                    ),
                    column(
                        width = 6,
                        box(width = NULL, 
                            plotlyOutput("boxplot"))
                    )
                )
            ),
            ## Hist
            tabItem(
                tabName = "hist",
                fluidRow(
                    column(
                        width = 6,
                        box(width = NULL,
                            plotlyOutput("hist_pval"), height = "100%")
                    ),
                    column(
                        width = 6,
                        box(width = NULL,
                            plotlyOutput("hist_padj"), height = "100%")
                    )
                )
            ),
            ## Corr with HDL Function
            tabItem(
                tabName = "func",
                fluidRow(
                    column(
                        width = 6,
                        box(width = NULL,
                            DTOutput("fct_dt"), height = "100%" )
                    ),
                    column(
                        width = 6,
                        box(width = NULL, height = "10%",
                            column(
                                width = 6, 
                                selectInput("fct", "Select HDL Function Variable",
                                            choices = featureNames(hdl_function),
                                            selected = featureNames(hdl_function)[1]))
                        ),
                        box(width = NULL, height = "90%",
                            plotlyOutput("fct_scatter"))
                    )
                )
            ),
            ## Corr with Clinical Values
            tabItem(
                tabName = "clinical",
                fluidRow(
                    column(
                        width = 6,
                        box(width = NULL,
                            DTOutput("clinical_dt"), height = "100%" )
                    ),
                    column(
                        width = 6,
                        box(width = NULL, height = "10%",
                            column(
                                width = 6, 
                                selectInput("clinical", "Select A Clinical Variable",
                                            choices = featureNames(clinical),
                                            selected = featureNames(clinical)[1]))
                        ),
                        box(width = NULL, height = "90%",
                            plotlyOutput("clinical_scatter"))
                    )
                )
            )
        )
    )
)

gghist = function(data, x){
    ggplot(data) +
        geom_histogram(aes_string(x), breaks = seq(0,1,0.025),
                       color = "white") +
        geom_vline(xintercept = 0.05, color = "red", linetype = "dashed",
                   size = 1) +
        theme_bw()
}

server <- function(input, output) {
    fa = reactive({if(input$fa) "FA Removed" else "All Features"})
    limma_table = reactive({limma_li[[fa()]][[input$level]][[input$norm]]})
    ## boxplot
    limma_dt = reactive({
         limma_table()%>%
            rownames_to_column("Feature") %>%
            arrange(P.Value) %>%
            sapply(function(col){
                if(!is.numeric(col)) return(col)
                round(col, digits = 3)
            }) %>%
            as.data.frame %>%
            column_to_rownames("Feature")
    }) 
    
   output$limma_dt = renderDT(
       limma_dt(), 
       selection = list(mode = "single", selected = 1),
       server=T
   )
   
   boxplot_selector = reactive({
       rownames(limma_dt())[input$limma_dt_rows_selected]
   })
   
   output$boxplot = renderPlotly({
       mset = lpd_li[[fa()]][[input$level]][[input$norm]]
       p = plot_boxplot(mset, 
                        x = "Timepoint", 
                        feature = boxplot_selector(),
                        cols = "Treatment",
                        line = "Subject",
                        color = "Subject",
                        color.pal = pal_jama()(7)) +
           labs(x = "")
       ggplotly(p)
   })
   ## Hist
   output$hist_pval = renderPlotly({
       p = gghist(limma_table(), "P.Value")
       ggplotly(p)
   })
   output$hist_padj = renderPlotly({
       p = gghist(limma_table(), "adj.P.Val")
       ggplotly(p)
   })
   ## fct scatterplot
   fct_dt = reactive({
       corr_fct[[fa()]][[input$level]][[input$norm]][[input$method]][[input$fct]] %>%
           rownames_to_column("Feature") %>%
           arrange(pval) %>%
           sapply(function(col){
               if(!is.numeric(col)) return(col)
               round(col, digits = 3)
           }) %>%
           as.data.frame %>%
           column_to_rownames("Feature")
   })
   
   output$fct_dt = renderDT(
       fct_dt(), 
       selection = list(mode = "single", selected = 1),
       server=T
   )
   fct_selector = reactive({
       rownames(fct_dt())[input$fct_dt_rows_selected]
   })
   
   output$fct_scatter = renderPlotly({
       df = data.frame(
           x = lpd_li[[fa()]][[input$level]][[input$norm]]$conc_table[fct_selector(),],
           y = hdl_function$conc_table[input$fct,],
           Treatment = hdl_function$sample_table$Treatment,
           Timepoint = hdl_function$sample_table$Timepoint,
           Subject = hdl_function$sample_table$Subject
       )
       p = ggscatterplot(df, "x", "y", color = "Subject", color.pal = pal_jama()(7)) +
           labs(x = paste0(fct_selector(), " [", input$norm, "]"), 
                y = input$fct)
       ggplotly(p)
   })
   ## fct scatterplot
   clinical_dt = reactive({
       corr_clinical[[fa()]][[input$level]][[input$norm]][[input$method]][[input$clinical]] %>%
           rownames_to_column("Feature") %>%
           arrange(pval) %>%
           sapply(function(col){
               if(!is.numeric(col)) return(col)
               round(col, digits = 3)
           }) %>%
           as.data.frame %>%
           column_to_rownames("Feature")
   })
   
   output$clinical_dt = renderDT(
       clinical_dt(), 
       selection = list(mode = "single", selected = 1),
       server=T
   )
   clinical_selector = reactive({
       rownames(clinical_dt())[input$clinical_dt_rows_selected]
   })
   
   output$clinical_scatter = renderPlotly({
       df = data.frame(
           x = lpd_li[[fa()]][[input$level]][[input$norm]]$conc_table[clinical_selector(),],
           y = clinical$conc_table[input$clinical,],
           Treatment = clinical$sample_table$Treatment,
           Timepoint = clinical$sample_table$Timepoint,
           Subject = clinical$sample_table$Subject
       )
       p = ggscatterplot(df, "x", "y", color = "Subject", color.pal = pal_jama()(7)) +
           labs(x = paste0(clinical_selector(), " [", input$norm, "]"), 
                y = input$clinical)
       ggplotly(p)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

