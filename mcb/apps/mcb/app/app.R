pkgs = c('phyloseq','phylox','ggplot2','ape', 'plotly',
         "shiny", "shinydashboard", "DT", "shinyjs")
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}

load('../../../../data/mcb.rda')

ui <- dashboardPage(
    
    skin = "red",
    header = dashboardHeader(title = "Egg Study"),
    sidebar = dashboardSidebar(
        
        sidebarMenu(
            menuItem("Home", icon = icon("home"), newtab = FALSE,
                     href="http://www.chenghaozhu.net/studies/egg/mcb.html")
        ),
        
        sidebarMenu(
            id = "sidebar",
            menuItem("Taxonomy Composition Barplot", tabName = "taxon_barplot"),
            menuItem("Alpha Diversity Boxplot", tabName = "alpha_boxplot"),
            menuItem("Beta Diversity Tree", tabName = "beta_tree"),
            menuItem("Phylogeny Level Boxplot", tabName = "phylo_boxplot")
        )
    ),
    
    body = dashboardBody(
        
        tags$head(
            tags$link(rel="stylesheet", type = "text/css", href = "styles.css"),
            shinyjs::useShinyjs()
        ),
        
        tabItems(
            tabItem(
                tabName = "taxon_barplot",
                fluidRow(
                    tags$div(
                        class = "col-sm-12 col-md-9 col-lg-6",
                        box(width = NULL,
                            selectInput("taxon", "Select a taxonomy level:",
                                        choices = c("kingdom", "phylum", "class", 
                                                    "order", "family", "genus",
                                                    "species"),
                                        
                                        selected = "phylum")),
                        box(width = NULL,
                            plotlyOutput("taxon_barplot"))
                    )
                )
            ),
            
            tabItem(
                tabName = "alpha_boxplot",
                fluidRow(
                    tags$div(
                        class = "col-sm-12 col-md-9 col-lg-6",
                        box(width = NULL,
                            selectInput("measure", "Select a measuring method:",
                                        choices = c("Observed", "Chao1", "ACE",
                                                    "Shannon", "Simpson", "InvSimpson",
                                                    "Fisher"),
                                        
                                        selected = "Shannon")),
                        box(width = NULL,
                            plotlyOutput("alpha_boxplot"))
                    )
                )
            ),
            
            tabItem(
                tabName = "beta_tree",
                fluidRow(
                    tags$div(
                        class = "col-sm-12 col-md-9 col-lg-6",
                        box(width = NULL,
                            plotlyOutput("beta_tree"))
                    )
                )
            ),
            
            tabItem(
                tabName = "phylo_boxplot",
                fluidRow(
                    column(
                        width = 6,
                        box(width = NULL,
                            selectInput("taxon2", "Select a taxonomy level:",
                                        choices = c("kingdom", "phylum", "class", 
                                                    "order", "family", "genus",
                                                    "species"),
                                        
                                        selected = "phylum")
                        ),
                        box(width = NULL,
                            DT::dataTableOutput("phylo_table")
                        )
                    ),
                    column(
                        width = 6,
                        box(width = NULL,
                            plotlyOutput("phylo_boxplot"))
                    )
                )
            )
        )
    )
)

server <- function(input, output) {
    
    ps = Metabase::as_phyloseq(mcb)
    sum_ps = summarizeFromPhyloseq(ps)
    ps_prop = transform_sample_counts(ps, function(x) x/sum(x))
    sum_ps_prop = summarizeFromPhyloseq(ps_prop)
    
    output$taxon_barplot <- renderPlotly({
        otu = slot(sum_ps_prop, paste0(input$taxon, "_table"))
        otu = otu[rownames(otu) != "NA",]
        ps = phyloseq(otu, sample_data(sum_ps_prop))
        plot_bar(ps, level = "OTU", by = c("Timepoint", "Treatment")) +
            guides(fill = guide_legend(title = input$taxon))
    })
    
    output$alpha_boxplot <- renderPlotly({
        plot_richness(ps, x = "Timepoint", measures = input$measure) +
            geom_boxplot(lwd = 0.7) +
            geom_line(aes(group = Subject, color = Subject)) +
            geom_point(aes(color = Subject)) +
            facet_grid(cols = vars(Treatment))
    })
    
    output$beta_tree <- renderPlotly({
        ps_prop = merge_phyloseq(ps_prop, tree)
        ps_prop_ord <- ordinate(ps_prop, "NMDS", "bray")
        
        df1 = data.frame(ps_prop_ord$points, 
                         StudyID = sample_data(ps_prop)$StudyID, 
                         Treatment = sample_data(ps_prop)$Treatment, 
                         Timepoint = sample_data(ps_prop)$Timepoint)
        
        p1 = ggplot(data = df1)
        p1 = p1 + geom_point(aes(x = MDS1, y = MDS2, color = interaction(Timepoint, Treatment)))
        
        ggplotly(p1)
    })
    
    phylo_limma = reactive({
        design = model.matrix(data = as(sample_data(ps), "data.frame"),
                              ~Treatment * Timepoint + Subject)
        spys_lm = spy_to_limma(sum_ps_prop, design, transform = "log",
                               p.value = 2, coef = 2)
        #eval(parse(text = paste0(input$taxon2, "_table(spys_lm)")))
        slot(spys_lm, paste0(input$taxon2, "_table"))
    })
    
    output$phylo_table = DT::renderDataTable(
        phylo_limma() %>%
            datatable(
                selection = list(mode = "single", selected = 1)
            ) %>%
            formatRound(columns = 1:5, digits = 2)
    )
    
    output$phylo_boxplot <- renderPlotly({
        logjs(input$phylo_table_rows_selected)
        otu = slot(sum_ps_prop, paste0(input$taxon2, "_table"))
        df = data.frame(otu = as.numeric(otu[input$phylo_table_rows_selected,]), 
                        studyID = sample_data(sum_ps_prop)$StudyID, 
                        treatment = sample_data(sum_ps_prop)$Treatment, 
                        timepoint = sample_data(sum_ps_prop)$Timepoint)
        
        p = ggplot(df, aes(timepoint, otu))
        p = p + geom_boxplot(lwd = 0.7) +
            geom_point(aes(color = studyID)) +
            geom_line(aes(group = studyID, color = studyID)) +
            facet_grid(cols = vars(treatment))
       
        ggplotly(p)
    }) 
    
    phylo_boxplot_selector = reactive({
        rownames(phylo_limma())[input$phylo_phylo_table_rows_selected]
    })
    
}

shinyApp(ui = ui, server = server)