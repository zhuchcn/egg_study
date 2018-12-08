#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
pkgs = c('dplyr','stringr','reshape2','tibble',
         'Metabase', 'ggplot2', 'knitr', 'kableExtra', 'plotly')
for(pkg in pkgs){
    library(pkg, verbose = FALSE, warn.conflicts = FALSE,
            character.only = TRUE, quietly = TRUE)
}
load("../../Rdata/bga_precalc.rda")

ui <- fluidPage(
    
    # App title ----
    titlePanel("Pre-Post peak height"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Selector for variable to plot against mpg ----
            selectInput(inputId = "variable", label = "Variable:",
                        choices = bga$feature_data$Annotation),
            
            # Input: Checkbox for whether outliers should be included ----
            checkboxInput("outliers", "Show outliers", TRUE)
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Plot of the requested variable against mpg ----
            plotlyOutput("AminePlot")
            
        )
    )
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
    
    # Generate a plot of the requested variable against mpg ----
    # and only exclude outliers if requested
    output$AminePlot <- renderPlotly({
        df = data.frame(some_amine = bga$conc_table[which(bga$feature_data$Annotation == input$variable),],
                        timepoint = bga$sample_table$Timepoint,
                        treatment = bga$sample_table$Treatment,
                        subject = bga$sample_table$Subject)
        df$timepoint <- factor(df$timepoint, labels = c("Pre", "Post"))
            ggplot(df, aes(x=timepoint, y=some_amine)) +
            geom_boxplot() +
            facet_grid(~treatment) + 
            geom_line(aes(group = subject, colour = subject)) + 
            geom_point(aes(color = subject)) +
            theme_bw()
    })
    
}

# Create Shiny app ----
shinyApp(ui, server)