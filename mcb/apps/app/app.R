pkgs = c("shiny", "dplyr", "reshape2", "glue", "tibble", "Metabase", 
         "ggplot2", "DT", "plotly", 'ggsci', "shinydashboard")
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}

# Define UI for application that draws a histogram
ui <- dashboardPage(
    header = dashboardHeader(title = "Egg Study"),
    sidebar = sidebar,
    body = body
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Biogenic Amines
    source("path/to/bga/boxplot.R", local = TRUE)
    
}

# Run the application 
shinyApp(ui = ui, server = server)

