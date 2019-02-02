pkgs = c('dplyr','stringr','reshape2','tibble', 'plotly', 'DT', 'Metabase','ggsci',
         "shiny", "shinydashboard", "ggmetaplots", "knitr", "kableExtra")
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}

load('data/data.rda')

source("ui/sidebar.R")
source("ui/body.R")

ui <- dashboardPage(
    header = dashboardHeader(title = "Egg Study"),
    sidebar = sidebar,
    body = body
)

server <- function(input, output) {
    source("ui/inputs.R",          local = TRUE)
    
    for(script in list.files("server", full.names = TRUE, 
                             recursive = T, include.dirs = T)){
        source(script, local = TRUE)
    }
}

# Run the application 
shinyApp(ui = ui, server = server)

