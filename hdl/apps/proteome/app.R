pkgs = c("shiny", "shinydashboard", "dplyr", "reshape2", "Metabase",
         "ggplot2", "plotly", "DT", "ggsci", "tibble", "ggmetaplots",
         "heatmaply", "glue")
for(pkg in pkgs){
    library(pkg, character.only = TRUE)
}

load("data/data.rda")

import::here(sidebar, .from = "ui/sidebar.R")
import::here(body, .from = "ui/body.R")

# Define UI for application that draws a histogram
ui <- dashboardPage(
   header = dashboardHeader(title="Egg Study Proteome"),
   sidebar = sidebar,
   body = body
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # import inputs
    source("ui/inputs.R", local = TRUE)
    
    for (script in list.files("server", recursive = T, full.names = T)) {
        source(script, local = TRUE)
    }
}

# Run the application 
shinyApp(ui = ui, server = server)

