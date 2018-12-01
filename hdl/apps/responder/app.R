pkgs = c("shiny", "shinydashboard", "dplyr", "reshape2", "glue", "Metabase",
         "ggplot2", "plotly", "DT", "ggsci")
for(pkg in pkgs){
    library(pkg, character.only = TRUE, quietly = TRUE, verbose = FALSE, warn.conflicts = FALSE)
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
    
    # Proteome
    source("server/prt/boxplot.R", local = TRUE)
    source("server/prt/corr_lpd.R", local = TRUE)
    source("server/prt/corr_fct.R", local = TRUE)
    
    # Lipidome
    source("server/lpd/boxplot.R", local = TRUE)
    
    # Function
    source("server/fct/boxplot.R", local = TRUE)
}

# Run the application 
shinyApp(ui = ui, server = server)

