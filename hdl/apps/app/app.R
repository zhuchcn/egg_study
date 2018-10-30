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
    
    source("server/lpd/boxplot.R",  local = TRUE)
    source("server/lpd/hist.R",     local = TRUE)
    source("server/lpd/pie.R",      local = TRUE)
    source("server/lpd/corr_imb.R", local = TRUE)
    source("server/lpd/corr_fct.R", local = TRUE)
    source("server/lpd/corr_cli.R", local = TRUE)
    
    source("server/imb/boxplot.R",  local = TRUE)
    source("server/imb/corr_fct.R", local = TRUE)
    source("server/imb/corr_cli.R", local = TRUE)
    source("server/imb/corr_diet.R", local = TRUE)

    source("server/fct/boxplot.R",  local = TRUE)
    source("server/fct/corr_fct.R", local = TRUE)
    source("server/fct/corr_cli.R", local = TRUE)
    
    source("server/cli/boxplot.R",  local = TRUE)
    
    source("server/diet/boxplot.R",  local = TRUE)
}

# Run the application 
shinyApp(ui = ui, server = server)

