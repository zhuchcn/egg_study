pkgs = c('dplyr','stringr','reshape2','tibble', 'plotly', 'DT', 'Metabase','ggsci',
         "shiny", "shinydashboard", "ggmetaplots")
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}

load('data/data.rda')

source("ui/sidebar.R")
source("ui/body.R")

ui <- dashboardPage(
    header = dashboardHeader(
        title = tags$a(
            href="http://www.chenghaozhu.net/studies/egg/docs/hdl.html", 
            style="color:inherit;",
            "Egg Study"
        )
    ),
    sidebar = sidebar,
    body = body
)

server <- function(input, output) {
    source("ui/inputs.R",          local = TRUE)
    
    source("server/lpd/boxplot.R",  local = TRUE)
    source("server/lpd/hist.R",     local = TRUE)
    source("server/lpd/corr_imb.R", local = TRUE)
    source("server/lpd/corr_fct.R", local = TRUE)
    source("server/lpd/corr_cli.R", local = TRUE)
    
    source("server/imb/boxplot.R",  local = TRUE)
    source("server/imb/corr_fct.R", local = TRUE)
    source("server/imb/corr_cli.R", local = TRUE)
    
    source("server/fct/boxplot.R",  local = TRUE)
    source("server/fct/corr_fct.R", local = TRUE)
    source("server/fct/corr_cli.R", local = TRUE)
}

# Run the application 
shinyApp(ui = ui, server = server)

