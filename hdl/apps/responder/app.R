pkgs = c("shiny", "dplyr", "reshape2", "stringr", "tibble", 
         "DT", "plotly", "Metabase", "shinydashboard")
for(pkg in pkgs) {
    library(pkg, character.only = T, quietly = T, verbose = F, warn.conflicts = F)
}

load("data/data.rda")
source("ui/sidebar.R")
source("ui/body.R")

# Define UI for application that draws a histogram
ui <- dashboardPage(
    header = dashboardHeader(
        title = "Responder vs Non-responder",
        titleWidth = "30%"
    ),
    sidebar = sidebar,
    body = body
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # Inputs
    source("ui/inputs.R", local = T)
    
    source("server/lpd.R", local = T)
    source("server/fct.R", local = T)
    source("server/cli.R", local = T)
    source("server/diet.R", local = T)
    
    shinyjs::addClass(class="nav-justified", selector = ".nav")
    shinyjs::addClass(class="btn btn-primary", selector = "ul.nav > li> a")
    shinyjs::addClass(class="btn btn-warning", selector = "ul.nav > li > a")

}

# Run the application 
shinyApp(ui = ui, server = server)

