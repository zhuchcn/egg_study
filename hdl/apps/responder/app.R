pkgs = c("shiny", "dplyr", "reshape2", "stringr", "tibble", "glue",
         "DT", "plotly", "Metabase", "shinydashboard")
for(pkg in pkgs) {
    library(pkg, character.only = T, quietly = T, verbose = F, warn.conflicts = F)
}

source("global.R", local = T)

load("data/data.rda")

source("ui/sidebar.R", local = T)
source("ui/body.R", local = T)

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
    
    for (script in list.files("server", full.names = TRUE)){
        source(script, local = T)
    }
    
    shinyjs::addClass(class="nav-justified", selector = ".nav")
    shinyjs::removeClass(class="btn-default", selector = "#cli_rmd1")
    shinyjs::removeClass(class="btn-default", selector = "#cli_rmd2")
    
    for(tabName in c("lpd", "fct", "imb", "cli", "diet")){
        navTabClassHandler(tabName = tabName, input = input)
    }
    
}

# Run the application 
shinyApp(ui = ui, server = server)

