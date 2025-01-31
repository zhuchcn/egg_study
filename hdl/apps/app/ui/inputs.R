levelSelector = function(type){
    if(type == "lpd"){
        label = "Lipid class, feature, or summarize?"
    }else if(type == "diet"){
        label = "Use raw measures or percentage?"
    }
    selectInput(inputId = paste0(type, ".level"), 
                label = label,
                choices = names(data[["data"]][[type]]),
                selected = names(data[["data"]][[type]])[1])
}

methodSelector = function(type, choices){
    selectInput(inputId = paste0(type, ".method"), 
                label = "Select a Correlation Method",
                choices = choices,
                selected = choices[1])
}

output$VarsInput = renderUI({
    type = str_split_fixed(input$sidebar, "_", n = 2)[1]
    
    if(type == "lpd") {
        choices = names(data[["corr"]][[type]][["fct"]][["class"]][[1]])
        tagList(
            levelSelector(type),
            uiOutput("lpdNormSelector"),
            methodSelector(type, choices)
        )
    }else if(type == "imb") {
        choices = names(data[["corr"]][["imb"]][["fct"]])
        tagList(
            methodSelector(type, choices)
        )
    }else if(type == "fct") {
        choices = names(data[["corr"]][["fct"]][["fct"]])
        tagList(
            methodSelector(type, choices)
        )
    }else if(type == "diet") {
        tagList(
            levelSelector(type)
        )
    }
})

output$lpdNormSelector = renderUI({
    text = 
        if(input$lpd.level != "summarize") "How to normalize the data?"
    else "Which summarized lipidome data?"
    choices = names(data[["data"]][["lpd"]][[input$lpd.level]])
    selectInput("lpd.norm", text, choices = choices, selected = choices[1])
})