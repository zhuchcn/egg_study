levelSelector = function(type){
    if(type == "lpd") label = "Lipid class, feature, or summarize?"
    if(type == "prt") label = "Select a Proteome Normalization Method"
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
    assayType = substr(input$sidebar, 1, 3)
    corrMethodChoices = c("pearson", "spearman", "kendall")
    
    if(assayType == "prt") {
        if(input$sidebar == "prt_lpd"){
            return(
                tagList(
                    levelSelector(assayType),
                    methodSelector(assayType, corrMethodChoices),
                    uiOutput("prtLpdLevel"),
                    uiOutput("prtLpdNorm"),
                    uiOutput("prtLpdSelect")
                )
            )
        } else if(input$sidebar == "prt_fct"){
            return(
                tagList(
                    levelSelector(assayType),
                    methodSelector(assayType, corrMethodChoices),
                    selectInput("prt_fct", "Select a Function Variable",
                                choices = featureNames(data$data$fct),
                                selected = featureNames(data$data$fct)[1])
                )
            )
        } else{
            return(
                tagList(
                    levelSelector(assayType),
                    methodSelector(assayType, corrMethodChoices)
                )
            )
        }
    }else if(assayType == "lpd") {
        return(
            tagList(
                levelSelector(assayType),
                uiOutput("lpdNormSelect")
            )
        )
    }else if(assayType == "fct") {
        return()
    }
})


## Define the input panels for Proteome
output$prtLpdLevel = renderUI({
    selectInput("prt.lpd_level", "Lipid Class, Species, or Summarized?",
                choices = names(data$data$lpd), 
                selected = names(data$data$lpd)[1])
})
output$prtLpdNorm = renderUI({
    selectInput("prt.lpd_norm", "Select the Normalization Method for Lipidome",
                choices = names(data$data$lpd[[input$prt.lpd_level]]),
                selected = names(data$data$lpd[[input$prt.lpd_level]])[1])
})
output$prtLpdSelect = renderUI({
    selectInput("prt_lpd", "Select a Lipidomics Variable",
                choices = featureNames(data$data$lpd[[input$prt.lpd_level]][[input$prt.lpd_norm]]),
                selected = featureNames(data$data$lpd[[input$prt.lpd_level]][[input$prt.lpd_norm]])[1])
})
output$lpdNormSelect = renderUI({
    choices = names(data$data$lpd[[input$lpd.level]])
    selectInput("lpd.norm", "Select a normalization method",
                choices = choices, selected = choices[1])
})