output$VarsInput = renderUI({
    if(input$sidebar == "lpd"){
        tagList(
            selectInput(
                'lpd_level', "Lipid class, species, or summarized values",
                choices = names(data$lpd), selected = names(data$lpd)[1]
            ),
            uiOutput('lpdNormSelect')
        )
    } else if (input$sidebar == "diet") {
        tagList(
            selectInput(
                'diet_level', "Use raw measurements or percentage?",
                choices = names(data$diet), selected = names(data$diet)[1]
            )
        )
    }
})

# lpd norm method selector
output$lpdNormSelect = renderUI({
    choices = names(data$lpd[[input$lpd_level]])
    selectInput(
        "lpd_norm", "Select a Normalization Method",
        choices = choices, selected = choices[1]
    )
})

