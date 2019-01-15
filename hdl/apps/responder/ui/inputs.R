output$VarsInput = renderUI({
    if(input$sidebar == "lpd"){
        tagList(
            selectInput(
                "responders","Select Responders",
                choices = levels(data$fct$sample_table$Subject),
                selected = c(101, 104, 105, 107, 108, 110, 111, 113, 114, 119, 123, 124),
                multiple = TRUE,
                selectize = TRUE
            ),
            selectInput(
                'lpd_level', "Lipid class, species, or summarized values",
                choices = names(data$lpd), selected = names(data$lpd)[1]
            ),
            uiOutput('lpdNormSelect')
        )
    } else if (input$sidebar == "diet") {
        tagList(
            selectInput(
                "responders","Select Responders",
                choices = levels(data$fct$sample_table$Subject),
                selected = c(101, 104, 105, 107, 108, 110, 111, 113, 114, 119, 123, 124),
                multiple = TRUE,
                selectize = TRUE
            ),
            selectInput(
                'diet_level', "Use raw measurements or percentage?",
                choices = names(data$diet), selected = names(data$diet)[1]
            )
        )
    } else {
        tagList(
            selectInput(
                "responders","Select Responders",
                choices = levels(data$fct$sample_table$Subject),
                selected = c(101, 104, 105, 107, 108, 110, 111, 113, 114, 119, 123, 124),
                multiple = TRUE,
                selectize = TRUE
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

