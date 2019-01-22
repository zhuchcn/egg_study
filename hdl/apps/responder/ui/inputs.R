output$VarsInput = renderUI({
    tagList(
        selectInput(
            "responders","Select Responders",
            choices = levels(data$fct$sample_table$Subject),
            selected = as.character(c(101, 104, 105, 107, 108, 110, 111, 113, 114, 119, 123, 124)),
            multiple = TRUE,
            selectize = TRUE
        ),
        if(input$sidebar == "lpd"){
            tagList(
                selectInput(
                    'lpd_level', "Lipid class, species, or summarized values",
                    choices = names(data$lpd), selected = names(data$lpd)[1]
                ),
                uiOutput('lpdNormSelect')
            )
        } else if (input$sidebar == "diet") {
            selectInput(
                'diet_level', "Use raw measurements or percentage?",
                choices = names(data$diet), selected = names(data$diet)[1]
            )
        } else {
            selectInput(
                "responders","Select Responders",
                choices = levels(data$fct$sample_table$Subject),
                selected = c(101, 104, 105, 107, 108, 110, 111, 113, 114, 119, 123, 124),
                multiple = TRUE,
                selectize = TRUE
            )
        }
    )
})

# lpd norm method selector
output$lpdNormSelect = renderUI({
    choices = names(data$lpd[[input$lpd_level]])
    selectInput(
        "lpd_norm", "Select a Normalization Method",
        choices = choices, selected = choices[1]
    )
})

