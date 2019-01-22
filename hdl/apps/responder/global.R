# This function returns a observer to change the color of a given tab
navTabClassHandler = function(tabName, input){
    selector1 = glue("a[data-value={tabName}_model1]")
    selector2 = glue("a[data-value={tabName}_model2")
    return(
        observe({ 
            if (input[[paste0(tabName, "_tab")]] == paste0(tabName, "_model1") ) {
                shinyjs::addClass(class="btn btn-primary", selector = selector1)
                shinyjs::removeClass(class="btn-warning", selector = selector1)
                shinyjs::addClass(class="btn btn-warning", selector = selector2)
                shinyjs::removeClass(class="btn-primary", selector = selector2)
            } else if (input[[paste0(tabName, "_tab")]] == paste0(tabName, "_model2")) {
                shinyjs::addClass(class="btn btn-primary", selector = selector2)
                shinyjs::removeClass(class="btn-warning", selector = selector2)
                shinyjs::addClass(class="btn btn-warning", selector = selector1)
                shinyjs::removeClass(class="btn-primary", selector = selector1)
            }
        })
    )
}

## Get the data for model 1
getData1 = function(input, data, type){
    reactive({
        data = data[[type]]
        if(type == "lpd"){
            data = data[[input$lpd_level]][[input$lpd_norm]]
        } else if (type == "diet") {
            data = data[[input$diet_level]]
        }
        responders = ifelse(data$sample_table$Subject %in% input$responders, 
                            "Responder", "Non-responder")
        responders = factor(responders, levels = c("Non-responder", "Responder"))
        names(responders) = data$sample_table$Subject
        mset = data
        if(type == "fct"){
            mset = subset_features(mset, 2:9)
        }
        mset$sample_table$Responder = responders[as.character(mset$sample_table$Subject)]
        mset
    })
}

## Fit model 1
fitStatModel1 = function(mset, type){
    mset =  t(mset$conc_table) %>%
        as.data.frame() %>%
        cbind(mset$sample_table[,c("Subject", "Timepoint", "Treatment", "Responder")]) %>%
        melt(id.vars=c("Subject", "Treatment", "Timepoint", "Responder"),
             variable.name = "Lipid") %>%
        dcast(Subject + Treatment + Responder + Lipid ~ Timepoint) %>%
        mutate(value = 
                   if(type %in% c("lpd", "fct")) post - pre
                   else Post - Pre
               ) %>%
        dcast(Subject + Responder + Lipid ~ Treatment, value.var = "value") %>%
        mutate(value = egg - sub) %>%
        dcast(Subject + Responder ~ Lipid, value.var = "value")
    rownames(mset) = mset$Subject
    mset = MultxSet(
        conc_table = conc_table(t(mset[,-(1:2)])),
        sample_table = sample_table(mset[,1:2])
    )
    design = model.matrix(data = as(mset$sample_table, "data.frame"),~ Responder)
    mSet_limma(mset, design, coef = 2, p.value = 2) %>%
        rownames_to_column("feature") %>%
        arrange(pvalue) %>%
        column_to_rownames("feature")
}

## Fit model 2
fitStatModel2 = function(mset, type){
    df = mset$conc_table %>%
        t %>% as.data.frame %>%
        cbind(mset$sample_table[,c("Subject", "Timepoint", "Responder")]) %>%
        melt(id.vars = c("Subject", "Timepoint", "Responder")) %>%
        dcast(Subject + variable + Responder ~ Timepoint) %>%
        mutate(value = 
                   if (type %in% c("lpd", "fct")) post - pre
                   else Post - Pre
               ) %>%
        dcast(Subject + Responder ~ variable)
    
    edata = df[,-2] %>%
        column_to_rownames("Subject") %>% t %>%
        conc_table
    
    pdata = df[,1:2]
    rownames(pdata) = pdata$Subject
    pdata = sample_table(pdata)
    
    mset = LipidomicsSet(conc_table = edata, sample_table = pdata)
    
    design = model.matrix(~ Responder, data = as(mset$sample_table, "data.frame"))
    mSet_limma(mset, design, coef = 2, p.value = 2) %>%
        rownames_to_column("feature") %>%
        arrange(pvalue) %>%
        column_to_rownames("feature")
}

