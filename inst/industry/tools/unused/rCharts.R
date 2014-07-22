## ## ############################
## ## rCharts plots
## ## ############################

## ## create parts of the "ui_random"
## output$uiRnd_var <- renderUI({
##     vars <- varnames()
##     isChar <- "character" == getdata_class()
##     vars <- vars[isChar]
##     if(length(vars) == 0) return()
##     selectInput(inputId = "rnd_var", label = "Variable (select one):", choices = vars,
##                 selected = state_singlevar("rnd_var",vars), multiple = FALSE)
## })

## output$uiRnd_block <- renderUI({
##     vars <- varnames()
##     isFct <- "factor" == getdata_class()
##     vars <- vars[isFct]
##     if(length(vars) == 0) return()
##     vars <- c("None",vars)
##     selectInput(inputId = "rnd_block", label = "Block variable (select one):", choices = vars,
##                 selected = state_singlevar("rnd_block",vars), multiple = FALSE)
## })

## ## define choice for radio button in "ui_random"
## rnd_sample <- c("Sample" = "sample", "Assign" = "assign")

## output$ui_random <- renderUI({
##     list(
##   	wellPanel(
##             uiOutput("uiRnd_var"),
##             radioButtons(inputId = "rnd_sample", label = "", rnd_sample,
##                          selected = state_init_list("rnd_sample","sample", rnd_sample)),
##             conditionalPanel(condition = "input.rnd_sample == 'sample'",
##                              numericInput("rnd_sample_size", "Sample size:", min = 1,
##                                           value = state_init("rnd_sample_size",1))
##                              ),
##             conditionalPanel(condition = "input.rnd_sample != 'sample'",
##                              numericInput("rnd_nrCond", "Number of conditions:", min = 2,
##                                           value = state_init("rnd_nrCond",2)),
##                              uiOutput("uiRnd_block"),
##                              actionButton("rnd_save_treatment", "Save treatment")
##                              )
##             ),
##         helpAndReport('Random','random',inclMD("tools/help/random.md"))
##  	)
## })
