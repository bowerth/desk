###############################
## tool
## additionally: see comments in "random.R"
###############################
output$uiTool_var <- renderUI({
    ## variable selection
    vars <- varnames()
    isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
    vars <- vars[isNum]
    if(length(vars) == 0) return()
    selectInput(inputId = "tool_var", label = "Variable (select one):", choices = vars,
                selected = state_singlevar("tool_var",vars), multiple = FALSE)
})

output$ui_tool <- renderUI({
    ## for ui
    list(
  	wellPanel(
            uiOutput("uiTool_var")
            ),
        helpAndReport("The tool",                          # title
                      "tool",                              # link
                      includeHTML("tools/help/tool.html")) # content: shiny function to include html into a Shiny UI
 	)
})

output$tool <- renderUI({
  ## for input-output
  ## radiant.R
  statTabPanel("Menu", # menu_name: sidebarPanel: HTML(paste("<label><strong>Menu:",menu_name,"</strong></label>")),
               "The tool", # fun_name: mainPanel: "statPanel(fun_name..."
               ".tool",               # rfun_label: mainPanel:
               ## ## Generate output for the summary and plots tabs
               ## output[[sum_name]] <- renderPrint({ result <- get(rfun_label)()
               ## output[[plot_name]] <- renderPlot({ result <- get(rfun_label)()
               "tool",                # fun_label: sidebarPanel: "uiOutput(paste0("ui_",fun_label))..."
               widthFun = "plotWidth",
               heightFun = "plotHeight")
  ## can insert custom functions for plotWidth and plotHeight
})

.tool <- reactive({
  ## reactive that calls the function for main analysis
  ## . used to indicate this is an 'internal' function
  tool()
})

## tools/app/report.R
observe({
    if(is.null(input$toolReport) || input$toolReport == 0) return()
    isolate({
        inp <- list(input$..., )
        updateReport(inp,"tool")     # updating the report when called
    })
})

tool <- function(datasets, ..., rmd = "") {
  ## function for main analysis

  ...

  result <- list()
  if(rmd == "Summary") return(summary_tool(result))
  if(rmd == "Plots") return(plots_tool(result))
  return(result)
}


## Generate output for the summary tab
## radiant.R
## sum_name <- paste0("summary_", fun_label)
## tabPanel("Summary", verbatimTextOutput(sum_name)),
summary_tool <- function(result = .tool()) {

}

## Generate output for the plots tab
## radiant.R
## plot_name <- paste0("plots_", fun_label)
## tabPanel("Plots", conditionalPanel(condition="!$('html').hasClass('shiny-busy')", plotOutput(plot_name, height = "100%")))
plots_tool <- function(result = .tool()) {

}
