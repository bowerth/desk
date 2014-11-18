
## library(RCurl)
## library(d3Network)

# Load data once
## ui.icionet.URL <- "https://raw.githubusercontent.com/christophergandrud/d3Network/master/JSONdata/miserables.json"
## ui.icionet.MisJson <- getURL(ui.icionet.URL, ssl.verifypeer = FALSE)
ui.icionet.file <- file.path("data", "data_init", "miserables.json")

# Convert JSON arrays into data frames
## ui.icionet.MisLinks <- JSONtoDF(jsonStr = ui.icionet.MisJson, array = "links")
ui.icionet.MisLinks <- JSONtoDF(jsonStr = NULL, file = ui.icionet.file, array = "links")
## ui.icionet.MisNodes <- JSONtoDF(jsonStr = ui.icionet.MisJson, array = "nodes")
ui.icionet.MisNodes <- JSONtoDF(jsonStr = NULL, file = ui.icionet.file, array = "nodes")




output$ui_icioNet <- renderUI({

    ## doLogin()
    ## if (loginData$LoggedIn) {

    list(

        conditionalPanel(condition = "input.tabs_icioNet == 'D3'",
                         wellPanel(
                             checkboxInput("icioNet_viz_plot_controls", "Plot options", FALSE),
                             conditionalPanel(condition = "input.icioNet_viz_plot_controls == true",
                                              ## htmlOutput("ui_plot_options"),
                                              sliderInput(inputId = "icioNet_viz_plot_width", label = "Width:", min = 400, max = 1200, value = 800, step = 50),
                                              conditionalPanel(condition = "input.tabs_icioNet == 'D3'",
                                                               sliderInput(inputId = "icioNet_viz_plot_height", label = "Height:", min = 300, max = 1200, value = 600, step = 50)
                                                               )
                                              )
                         )
                         )
       ,
        sliderInput("icionet_opacity", label = "Choose node opacity",
                    min = 0, max = 1, step = 0.01, value = 0.5
                    )
       ,
        helpAndReport("ICIO Networks", "icioNet", inclMD(file.path("tools", "help", "icioNet.md")))
    ) # list(...

})


icioNet_widthSize <- reactive({
    ifelse(is.null(input$icioNet_viz_plot_width), return(values$plotWidth), return(input$icioNet_viz_plot_width))
})
icioNet_heightSize <- reactive({
    ifelse(is.null(input$icioNet_viz_plot_height), return(values$plotHeight), return(input$icioNet_viz_plot_height))
})

output$icioNet <- renderUI({
  ## for input-output
  statTabPanel(menu_name = "ICIO", # menu_name: for side bar - coincide with navbarMenu
               fun_name = "ICIO Networks",           # fun_name
               rfun_label = ".icioNet",         # rfun_label
               fun_label = "icioNet"           # fun_label
               ,fun_tabs = c("HTML") # , "NVD3Charts") #, "ggVis") # , "Tables", "Maps")
               ,widthFun = "icioNet_widthSize"
               ,heightFun = "icioNet_heightSize"
               )
})


.icioNet <- reactive({

    icioNet(
        icionet_opacity = input$icionet_opacity,
        icioNet_viz_plot_width = input$icioNet_viz_plot_width,
        icioNet_viz_plot_height = input$icioNet_viz_plot_height
        )
})


icioNet <- function(
    icionet_opacity = icionet_opacity,
    icioNet_viz_plot_width = icioNet_viz_plot_width,
    icioNet_viz_plot_height = icioNet_viz_plot_height
    ) {

    return(list(icionet_opacity = icionet_opacity,
                icioNet_viz_plot_width = icioNet_viz_plot_width,
                icioNet_viz_plot_height = icioNet_viz_plot_height
                )
           )
}

summary_icioNet <- function(result = .icioNet()) {
    if (length(result) > 0) {

        list.print <- NULL

        icionet_opacity <- result$icionet_opacity
        icioNet_viz_plot_width = result$icioNet_viz_plot_width
        icioNet_viz_plot_height = result$icioNet_viz_plot_height

        list.print <- c(list.print,
                        list(Opacity = icionet_opacity,
                             Width = icioNet_viz_plot_width,
                             Height = icioNet_viz_plot_height)
                        )
        return(list.print)
    }
}

html_icioNet <- function(result = .icioNet()) {
    if (length(result) > 0) {

    icionet_opacity = ifelse(is.null(result$icionet_opacity), 0.5, result$icionet_opacity)
    icioNet_viz_plot_width <- isolate(icioNet_widthSize())
    icioNet_viz_plot_height <- isolate(icioNet_heightSize())

        d3ForceNetwork(Nodes = ui.icionet.MisNodes,
                       Links = ui.icionet.MisLinks,
                       Source = "source",
                       Target = "target",
                       Value = "value",
                       NodeID = "name",
                       Group = "group",
                       ## width = 400,
                       ## height = 500,
                       width = icioNet_viz_plot_width,
                       height = icioNet_viz_plot_height,
                       opacity = icionet_opacity,
                       standAlone = FALSE,
                       parentElement = '#html_icioNet' # needs to be function name!!
                       )

    }
}
