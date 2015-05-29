
## ui.fameBrowser.datasetname <- file.path("data", "data_init", "eoa.db")

Sys.setenv(FAME="/opt/fame/")

## rm(usefame)
## usefame <- tryCatch(fameStart(), error = function(e) FALSE)
## if (usefame) fameStart()
fameStart()

output$uiFb_aceEditor <- renderUI({

    if (is.null(input$fameBrowser_uploadFile$datapath) || input$fameBrowser_uploadFile$datapath == 0) {
      ## filepath <- "/home/z930/Dropbox/GitHub/desk/inst/industry/data/data_init"
      filepath <- file.path("data", "data_init")
      filename <- "eoa.db"
    } else {
      filepath <- dirname(input$fameBrowser_uploadFile$datapath[1])
      filename <- input$fameBrowser_uploadFile$name[1]
    }
    mydb <- file.path(filepath, filename)

    ## input <- NULL
    ## input$fameBrowser_listSeries <- 10
    ## input$fameBrowser_filterSeries <- ".cbd"
    fameseries <- fameWildlist(mydb, nMax = input$fameBrowser_listSeries)$name

    ## require(stringr)
    ## ?str_match
    ## fameseries[!is.na(str_match(fameseries, "cbrd"))]
    ## fameseries[!is.na(str_match(fameseries, "afm"))]
    fameseries <- fameseries[!is.na(str_match(fameseries, input$fameBrowser_filterSeries))]
    
    ## fameseries <- fameseries[substr(fameseries,
    ##                                 1,
    ##                                 nchar(input$fameBrowser_filterSeries)) == input$fameBrowser_filterSeries]


    
    ## X <- strsplit(fameseries, split = "[.]")
    ## fameseries <- fameseries[sapply(X, length)!=1]
    ## fameseries

    

      command.string <- paste0(
        c(
          'TRY',
          paste0('CLOSE ', filename),
          'END TRY',
          paste0('CD "', filepath, '"'),
          paste0('OPEN ', filename)
          ),
        collapse = '\n')

    list(    
      selectInput("famebrowser_selseries", "Select FAME series:", choices = fameseries, selected = fameseries[1], multiple = TRUE)
      ,
      aceEditor("famebrowser_aceeditor", mode = "r", height = "400px", value = command.string)
      )

  })


output$ui_fameBrowser <- renderUI({

    list(

        sliderInput(inputId = "fameBrowser_viz_plot_height", label = "Height:", min = 0, max = 1000, value = 0, step = 50),
        sliderInput(inputId = "fameBrowser_viz_plot_width", label = "Width:", min = 400, max = 1200, value = 850, step = 50),

      shinysky::actionButton("famebrowser_commandSendButton", "Submit Command", styleclass="success",icon = NULL, size = "large", block = TRUE),
      fileInput("fameBrowser_uploadFile", "Open FAME database file:"),
      numericInput("fameBrowser_listSeries", "List n FAME series:", value = 1000),
      textInput("fameBrowser_filterSeries", "Filter FAME series:", value = "afm"),
      uiOutput("uiFb_aceEditor"),
        helpAndReport("FAME Browser", "famebrowser", inclMD(file.path("tools", "help", "fameBrowser.md")))
    )

})

## input <- list(fameBrowser_uploadFile = data.frame(name = "eoa.db", size = 36, type = NA, datapath = "/tmp/RtmpxWxner/f9cbbfd599ac1632e59c83b3/0", stringsAsFactors = FALSE))

observe({
  if (is.null(input$fameBrowser_uploadFile$datapath) || input$fameBrowser_uploadFile$datapath == 0) return()

  file.rename(input$fameBrowser_uploadFile$datapath[1],
              file.path(dirname(input$fameBrowser_uploadFile$datapath[1]),
                        input$fameBrowser_uploadFile$name[1]))
  
})

fameBrowser_widthSize <- reactive({
    ifelse(is.null(input$fameBrowser_viz_plot_width), return(values$plotWidth), return(input$fameBrowser_viz_plot_width))
})
fameBrowser_heightSize <- reactive({
    ifelse(is.null(input$fameBrowser_viz_plot_height), return(values$plotHeight), return(input$fameBrowser_viz_plot_height))
})

output$fameBrowser <- renderUI({
  statTabPanel(menu_name = "API",
               fun_name = "FAME Browser",
               rfun_label = ".fameBrowser",
               fun_label = "fameBrowser",
               fun_tabs = c("Plots", "dygraph"),
               widthFun = "fameBrowser_widthSize",
               heightFun = "fameBrowser_heightSize"
               )
})


.fameBrowser <- reactive({

    fameBrowser(
      famebrowser_aceeditor = input$famebrowser_aceeditor,
      famebrowser_commandSendButton = input$famebrowser_commandSendButton,
      famebrowser_selseries = input$famebrowser_selseries,
      fameBrowser_uploadFile = input$fameBrowser_uploadFile,
      fameBrowser_viz_plot_width = input$fameBrowser_viz_plot_width,
      fameBrowser_viz_plot_height = input$fameBrowser_viz_plot_height
        )
})


fameBrowser <- function(
  famebrowser_aceeditor = famebrowser_aceeditor,
  famebrowser_commandSendButton = famebrowser_commandSendButton,
  famebrowser_selseries = famebrowser_selseries,
  fameBrowser_uploadFile = fameBrowser_uploadFile,
  fameBrowser_viz_plot_width = fameBrowser_viz_plot_width,
  fameBrowser_viz_plot_height = fameBrowser_viz_plot_height
  ) {

    if(famebrowser_commandSendButton == 0) {
        capture <- NULL
    } else {
        isolate({
            capture <- fameCommand(famebrowser_aceeditor, silent = F, capture = T)
        })
    }

    if (is.null(input$fameBrowser_uploadFile$datapath) || input$fameBrowser_uploadFile$datapath == 0) {
      ## filepath <- "/home/z930/Dropbox/GitHub/desk/inst/industry/data/data_init"
      filepath <- file.path("data", "data_init")
      filename <- "eoa.db"
    } else {
      filepath <- dirname(input$fameBrowser_uploadFile$datapath[1])
      filename <- input$fameBrowser_uploadFile$name[1]
    }
    mydb <- file.path(filepath, filename)

    ## series.all <- c("afm.bsiid", "afm.cbd")
    outseries <- NULL
    ## series <- series.all[1]
    for (series in famebrowser_selseries) {
      getfame(c(fameseries = series), db = mydb, save = TRUE)
      fameseries.ts <- as.ts(fameseries)
      fameseries.xts <- as.xts(fameseries.ts)
      outseries <- cbind(outseries, fameseries.xts)
    }
    names(outseries) <- famebrowser_selseries

    
    return(
        list(
          capture = capture,
          outseries = outseries,
          fameBrowser_uploadFile = fameBrowser_uploadFile,
            fameBrowser_viz_plot_width = fameBrowser_viz_plot_width,
            fameBrowser_viz_plot_height = fameBrowser_viz_plot_height
        )
    )
}

summary_fameBrowser <- function(result = .fameBrowser()) {
    if (length(result) > 0) {

      capture <- result$capture
      fameBrowser_uploadFile <- result$fameBrowser_uploadFile

        list.print <- NULL

        ## list.print <- fameCommand("type 100", silent = F, capture = T)
        list.print <- capture

        list.print <- list(list.print, fameBrowser_uploadFile)

        return(list.print)
    }
}

plots_fameBrowser <- function(result = .fameBrowser()) {
    if (length(result) > 0) {

    }
}

dygraphs_fameBrowser <- function(result = .fameBrowser()) {
  if (length(result) > 0) {

    outseries <- result$outseries
    
    d1 <- dygraph(outseries)

    return(d1)

    }
}

      ## command.string <- paste0(c(
      ##   'TRY',
      ##   'CLOSE eoa.db',
      ##   'END TRY',
      ##   ## 'CD "/media/sf_Dropbox/GitHub/desk/inst/industry/"',
      ##   'CD "/home/z930/Dropbox/GitHub/desk/inst/industry/"',
      ##   'type @CWD',
      ##   'CD "data"',
      ##   'type @CWD',
      ##   'CD "data_init"',
      ##   'type @CWD',
      ##   'OPEN eoa.db',
      ##   'DISPLAY FRA.NLG'),
      ##                          collapse = '\n')
