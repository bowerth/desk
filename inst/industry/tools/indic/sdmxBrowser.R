#######################################
## Shiny interface for SDMX
## naming conventions
## UI variables: ui.sdmxBrowser.[] -> all possible choices
## rendered UIs: output$uisB_[]
#######################################

ui.sdmxBrowser.col <- c("#4F81BD", "#C0504D", "#9BBB59", "#8064A2", "#4BACC6", "#F79646")
ui.sdmxBrowser.year <- c(1970, as.numeric(as.character((format(Sys.time(), "%Y")))) + 2)

## ## create list with flows by provider
## ui.sdmxbrowser_provider <- getProviders()
## toString(ui.sdmxbrowser_provider)
##
## for (provider in ui.sdmxbrowser_provider) {
##     flows <- getFlows(provider)
##     names <- names(flows)
##     df <- data.frame(ID = names, Label = unlist(flows))
##     table <- kable(df, format = "markdown", row.names = FALSE, output = FALSE)
##     cat(paste0('\n', provider, '\n\n'))
##     cat(table, sep = '\n')
## }
##
## ui.sdmxBrowser.flows.list <- NULL
## ## provider <- "IMF"
## ## p <- 1
## for (p in seq(along = ui.sdmxbrowser_provider)) {
##     provider <- sort(ui.sdmxbrowser_provider)[p]
##     flows <- sort(names(getFlows(provider)))
##     ## ISTAT contains no flows
##     ## if (length(flows) > 0) {
##         flows <- list(flows)
##         names(flows) <- provider
##         ui.sdmxBrowser.flows.list <- c(ui.sdmxBrowser.flows.list, flows)
##     ## }
## }
## save(ui.sdmxBrowser.flows.list, file = "data/data_init/sdmxBrowser.rda")

load("data/data_init/sdmxBrowser.rda")

##
output$uisB_provider <- renderUI({
    ui.sdmxbrowser_provider <- getProviders()
    selectInput("sdmxbrowser_provider", "Provider:", ui.sdmxbrowser_provider,
                ## selected = state_init_list("sdmxbrowser_provider","EUROSTAT", ui.sdmxbrowser_provider),
                selected = "OECD",
                multiple = FALSE)
})
##
output$uisB_flow <- renderUI({

    sdmxbrowser_provider <- input$sdmxbrowser_provider
    if (input$sdmxbrowser_flow_updateButton != 0) {
        load("data/data_init/sdmxBrowser.rda")
    }
    ui.sdmxbrowser_flow <- ui.sdmxBrowser.flows.list[[sdmxbrowser_provider]]
    selectInput("sdmxbrowser_flow", "Flow:", c("", ui.sdmxbrowser_flow),
                selected = "", multiple = FALSE)
})
##
.sdmxbrowser_dimensions_all <- reactive({
    sdmxbrowser_dimensions_all<- names(getDimensions(input$sdmxbrowser_provider,
                                                     input$sdmxbrowser_flow))
    return(sdmxbrowser_dimensions_all)
})
##
output$uisB_dimensions <- renderUI({

    if (input$sdmxbrowser_flow=="") return()

    sdmxbrowser_dimensions_all <- .sdmxbrowser_dimensions_all()
    selectInput("sdmxbrowser_dimensions", "Filter Dimensions:", sdmxbrowser_dimensions_all,
                ## selected = state_multvar("sdmxbrowser_dimensions", sdmxbrowser_dimensions_all),
                selected = sdmxbrowser_dimensions_all,
                multiple = TRUE, selectize = FALSE)
})
##
.sdmxbrowser_dimensioncodes <- reactive({
    sdmxbrowser_dimensions <- input$sdmxbrowser_dimensions
    sdmxbrowser_dimensioncodes <- as.list(sdmxbrowser_dimensions)
    sdmxbrowser_dimensioncodes <- sapply(sdmxbrowser_dimensioncodes,function(x) NULL)
    names(sdmxbrowser_dimensioncodes) <- sdmxbrowser_dimensions
    return(sdmxbrowser_dimensioncodes)
})
##
output$uisB_dimensioncodes <- renderUI({
    sdmxbrowser_provider <- input$sdmxbrowser_provider
    sdmxbrowser_flow <- input$sdmxbrowser_flow
    sdmxbrowser_dimensions <- input$sdmxbrowser_dimensions

    if (sdmxbrowser_flow!="") {

        sdmxbrowser_dimensioncodes <- .sdmxbrowser_dimensioncodes()
        command.all <- NULL
        for (d in seq(along = sdmxbrowser_dimensions)) {
            ## get all codes
            sdmxbrowser_dimensioncodes[[d]] <- names(getCodes(sdmxbrowser_provider,
                                                              sdmxbrowser_flow,
                                                              sdmxbrowser_dimensions[d]))

            sdmxbrowser_dimensioncodes[[d]] <- sort(sdmxbrowser_dimensioncodes[[d]])

            command <- paste0('selectInput("sdmxbrowser_dimensioncodes_', d,
                              '", "Select ', sdmxbrowser_dimensions[d],
                              ':", c("',
                              gsub(', ', '", "', toString(sdmxbrowser_dimensioncodes[[d]]))
                              ,
                              '"), selected = "', sdmxbrowser_dimensioncodes[[d]][1], '", multiple = TRUE, selectize = TRUE)')
            command.all <- paste(command.all, command, sep = ",")
        }
        command.all <- sub(",", "", command.all)
        eval(parse(text = paste0('list(', command.all, ')')))

    } else {
        return(h5("Please select data flow and submit query"))
    }

})
##
output$uisB_query <- renderUI({
    sdmxbrowser_flow <- input$sdmxbrowser_flow
    sdmxbrowser_dimensions <- input$sdmxbrowser_dimensions # selected dimensions

    if (sdmxbrowser_flow!="") {

        sdmxbrowser_dimensioncodes <- .sdmxbrowser_dimensioncodes()
        for (d in seq(along = sdmxbrowser_dimensions)) {
            eval(parse(text = paste0('sdmxbrowser_dimensioncodes[[', d, ']] <- input$sdmxbrowser_dimensioncodes_', d)))
        }
        sdmxbrowser_dimensions_all<- .sdmxbrowser_dimensions_all()
        dimequal <- match(sdmxbrowser_dimensions_all, sdmxbrowser_dimensions)
        query <- sdmxbrowser_flow
        for (d in seq(along = dimequal)) {
            if (is.na(dimequal[d])) {
                query.part <- "*"
            } else {
                query.part <- gsub(", ", "+", toString(sdmxbrowser_dimensioncodes[[dimequal[d]]]))
            }
            query <- paste(query, query.part, sep = ".")
        }
        textInput("sdmxbrowser_query", "SDMX Query:",
                  ## state_init_list("sdmxbrowser_query","nama_nace64_c.A.MIO_NAC.TOTAL.B1G.AT", query), # will prevent query from updating upon change of dimension selection
                  query)

    } else {
        return(h5("Please select data flow and submit query"))
    }

})

output$ui_sdmxBrowser <- renderUI({

    ## doLogin()
    ## if (loginData$LoggedIn) {

  list(
    conditionalPanel(condition = "input.tabs_sdmxBrowser!='DataTables'",
                     wellPanel(
                       checkboxInput("sdmxbrowser_viz_plot_controls", "Plot options", FALSE),
                       conditionalPanel(condition = "input.sdmxbrowser_viz_plot_controls==true",
                                        ## htmlOutput("ui_plot_options"),
                                        sliderInput(inputId = "sdmxBrowser_viz_plot_height", label = "Height:", min = 400, max = 1000, value = 500, step = 50),
                                        sliderInput(inputId = "sdmxBrowser_viz_plot_width", label = "Width:", min = 400, max = 1200, value = 850, step = 50)
                                        )
                       )
                     )
    ,
    wellPanel(
      uiOutput("uisB_query"),
      ## actionButton("sdmxbrowser_querySendButton", "Send query"),
      shinysky::actionButton("sdmxbrowser_querySendButton", "Submit Query", styleclass="success",icon = NULL, size = "large", block = TRUE),
      helpText("Click button to retrieve values"),
      downloadButton('download_sdmxBrowser', 'Download CSV')
      ),
    wellPanel(
      h5("SDMX Query Builder"),
      uiOutput("uisB_provider"),
      uiOutput("uisB_flow"),
      actionButton("sdmxbrowser_flow_updateButton", "Update Flows"),
      wellPanel(
        uiOutput("uisB_dimensions"),
        uiOutput("uisB_dimensioncodes")
        ),
      sliderInput(inputId = "sdmxbrowser_yearStartEnd", label = "Period:",
                  min = 1970,
                  max = 2015,
                  value = c(2000, 2012),
                  format = "####")
      ),
    helpAndReport("SDMX Browser","sdmxBrowser",inclMD("tools/help/sdmxBrowser.md"))
    ) # list(...

    ## } else
    ## {
    ##     h3("Please log in")
    ## }

})

sdmxBrowser_widthSize <- reactive({
    ifelse(is.null(input$sdmxBrowser_viz_plot_width), return(values$plotWidth), return(input$sdmxBrowser_viz_plot_width))
})
sdmxBrowser_heightSize <- reactive({
    ifelse(is.null(input$sdmxBrowser_viz_plot_height), return(values$plotHeight), return(input$sdmxBrowser_viz_plot_height))
})

output$sdmxBrowser <- renderUI({
    ## for input-output
    statTabPanel(menu_name = "SDMX", # menu_name: for side bar - coincide with navbarMenu
                 fun_name = "SDMX Browser",   # fun_name
                 rfun_label = ".sdmxBrowser", # rfun_label
                 fun_label = "sdmxBrowser" # fun_label
                 ## ,rChart_lib = input$sdmxbrowser_rchartlib
                 ,fun_tabs = c("DataTables", "Plots") #, "MorrisCharts") # "Tables", "NVD3Charts")
                 ,widthFun = "sdmxBrowser_widthSize"
                 ,heightFun = "sdmxBrowser_heightSize"
                 )
})

.sdmxBrowser <- reactive({
    ## reactive that calls the function for main analysis
    ## . used to indicate this is an 'internal' function
    ##
    ## if (length(input$sdmxbrowser_dimS)==0) return ()
    ##
    sdmxBrowser(sdmxbrowser_provider = input$sdmxbrowser_provider
                ,
                sdmxbrowser_flow = input$sdmxbrowser_flow
                ,
                sdmxbrowser_query = input$sdmxbrowser_query
                ,
                sdmxbrowser_querySendButton = input$sdmxbrowser_querySendButton
                ,
                sdmxbrowser_yearStartEnd = input$sdmxbrowser_yearStartEnd
                ## ,
                ## sdmxBrowser_viz_plot_height = input$sdmxBrowser_viz_plot_height
                ## ,
                ## sdmxBrowser_viz_plot_width = input$sdmxBrowser_viz_plot_width
                )
})

observe({
    if (is.null(input$sdmxbrowser_flow_updateButton) || input$sdmxbrowser_flow_updateButton == 0) {
        return()
    } else {
        isolate({
            provider <- input$sdmxbrowser_provider
            flows <- sort(names(getFlows(provider)))
            ## flows <- list(flows)
            ## names(flows) <- provider
            ui.sdmxBrowser.flows.list[[provider]] <- flows
            load("data/data_init/sdmxBrowser.rda")
            save(ui.sdmxBrowser.flows.list, file = "data/data_init/sdmxBrowser.rda")
            print("saved flow information")
        })
    }
})

sdmxBrowser <- function(
    sdmxbrowser_provider = sdmxbrowser_provider
    ,
    sdmxbrowser_flow = sdmxbrowser_flow
    ,
    ## sdmxbrowser_dimensions = input$sdmxbrowser_dimensions
    ## ,
    sdmxbrowser_query = sdmxbrowser_query
    ,
    sdmxbrowser_querySendButton = sdmxbrowser_querySendButton
    ,
    sdmxbrowser_yearStartEnd = sdmxbrowser_yearStartEnd
    ## ,
    ## sdmxBrowser_viz_plot_height = sdmxBrowser_viz_plot_height,
    ## ,
    ## sdmxBrowser_viz_plot_width = sdmxBrowser_viz_plot_width
    )
{

  if (sdmxbrowser_flow=="") return(list(sdmxbrowser_flow = sdmxbrowser_flow))

    sdmxbrowser_dimensions_all <- .sdmxbrowser_dimensions_all()

    yearStart <- as.character(sdmxbrowser_yearStartEnd[1])
    yearEnd <- as.character(sdmxbrowser_yearStartEnd[2])

    ## if(is.null(sdmxbrowser_querySendButton) | sdmxbrowser_querySendButton == 0) {
    if(sdmxbrowser_querySendButton == 0) {
        isolate({
            queryData <- getSDMX(sdmxbrowser_provider, sdmxbrowser_query, start = yearStart, end = yearEnd)
        })
    } else {
        isolate({
            queryData <- getSDMX(sdmxbrowser_provider, sdmxbrowser_query, start = yearStart, end = yearEnd)
        })
    }

    ## sdmxbrowser_query <- "EXR.M.USD+GBP.EUR.SP00.A"
    ## queryData <- getTimeSeries('ECB', 'EXR.M.USD+GBP.EUR.SP00.A')
    ## queryData <- getTimeSeries('ECB', 'EXR.Q.USD+GBP.EUR.SP00.A')
    ## queryData <- getTimeSeries('ECB', 'EXR.A.USD+GBP.EUR.SP00.A')

##   getDimensions <- function(provider, dataflow) {
##   res <- J("it.bankitalia.reri.sia.sdmx.client.SdmxClientHandler")$getDimensions(provider, dataflow)
##   jlist <- .jcall(res,"[Ljava/lang/Object;","toArray");
##   res = convertDimList(jlist)
##   return(res)
## }

  ## require(devtools)
  ## load_all(file.path(dbpath, "GitHub", "RJSDMX"))
  ## ## install.packages(file.path(dbpath, "CRAN", "src", "contrib", "RJSDMX_0.1.tar.gz"), repos = NULL, type = "source")

  getDimensions("EUROSTAT", "DS-016890")
##   getCodes("EUROSTAT", "DS-016890", "FLOW")
##   getCodes("EUROSTAT", "DS-016890", "PRODUCT")

  all.codes.FLOW <- names(getCodes("EUROSTAT", "DS-016890", "FLOW"))

  all.codes.FREQ <- names(getCodes("EUROSTAT", "DS-016890", "FREQ"))
  all.codes.INDICATORS <- names(getCodes("EUROSTAT", "DS-016890", "INDICATORS"))

##   names(getCodes("EUROSTAT", "nama_nace64_c", "UNIT"))

##   ## all.codes.OBS_STATUS <- names(getCodes("EUROSTAT", "DS-016890", "OBS_STATUS"))

##   all.codes.PARTNER <- names(getCodes("EUROSTAT", "DS-016890", "PARTNER"))
##   all.codes.PRODUCT <- names(getCodes("EUROSTAT", "DS-016890", "PRODUCT"))
##   all.codes.REPORTER <- names(getCodes("EUROSTAT", "DS-016890", "REPORTER"))

##   for(i in seq(1,10)) {
##     query <- gsub(", ", ".", toString(c(sample(all.codes.FLOW, 1),
##                                         sample(all.codes.FREQ, 1),
##                                         sample(all.codes.INDICATORS, 1),
##                                         "",
##                                         sample(all.codes.PARTNER, 1),
##                                         sample(all.codes.PRODUCT, 1),
##                                         sample(all.codes.REPORTER, 1)
##                                         )))
##     ##
##     getTimeSeries("EUROSTAT", paste0("DS-016890.", query))
##   }

  ## ######################
  ## end testing
  ## ######################


    queryDataFreq <- frequency(queryData[[1]])

    queryData <- as.data.frame(queryData)
    queryData <- data.frame(time = rownames(queryData), queryData)
    queryData <- suppressWarnings(melt(queryData, id.vars = "time"))


  return(list(sdmxbrowser_provider = sdmxbrowser_provider
              ,
              sdmxbrowser_flow = sdmxbrowser_flow
              ,
              sdmxbrowser_dimensions_all = sdmxbrowser_dimensions_all
              ,
              sdmxbrowser_query = sdmxbrowser_query
              ,
              yearStart = yearStart
              ,
              yearEnd = yearEnd
              ,
              queryData = queryData
              ,
              queryDataFreq = queryDataFreq
              ## ,
              ## sdmxBrowser_viz_plot_height = sdmxBrowser_viz_plot_height,
              ## ,
              ## sdmxBrowser_viz_plot_width = sdmxBrowser_viz_plot_width
              )
         )
}

summary_sdmxBrowser <- function(result = .sdmxBrowser())
{ if (length(result) > 0) {

  sdmxbrowser_provider = result$sdmxbrowser_provider
  sdmxbrowser_flow = result$sdmxbrowser_flow
  sdmxbrowser_dimensions_all = result$sdmxbrowser_dimensions_all
  sdmxbrowser_query = result$sdmxbrowser_query
  yearStart = result$yearStart
  yearEnd = result$yearEnd

  queryData <- result$queryData

  if (sdmxbrowser_flow=="") return(cat("Please select data flow and submit query"))

  blurb <- paste(paste('Provider =', sdmxbrowser_provider),
                 paste('Flow =', sdmxbrowser_flow),
                 paste('Dimensions =', toString(sdmxbrowser_dimensions_all)),
                 paste('Query =', paste0(sdmxbrowser_query, ', start = ', yearStart, ', end = ', yearEnd)),
                 sep = '\n')

  return(cat(blurb))

  ## return data in Summary:
  ## list.print <- list(
  ##     Provider = sdmxbrowser_provider,
  ##     Flow = sdmxbrowser_flow,
  ##     Dimensions = toString(sdmxbrowser_dimensions_all),
  ##     Query = paste(sdmxbrowser_query,
  ##         'start =', yearStart,
  ##         'end =', yearEnd)
  ##     ,
  ##     Data = head(queryData)
  ##     )
  ## return(list.print)

}}

datatables_sdmxBrowser <- function(result = .sdmxBrowser())
{ if (length(result) > 0) {

  sdmxbrowser_flow = result$sdmxbrowser_flow

  sdmxbrowser_dimensions_all = result$sdmxbrowser_dimensions_all
  queryData <- result$queryData

  if (sdmxbrowser_flow=="") return(data.frame(INFO = "Please select data flow and submit query"))

  data.datatable <- queryData
  X <- strsplit(as.character(data.datatable$variable), split = "[.]")

  for (d in (seq(along = sdmxbrowser_dimensions_all)+1)) { # first item is flow id, d starting from 2
    data.datatable[[sdmxbrowser_dimensions_all[d-1]]] <- sapply(X, '[[', d)
  }
  data.datatable <- data.datatable[,!colnames(data.datatable)=="variable"]

  return(data.datatable)

}}

## M <- "Jun 2000"
## Q <- "2000 Q1"
## A <- "2000"

## as.Date(as.yearmon(M, format = "%b %Y"))
## as.Date(as.yearqtr(Q))
## as.Date(A, format = "%Y")



plots_sdmxBrowser <- function(result = .sdmxBrowser())
{ if (length(result) > 0) {

    sdmxbrowser_flow = result$sdmxbrowser_flow
    sdmxbrowser_query <- result$sdmxbrowser_query
    sdmxbrowser_yearStart <- result$sdmxbrowser_yearStart
    sdmxbrowser_yearEnd <- result$sdmxbrowser_yearEnd

    queryDataFreq <- result$queryDataFreq
    queryData <- result$queryData

    data.plots <- queryData

    if (sdmxbrowser_flow=="") return()

    if (queryDataFreq==12) {
        data.plots$time <- as.Date(as.yearmon(data.plots$time, format = "%b %Y"))
    } else if (queryDataFreq==4) {
        data.plots$time <- as.Date(as.yearqtr(data.plots$time))
    } else if (queryDataFreq==1) {
        ## data.plots$time <- as.Date(data.plots$time, format = "%Y")
        ## data.plots$time <- as.numeric(as.character(data.plots$time))
        data.plots$time <- as.Date(paste0(data.plots$time, '-01-01'), format = "%Y-%m-%d")
    }

    ncol <- length(unique(data.plots$variable))
    color.fill <- colorRampPalette(ui.sdmxBrowser.col)(ncol)

    p1 <- ggplot(data = data.plots, aes(x = time, y = value, group = variable)) +
      geom_line(aes(color = variable)) +
        ylab(label = NULL) +
          xlab(label = NULL) +
            scale_colour_manual(values = color.fill) +
              theme_bw() +
                theme(legend.position = "top", legend.box = "horizontal") +
                  ggtitle(label = paste(sdmxbrowser_query, "Start:", min(data.plots$time), "End:", max(data.plots$time)))

    ## print(p1)
    return(p1)

}}

morrischarts_sdmxBrowser <- function(result = .sdmxBrowser())
{ if (length(result) > 0) {

    queryData <- result$queryData
    queryDataFreq <- result$queryDataFreq
    ## freq <- frequency(queryData[[1]])
    ## queryData <- as.data.frame(queryData)
    ## data.morris <- data.frame(time = rownames(queryData), queryData)

    data.morris <- reshape(data = dataQuery, time ~ variable, value.var = "value")

    if (queryDataFreq==12) {
        data.morris$time <- format(as.yearmon(data.morris$time, format = "%b %Y"), format = "%Y-%m")
    }
    m1 <- mPlot(x = "time",
                y = names(data.morris)[names(data.morris)!="time"],
                type = "Line",
                data = data.morris)

    m1$set(pointSize = 0, lineWidth = 1
           )

    m1$addParams(
        ## width = 800,
        ## height = 400,
        ## resize = "true",
        ## redraw = "true",
        dom = "morrischarts_sdmxBrowser")
    ## m1
    return(m1)

}}

download_sdmxBrowser <- function(result = .sdmxBrowser(), zipfile = fname)
{ if (length(result) > 0) {

    ## export queries to download section
    sdmxbrowser_query <- result$sdmxbrowser_query
    queryData <- result$queryData

    sdmxbrowser_dimensions_all = result$sdmxbrowser_dimensions_all
    queryData <- result$queryData

    ## identical to datatable function
    data.datatable <- queryData
    X <- strsplit(as.character(data.datatable$variable), split = "[.]")

    for (d in (seq(along = sdmxbrowser_dimensions_all)+1)) { # first item is flow id, d starting from 2
        data.datatable[[sdmxbrowser_dimensions_all[d-1]]] <- sapply(X, '[[', d)
    }
    data.datatable <- data.datatable[,!colnames(data.datatable)=="variable"]

    tempdir = tempdir()
    unlink(paste0(tempdir, list.files(tempdir)))
    file.remove(file.path(tempdir, list.files(tempdir)))

    file <- file.path(tempdir, paste0(gsub("[*]", "", sdmxbrowser_query), '.csv'))
    ## write.csv(queryData, file = file, row.names = FALSE)
    write.csv(data.datatable, file = file, row.names = FALSE)

    zip(zipfile = zipfile, files = tempdir, extras = "-j")

}}
