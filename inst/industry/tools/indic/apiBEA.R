
## require(stanApi)
## require(RCurl)
## require(rjson)
## configuration
## ui.apiBEA.proxy <- "wsg-proxy.oecd.org:80"
ui.apiBEA.proxy <- ""
ui.apiBEA.curl <- getCurlHandle()
curlSetOpt(.opts = list(proxy = ui.apiBEA.proxy), curl = ui.apiBEA.curl)
ui.apiBEA.apikey <- "7023E825-15FF-488D-B8D9-D70E6F67D439"

ui.apiBEA.method <- c(
    ## "GETDATASETLIST",
    ## "GETPARAMETERLIST",
    ## "GETPARAMETERVALUES",
    "GETDATA")


## get dataset list
## retrieves a list of the datasets currently offered
api.param <- list(USERID = ui.apiBEA.apikey,
                  METHOD = "GETDATASETLIST",
                  RESULTFORMAT = "JSON")
beaAPI(api.param = api.param, curl = ui.apiBEA.curl)


## datasets <- sapply(data[[1]][[2]][[1]], function(x) x$DatasetName)
## ui.apiBEA.datasetname = c("RegionalData", "NIPA", "NIUnderlyingDetail", "MNE", "FixedAssets", "ITA", "IIP", "GDPbyIndustry")
ui.apiBEA.datasetname = rbind.data.frame(
    ## c("RegionalData", 7),
    c("GDPbyIndustry", TRUE)
   ,
    c("NIPA", FALSE)
    ## ,
    ## c("NIUnderlyingDetail", 1)
    ## ,
    ## c("MNE", 1)
    ,
    c("FixedAssets", FALSE)
    ## ,
    ## c("ITA", 1)
    ## ,
    ## c("IIP", 1)
)
names(ui.apiBEA.datasetname) <- c("name", "multiple")

## GDPbyIndustry
## parameters <- sapply(data[[1]][[2]][[1]], function(x) x$ParameterName)
## cat(paste0('c(\"', gsub(", ", "\", \"", toString(parameters)), '\")','\n'))
ui.apiBEA.parameterlist <- c("Frequency", "Industry", "TableID", "Year")

output$uiaB_parameterlist <- renderUI({

        api.param <- list(USERID = ui.apiBEA.apikey,
                          METHOD = "GETPARAMETERLIST",
                          DATASETNAME = input$apibea_datasetname,
                          RESULTFORMAT = input$apibea_resultformat)

        data <- beaAPI(api.param = api.param, curl = ui.apiBEA.curl)
        parameterlist <- sapply(data[[1]][[2]][[1]], function(x) x$ParameterName)

        parameterlist <- parameterlist[!tolower(parameterlist)%in%c("year")]
        parameterlist <- parameterlist[!tolower(parameterlist)%in%c("showmillions")]

        list(
        selectInput("apibea_parameterlist", "Filter Dimensions:",
                    choices = parameterlist,
                    selected = parameterlist, multiple = TRUE, selectize = FALSE)
            )

})

output$uiaB_parametervalues <- renderUI({

    command.all <- NULL
    ## d <- 1
    for (d in seq(along = input$apibea_parameterlist)) {

        ## multiple <- "TRUE"
        if (input$apibea_parameterlist[d]=="TableID") {
            ## if (input$apibea_datasetname%in%c("NIPA", "FixedAssets")) {
            ##     multiple <- "FALSE"
            ## }
            multiple <- ui.apiBEA.datasetname$multiple[ui.apiBEA.datasetname$name==input$apibea_datasetname]
        } else multiple <- TRUE

        api.param <- list(USERID = ui.apiBEA.apikey,
                          METHOD = "GETPARAMETERVALUES",
                          DATASETNAME = input$apibea_datasetname,
                          PARAMETERNAME = input$apibea_parameterlist[d],
                          RESULTFORMAT = input$apibea_resultformat)

        data <- beaAPI(api.param = api.param, curl = ui.apiBEA.curl)

        dim.value <- beaJSONtoDF(List=data, third = 1)[, 1]
        dim.value <- as.character(dim.value)

        command <- paste0('selectInput("apibea_dim', d, '", "', input$apibea_parameterlist[d], ':", ',
                          'choices = ', paste0('c("', gsub(', ', '", "', toString(dim.value)), '")'), ', ',
                          'selected = "', dim.value[1], '", multiple = ', multiple, ', selectize = TRUE)'
                          )
        command.all <- paste(c(command.all, command), collapse = ',\n')
    }

    command.all <- paste0('list(', command.all, ')')

    eval(parse(text = command.all))

})

output$uiaB_timeperiod <- renderUI({

        ## api.param <- list(USERID = ui.apiBEA.apikey,
        ##                   METHOD = "GETPARAMETERVALUES",
        ##                   DATASETNAME = input$apibea_datasetname,
        ##                   PARAMETERNAME = "Year",
        ##                   RESULTFORMAT = input$apibea_resultformat)

        ## ## url <- beaAPI(api.param = api.param, curl = ui.apiBEA.curl, query = TRUE)

        ## data <- beaAPI(api.param = api.param, curl = ui.apiBEA.curl)

        ## ## ## in NIPA tables, each TableID has its own time period
        ## ## d <- match("TableID", apibea_parameterlist)
        ## ## table <- eval(parse(text = paste0('apibea_dim', d)))

        ## ## dim.value <- sapply(data[[1]][[2]][[1]], function(x) x$Key)
        ## dim.value <- beaJSONtoDF(List=data, third = 1)[, 1]

        ## dim.value <- as.numeric(dim.value)
        ## ## see request 2014-12-10
        ## dim.value <- dim.value[dim.value < 2014]
        ## ## dim.value[length(dim.value)]

        dim.value <- as.numeric(c(1970:2013))

        sliderInput("apibea_year", "Time Period:",
                    ## min = min(dim.value), max = max(dim.value),
                    min = dim.value[1], max = dim.value[length(dim.value)],
                    ## value = c((mean(dim.value)-1):(mean(dim.value)+1)),
                    ## value = c(dim.value[1], dim.value[length(dim.value)]),
                    value = c(1997, 2010),
                    format = "###0.")

})

output$ui_apiBEA <- renderUI({

    list(
        sliderInput(inputId = "apiBEA_viz_plot_height", label = "Height:", min = 0, max = 1000, value = 450, step = 50),
        sliderInput(inputId = "apiBEA_viz_plot_width", label = "Width:", min = 400, max = 1200, value = 850, step = 50),
        selectInput("apibea_method", "Method:", choices = ui.apiBEA.method, selected = "GETDATA", multiple = FALSE),
        selectInput("apibea_datasetname", "Datasetname:",
                    choices = as.character(ui.apiBEA.datasetname[["name"]]),
                    ## selected = "GDPbyIndustry", multiple = FALSE),
                    ## selected = "NIPA", multiple = FALSE),
                    selected = "FixedAssets", multiple = FALSE),
                    ## selected = "RegionalData", multiple = FALSE),
        uiOutput("uiaB_parameterlist"),
        uiOutput("uiaB_parametervalues"),
        uiOutput("uiaB_timeperiod"),

        selectInput("apibea_resultformat", "Resultformat:", "JSON"),
        wellPanel(
            h5("Summary"),
            checkboxInput("apibea_showparam", "Show API parameters", FALSE),
            checkboxInput("apibea_showquery", "Show Query", FALSE),
            checkboxInput("apibea_showdata", "Show Data", FALSE),
            ## conditionalPanel(condition = "input.apibea_showdata==true",
            checkboxInput("apibea_resultraw", "Return JSON string", FALSE)
                             ## )
            )
           ,

        helpAndReport("API BEA", "apibea", inclMD(file.path("tools", "help", "apiBEA.md")))
    )

})

apiBEA_widthSize <- reactive({
    ifelse(is.null(input$apiBEA_viz_plot_width), return(values$plotWidth), return(input$apiBEA_viz_plot_width))
})
apiBEA_heightSize <- reactive({
    ifelse(is.null(input$apiBEA_viz_plot_height), return(values$plotHeight), return(input$apiBEA_viz_plot_height))
})

output$apiBEA <- renderUI({
  statTabPanel(menu_name = "API",
               fun_name = "API BEA",
               rfun_label = ".apiBEA",
               fun_label = "apiBEA",
               fun_tabs = c("dygraph"),
               widthFun = "apiBEA_widthSize",
               heightFun = "apiBEA_heightSize"
               )
})


.apiBEA <- reactive({

    apiBEA(
        apibea_datasetname = input$apibea_datasetname,
        apibea_method = input$apibea_method,
        apibea_parameterlist = input$apibea_parameterlist,
        apibea_resultformat = input$apibea_resultformat,
        apibea_resultraw = input$apibea_resultraw,
        apibea_showparam = input$apibea_showparam,
        apibea_showquery = input$apibea_showquery,
        apibea_showdata = input$apibea_showdata,
        apibea_year = input$apibea_year,
        apiBEA_viz_plot_width = input$apiBEA_viz_plot_width,
        apiBEA_viz_plot_height = input$apiBEA_viz_plot_height
        )
})


apiBEA <- function(
    apibea_datasetname = apibea_datasetname,
    apibea_method = apibea_method,
    apibea_parameterlist = apibea_parameterlist,
    apibea_resultformat = apibea_resultformat,
    apibea_resultraw = apibea_resultraw,
    apibea_showparam = apibea_showparam,
    apibea_showquery = apibea_showquery,
    apibea_showdata = apibea_showdata,
    apibea_year = apibea_year,
    apiBEA_viz_plot_width = apiBEA_viz_plot_width,
    apiBEA_viz_plot_height = apiBEA_viz_plot_height
    ) {


    api.param <- list(USERID = ui.apiBEA.apikey,
                      METHOD = "GETPARAMETERLIST",
                      DATASETNAME = input$apibea_datasetname,
                      RESULTFORMAT = input$apibea_resultformat)

    data <- beaAPI(api.param = api.param, curl = ui.apiBEA.curl)
    parameterlist <- sapply(data[[1]][[2]][[1]], function(x) x$ParameterName)


    ## ## TESTING ##
    ## input <- list(
    ##     apibea_method = "GETDATA",
    ##     ## apibea_datasetname = "NIPA",
    ##     ## apibea_parameterlist = c("TableID", "Frequency", "ShowMillions"),
    ##     apibea_datasetname = "GDPbyIndustry",
    ##     apibea_parameterlist = c("Frequency", "Industry", "TableID"), # , "Year"), # output$uiaB_parameterlist
    ##     apibea_dim1 = "A", # Frequency
    ##     apibea_dim2 = "GDP", # Industry
    ##     apibea_dim3 = "1", # TableID
    ##     ## apibea_datasetname = "RegionalData",
    ##     ## apibea_datasetname = "NIUnderlyingDetail",
    ##     apibea_resultformat = "JSON",
    ##     apibea_year = c(1997, 2010)
    ## )
    ## apibea_datasetname = input$apibea_datasetname
    ## apibea_parameterlist = input$apibea_parameterlist
    ## apibea_year = input$apibea_year
    ## apibea_method = input$apibea_method
    ## apibea_resultformat = input$apibea_resultformat
    ## apibea_resultraw = input$apibea_resultraw

    ## apibea_parameterlist <- c("Frequency", "Industry", "TableID")
    parametervalue.all <- NULL
    for (d in seq(along = apibea_parameterlist)) {
        parametervalue <- eval(parse(text = paste0('input$apibea_dim', d)))
        parametervalue <- gsub(", ", ",", toString(parametervalue))
        parametervalue <- list(parametervalue)
        names(parametervalue)[1] <- apibea_parameterlist[d]
        parametervalue.all <- c(parametervalue.all, parametervalue)
    }

    ## apibea_year <- c(1997,1999)
    ## Add year from time slider
    parametervalue.year <- gsub(" ", "", toString(c(apibea_year[1]:apibea_year[2])))
    parametervalue.year <- list(parametervalue.year)
    names(parametervalue.year)[1] <- "Year"
    parametervalue.all <- c(parametervalue.all, parametervalue.year)

    api.param <- list(USERID = ui.apiBEA.apikey,
                      METHOD = apibea_method,
                      DATASETNAME = apibea_datasetname,
                      RESULTFORMAT = apibea_resultformat)
    api.param <- c(api.param, parametervalue.all)

    if (apibea_resultraw) {
        data <- beaAPI(api.param = api.param, curl = ui.apiBEA.curl, raw = TRUE)
    } else {
        List <- beaAPI(api.param = api.param, curl = ui.apiBEA.curl)
        ## data <- beaAPI(api.param = api.param, curl = ui.apiBEA.curl, raw = apibea_resultraw)

        ## ## data values are stored in different places of list - variation across datasetname
        ## valuefield <- as.numeric(as.character(ui.apiBEA.datasetname$valuefield[ui.apiBEA.datasetname$name==apibea_datasetname]))
        ## third = valuefield: 4: NIPA, GDPbyIndustry; 7: RegionalData
        ## data <- beaJSONtoDF(List=List, third = valuefield)
        data <- beaJSONtoDF(List=List, third = 4)

        ## beaJSONtoDF(List=List, third = 2)

        distinct.var <- names(data)
        if ("TimePeriod"%in%names(data)) setnames(data, "TimePeriod", "Year") # NIPA
        distinct.var <- distinct.var[!distinct.var%in%c("Year", "IndustrYDescription", "DataValue", "NoteRef")]
        ## data.plots <- data
        distinct.col <- data[, colnames(data)%in%distinct.var]
        distinct.col2 <- data.frame(apply(distinct.col, 2, function(x) as.character(x)), stringsAsFactors = FALSE)
        data$variable <-  apply(distinct.col2, 1, function(x) gsub(", ", ".", toString(x)))
        data$DataValue <- as.numeric(as.character(data$DataValue))

##
## Store data for replication
##
## print(data[,!colnames(data)%in%c("IndustrYDescription")])
## print.data.frame(data[,!colnames(data)%in%c("IndustrYDescription")], quote = TRUE)
##
## "TableID" "Frequency" "Year" "Industry" "DataValue" "NoteRef" "variable"
## "1" "A" "1997" "GDP" " 8608.5" "1" "1.A.GDP"
## "1" "A" "1998" "GDP" " 9089.2" "1" "1.A.GDP"
## "1" "A" "1999" "GDP" " 9660.6" "1" "1.A.GDP"
## "1" "A" "2000" "GDP" "10284.8" "1" "1.A.GDP"
## "1" "A" "2001" "GDP" "10621.8" "1" "1.A.GDP"
## "1" "A" "2002" "GDP" "10977.5" "1" "1.A.GDP"
## "1" "A" "2003" "GDP" "11510.7" "1" "1.A.GDP"
## "1" "A" "2004" "GDP" "12274.9" "1" "1.A.GDP"
## "1" "A" "2005" "GDP" "13093.7" "1" "1.A.GDP"
## "1" "A" "2006" "GDP" "13855.9" "1" "1.A.GDP"
## "1" "A" "2007" "GDP" "14477.6" "1" "1.A.GDP"
## "1" "A" "2008" "GDP" "14718.6" "1" "1.A.GDP"
## "1" "A" "2009" "GDP" "14418.7" "1" "1.A.GDP"
## "1" "A" "2010" "GDP" "14964.4" "1" "1.A.GDP"


    }

    return(
        list(api.param = api.param,
             data = data,
             ## parameterlist = parameterlist,
             apibea_resultraw = apibea_resultraw,
             apibea_showparam = apibea_showparam,
             apibea_showquery = apibea_showquery,
             apibea_showdata = apibea_showdata,
             apibea_year = apibea_year,
             apiBEA_viz_plot_width = apiBEA_viz_plot_width,
             apiBEA_viz_plot_height = apiBEA_viz_plot_height
        )
    )
}

summary_apiBEA <- function(result = .apiBEA()) {
    if (length(result) > 0) {

        apibea_showparam <- result$apibea_showparam
        apibea_showquery <- result$apibea_showquery
        apibea_showdata <- result$apibea_showdata

        api.param <- result$api.param
        data <- result$data
        apibea_year <- result$apibea_year

        query <- beaAPI(api.param = api.param, curl = ui.apiBEA.curl, query = TRUE)

        list.print <- list(Summary = "add items using checkbox inputs")

        if(apibea_showparam) list.print <- c(list.print, api.param)
        if(apibea_showquery) list.print <- c(list.print, list(query = query))
        if(apibea_showdata) list.print <- c(list.print, list(data = data))

        return(list.print)
    }
}

plots_apiBEA <- function(result = .apiBEA()) {
    if (length(result) > 0) {

        apibea_resultraw <- result$apibea_resultraw
        if (apibea_resultraw==TRUE) return()

        data <- result$data
        ## parameterlist <- result$parameterlist

        ## require(ggplot2)

        p1 <- ggplot(data = data, aes(x = Year, y = DataValue, group = variable)) +
            geom_line(aes(color = variable)) # +
        ## ylab(label = NULL) +
        ##     xlab(label = NULL) +
        ##         scale_colour_manual(values = color.fill) +
        ##             theme_bw() +
        ##                 theme(legend.position = "top", legend.box = "horizontal") +
        ##                     ggtitle(label = paste(sdmxbrowser_query, "Start:", min(data.plots$time), "End:", max(data.plots$time)))

    ## print(p1)
        return(p1)

    }
}

dygraphs_apiBEA <- function(result = .apiBEA()) {
    if (length(result) > 0) {

        ## require(dygraphs)
        ## require(xts)
        ## require(reshape2)
        ## data(sample_matrix)
        ## rownames(sample_matrix)

        apibea_resultraw <- result$apibea_resultraw
        if (apibea_resultraw) return()

        data <- result$data

        data.d <- dcast(data, Year ~ variable, value.var = "DataValue")
        ## h(data.d)

        data.d$Year <- as.numeric(as.character(data.d$Year))
        data.d$Year <- paste0(data.d$Year, '-01-01')

        rownames(data.d) <- data.d$Year
        data.d <- data.d[, colnames(data.d)!="Year"]

        data.d.xts <- as.xts(data.d, dateFormat = 'Date')

        d1 <- dygraph(data.d.xts)

        return(d1)

    }
}


## ## create help file:
## ## datasetname.TableID <- ui.apiBEA.datasetname[, 1]
## ## datasetname.TableID <- datasetname.TableID[!datasetname.TableID%in%c("RegionalData", "MNE", "FixedAssets", "ITA", "IIP")]
## datasetname.TableID <- c("GDPbyIndustry", "FixedAssets", "NIPA") # "NIUnderlyingDetail",
## ##
## table.all <- NULL
## for (dataset in datasetname.TableID) {
##     api.param <- list(USERID = ui.apiBEA.apikey,
##                       METHOD = "GETPARAMETERVALUES",
##                       DATASETNAME = dataset,
##                       PARAMETERNAME = "TableID",
##                       RESULTFORMAT = "JSON")
##     data <- beaAPI(api.param = api.param, curl = ui.apiBEA.curl)
##     df <- beaJSONtoDF(List=data, third = 1)
##     table <- kable(x = df, format = "markdown", row.names = FALSE, output = FALSE)
##     ##
##     table.prefix <- paste0('\n##### ', dataset, '\n')
##     table <- c(table.prefix, table)
##     table.all <- paste0(c(table.all, table), collapse = '\n')
##     ## cat(paste0('\n\n### ', dataset, '\n\n'))
##     ## cat(table, sep = '\n')
## }
## ##
## table.all <- gsub("[[]", "(", table.all)
## table.all <- gsub("[]]", ")", table.all)
## ##
## helpfile <- file.path(path, "tools", "help", "apiBEA.md")
## filecon <- file(helpfile)
## ## readLines(filecon)
## help.prefix <- paste0(c('This application provides a front-end to the BEA API',
##                         '#### Table contents by Dataset Name'), collapse = '\n\n')
## help.suffix <- "\n---\n\n&copy; OECD (2014)"
## help.body <- c(help.prefix, table.all, help.suffix)
## writeLines(text = help.body, con = filecon)
## close(filecon)
## ##
