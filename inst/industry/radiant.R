################################################################
## functions used across tools in radiant
################################################################
changedata <- function(addCol, addColName = "") {
    if(nrow(getdata()) == nrow(addCol) && addColName[1] != "") {
        return(values[[input$datasets]][,addColName] <- addCol)
    }
}

changedata_names <- function(oldnames, newnames) {
    upnames <- colnames(values[[input$datasets]])
    upnames[which(upnames %in% oldnames)] <- newnames
    return(colnames(values[[input$datasets]]) <- upnames)
}

date2character_dat <- function(dat) {
    ## xtable doesn't like dates
    isDate <- c(sapply(dat, is.Date))
    dat[,isDate] <- sapply(dat[,isDate], as.character)
    dat
}

inChecker <- function(tocheck) {
    ifelse(sum(tocheck %in% varnames()) < length(tocheck), return(NULL), return('OK'))
}

########################
## reactive functions
########################
getdata <- reactive({
    values[[input$datasets]]
})

getdata_class <- reactive({
    ## don't use isolate here or values won't change when the dataset is changed
    cls <- sapply(getdata(), function(x) class(x)[1])
    gsub("ordered","factor", cls)
})

varnames <- reactive({
    dat <- getdata_class()
    vars <- names(dat)
    names(vars) <- paste(vars, " {", dat, "}", sep = "")
    vars
})

date2character <- reactive({
    date2character_dat(getdata())
})

################################################################
## functions used to create Shiny in and outputs
################################################################
plotWidth <- function(width_name) {
    ## ifelse(is.null(input$viz_plot_width), return(values$plotWidth), return(input$viz_plot_width))
    ifelse(is.null(input[[width_name]]), return(values$plotWidth), return(input[[width_name]]))
}
plotHeight <- function(height_name) {
    ## ifelse(is.null(input$viz_plot_height), return(values$plotHeight), return(input$viz_plot_height))
    ifelse(is.null(input[[height_name]]), return(values$plotHeight), return(input[[height_name]]))
}

statPanel <- function(fun_name, rfun_label, fun_label,
                      ## rChart_lib,
                      fun_tabs,
                      widthFun, heightFun)
{
    if(isolate(input$nav_radiant) != fun_name) {
        return() # capital
    }
    width_name <- paste0(fun_label, '_viz_plot_width')
    height_name <- paste0(fun_label, '_viz_plot_height')
    sum_name <- paste0("summary_", fun_label)
    table_name <- paste0("tables_", fun_label)
    datatable_name <- paste0("datatables_", fun_label)
    plot_name <- paste0("plots_", fun_label)
    polychart_name <- paste0("polycharts_", fun_label)
    highchart_name <- paste0("highcharts_", fun_label)
    nvd3chart_name <- paste0("nvd3charts_", fun_label)
    morrischart_name <- paste0("morrischarts_", fun_label)
    map_name <- paste0("maps_", fun_label)
    download_name <- paste0("download_", fun_label)
    ## ace_name <- paste0("ace_", fun_label)
    html_name <- paste0("html_", fun_label)
    forcenetwork_name <- paste0("forcenetwork_", fun_label)
    sankeynetwork_name <- paste0("sankeynetwork_", fun_label)
    simplenetwork_name <- paste0("simplenetwork_", fun_label)
    treenetwork_name <- paste0("treenetwork_", fun_label)
    clusternetwork_name <- paste0("clusternetwork_", fun_label)
    treemapnetwork_name <- paste0("treemapnetwork_", fun_label)
    ## Generate output for the summary tab
    output[[sum_name]] <- renderPrint({
        result <- get(rfun_label)()
        ## result <- isolate(get(rfun_label)())
        ## when no analysis was conducted (e.g., no variables selected)
        if(is.character(result)) return(cat(result,"\n"))
        get(sum_name)()
        ## isolate(get(sum_name)())
    }) # width = 50
    ## Generate output for the tables tab
    output[[table_name]] <- renderTable({
        result <- get(rfun_label)()
        ## result <- isolate(get(rfun_label)())
        ## when no analysis was conducted (e.g., no variables selected)
        if(is.character(result)) return(cat(result,"\n"))
        get(table_name)()
        ## isolate(get(table_name)())
    })
    ## Generate output for the data tables tab
    output[[datatable_name]] <- renderDataTable({
        result <- get(rfun_label)()
        ## result <- isolate(get(rfun_label)())
        ## when no analysis was conducted (e.g., no variables selected)
        if(is.character(result)) return(cat(result,"\n"))
        get(datatable_name)()
        ## isolate(get(datatable_name)())
    })
    ## Generate output for the plots tab
    output[[plot_name]] <- renderPlot({
        result <- get(rfun_label)()
        ## w    n no analysis was conducted (e.g., no variables selected)
        if(is.character(result)) return(plot(x = 1, type = 'n', main=result, axes = FALSE, xlab = "", ylab = ""))
        get(plot_name)()
        ## isolate(get(plot_name)())
      },
                                      width=get(widthFun),
                                      height=get(heightFun)
                                      ## width=get(widthFun)(width_name), # call function "widthFun" with variable "width_name"
                                      ## height=get(heightFun)(height_name)
                                      )
    ## Generate output for the polycharts tab
    output[[polychart_name]] <- renderChart2({
        result <- get(rfun_label)()
        ## w    n no analysis was conducted (e.g., no variables selected)
        ## if(is.character(result)) return(plot(x = 1, type = 'n', main=result, axes = FALSE, xlab = "", ylab = ""))
        if(is.character(result)) return()
        get(polychart_name)()
    })
    ## Generate output for the highcharts tab
    output[[highchart_name]] <- renderChart({
        result <- get(rfun_label)()
        ## w    n no analysis was conducted (e.g., no variables selected)
        ## if(is.character(result)) return(plot(x = 1, type = 'n', main=result, axes = FALSE, xlab = "", ylab = ""))
        if(is.character(result)) return()
        get(highchart_name)()
    })
    ## Generate output for the nvd3charts tab
    output[[nvd3chart_name]] <- renderChart({
        result <- get(rfun_label)()
        ## w    n no analysis was conducted (e.g., no variables selected)
        ## if(is.character(result)) return(plot(x = 1, type = 'n', main=result, axes = FALSE, xlab = "", ylab = ""))
        if(is.character(result)) return()
        get(nvd3chart_name)()
    })
    ## Generate output for the morrischarts tab
    output[[morrischart_name]] <- renderChart({
        result <- get(rfun_label)()
        ## w    n no analysis was conducted (e.g., no variables selected)
        ## if(is.character(result)) return(plot(x = 1, type = 'n', main=result, axes = FALSE, xlab = "", ylab = ""))
        if(is.character(result)) return()
        get(morrischart_name)()
    })
    ## Generate output for the maps tab
    output[[map_name]] <- renderChart2({
        result <- get(rfun_label)()
        ## w    n no analysis was conducted (e.g., no variables selected)
        ## if(is.character(result)) return(plot(x = 1, type = 'n', main=result, axes = FALSE, xlab = "", ylab = ""))
        if(is.character(result)) return()
        get(map_name)()
    })
    output[[download_name]] <- downloadHandler(
        ## filename = function() { "output.zip" },
        filename = function() { paste0(sub("download_", "", download_name),'.zip') },
        content = function(file) {
            fname <- paste0(file, '.zip')
            get(download_name)(zipfile=fname)
            file.rename(fname,file)
        }
        ## isolate(get(download_name)(file="test.xls"))
        )

    output[[html_name]] <- renderPrint({
        result <- get(rfun_label)()
        ## w    n no analysis was conducted (e.g., no variables selected)
        ## if(is.character(result)) return(plot(x = 1, type = 'n', main=result, axes = FALSE, xlab = "", ylab = ""))
        if(is.character(result)) return()
        get(html_name)()
    })

    output[[forcenetwork_name]] <- renderForceNetwork({
        result <- get(rfun_label)()
        ## w    n no analysis was conducted (e.g., no variables selected)
        ## if(is.character(result)) return(plot(x = 1, type = 'n', main=result, axes = FALSE, xlab = "", ylab = ""))
        if(is.character(result)) return()
        get(forcenetwork_name)()
    })
    output[[sankeynetwork_name]] <- renderSankeyNetwork({
        result <- get(rfun_label)()
        ## w    n no analysis was conducted (e.g., no variables selected)
        ## if(is.character(result)) return(plot(x = 1, type = 'n', main=result, axes = FALSE, xlab = "", ylab = ""))
        if(is.character(result)) return()
        get(sankeynetwork_name)()
    })
    output[[simplenetwork_name]] <- renderSimpleNetwork({
        result <- get(rfun_label)()
        ## w    n no analysis was conducted (e.g., no variables selected)
        ## if(is.character(result)) return(plot(x = 1, type = 'n', main=result, axes = FALSE, xlab = "", ylab = ""))
        if(is.character(result)) return()
        get(simplenetwork_name)()
    })
    output[[treenetwork_name]] <- renderTreeNetwork({
        result <- get(rfun_label)()
        ## w    n no analysis was conducted (e.g., no variables selected)
        ## if(is.character(result)) return(plot(x = 1, type = 'n', main=result, axes = FALSE, xlab = "", ylab = ""))
        if(is.character(result)) return()
        get(treenetwork_name)()
    })

    output[[clusternetwork_name]] <- renderClusterNetwork({
        result <- get(rfun_label)()
        ## w    n no analysis was conducted (e.g., no variables selected)
        ## if(is.character(result)) return(plot(x = 1, type = 'n', main=result, axes = FALSE, xlab = "", ylab = ""))
        if(is.character(result)) return()
        get(clusternetwork_name)()
    })
    output[[treemapnetwork_name]] <- renderTreemapNetwork({
        result <- get(rfun_label)()
        ## w    n no analysis was conducted (e.g., no variables selected)
        ## if(is.character(result)) return(plot(x = 1, type = 'n', main=result, axes = FALSE, xlab = "", ylab = ""))
        if(is.character(result)) return()
        get(treemapnetwork_name)()
    })


    string.tabsetPanel <- NULL
    if ("Tables"%in%fun_tabs) {
        string.tabPanel <- paste0('tabPanel("Tables", conditionalPanel(condition="!$(\'html\').hasClass(\'shiny-busy\')",\n',
                                  'tableOutput(table_name)))')
        string.tabsetPanel <- paste0(string.tabsetPanel, string.tabPanel, sep = ',\n')
    }
    if ("DataTables"%in%fun_tabs) {
        string.tabPanel <- paste0('tabPanel("DataTables", conditionalPanel(condition="!$(\'html\').hasClass(\'shiny-busy\')",\n',
                                  'dataTableOutput(datatable_name)))')
        string.tabsetPanel <- paste0(string.tabsetPanel, string.tabPanel, sep = ',\n')
    }
    if ("Plots"%in%fun_tabs) {
        string.tabPanel <- paste0('tabPanel("Plots", conditionalPanel(condition="!$(\'html\').hasClass(\'shiny-busy\')",\n',
                                  'plotOutput(plot_name, height = "100%"),\n', # height = "100%"
                                  'verbatimTextOutput(sum_name)))')
        string.tabsetPanel <- paste0(string.tabsetPanel, string.tabPanel, sep = ',\n')
    }
    if ("NVD3Charts"%in%fun_tabs) {
        string.tabPanel <- paste0('tabPanel("NVD3Charts", conditionalPanel(condition="!$(\'html\').hasClass(\'shiny-busy\')",\n',
                                  'showOutput(nvd3chart_name, "nvd3")))')
        string.tabsetPanel <- paste0(string.tabsetPanel, string.tabPanel, sep = ',\n')
    }
    if ("HighCharts"%in%fun_tabs) {
        string.tabPanel <- paste0('tabPanel("HighCharts", conditionalPanel(condition="!$(\'html\').hasClass(\'shiny-busy\')",\n',
                                  'showOutput(highchart_name, "highcharts")))')
        string.tabsetPanel <- paste0(string.tabsetPanel, string.tabPanel, sep = ',\n')
    }
    if ("PolyCharts"%in%fun_tabs) {
        string.tabPanel <- paste0('tabPanel("PolyCharts", conditionalPanel(condition="!$(\'html\').hasClass(\'shiny-busy\')",\n',
                                  'showOutput(polychart_name, "polycharts")))')
        string.tabsetPanel <- paste0(string.tabsetPanel, string.tabPanel, sep = ',\n')
    }
    if ("MorrisCharts"%in%fun_tabs) {
        ## tags$style(".morris{width: 800px;}"),
        ## tags$style('.leaflet {height: 400px;}')
        ## tags$style("#morrischarts_sdmxBrowser{width:800px;}") # CSS
        string.tabPanel <- paste0('tabPanel("MorrisCharts", conditionalPanel(condition="!$(\'html\').hasClass(\'shiny-busy\')",\n',
                                  'showOutput(morrischart_name, "morris")))')
        string.tabsetPanel <- paste0(string.tabsetPanel, string.tabPanel, sep = ',\n')
    }
    if ("Maps"%in%fun_tabs) {
        string.tabPanel <- paste0('tabPanel("Maps",\n',
                                    'div(id = "rMaps", style = "display:inline;position:absolute",\n',
                                      'showOutput(map_name, "datamaps")',
                                    ')',
                                  ')')
        string.tabsetPanel <- paste0(string.tabsetPanel, string.tabPanel, sep = ',\n')
    }

    if ("HTML"%in%fun_tabs) {
        string.tabPanel <- paste0('tabPanel("HTML", \n',
                                  'htmlOutput(html_name) \n',
                                  ')')
        string.tabsetPanel <- paste0(string.tabsetPanel, string.tabPanel, sep = ',\n')
    }

    if ("forceNetwork"%in%fun_tabs) {
        string.tabPanel <- paste0('tabPanel("forceNetwork", \n',
                                  'forceNetworkOutput(forcenetwork_name) \n',
                                  ')')
        string.tabsetPanel <- paste0(string.tabsetPanel, string.tabPanel, sep = ',\n')
    }
    if ("sankeyNetwork"%in%fun_tabs) {
        string.tabPanel <- paste0('tabPanel("sankeyNetwork", \n',
                                  'sankeyNetworkOutput(sankeynetwork_name) \n',
                                  ')')
        string.tabsetPanel <- paste0(string.tabsetPanel, string.tabPanel, sep = ',\n')
    }
    if ("simpleNetwork"%in%fun_tabs) {
        string.tabPanel <- paste0('tabPanel("simpleNetwork", \n',
                                  'simpleNetworkOutput(simplenetwork_name) \n',
                                  ')')
        string.tabsetPanel <- paste0(string.tabsetPanel, string.tabPanel, sep = ',\n')
    }
    if ("treeNetwork"%in%fun_tabs) {
        string.tabPanel <- paste0('tabPanel("treeNetwork", \n',
                                  'treeNetworkOutput(treenetwork_name, height = 650) \n',
                                  ')')
        string.tabsetPanel <- paste0(string.tabsetPanel, string.tabPanel, sep = ',\n')
    }
    if ("clusterNetwork"%in%fun_tabs) {
        string.tabPanel <- paste0('tabPanel("clusterNetwork", \n',
                                  ## 'div(id = "cluster2", style = "display:inline;position:absolute",\n',
                                    'clusterNetworkOutput(clusternetwork_name, height = 650) \n',
                                  ## 'clusterNetworkOutput(clusternetwork_name, width = ', get(widthFun)(), ', height = ', get(heightFun)(), ') \n',
                                  ## ')',
                                  ',includeCSS(system.file("htmlwidgets", "lib", "css-0", "clusterNetwork.css", package = "networkD3")) \n',
                                  ',verbatimTextOutput(sum_name) \n',
                                  ')')
        string.tabsetPanel <- paste0(string.tabsetPanel, string.tabPanel, sep = ',\n')
    }
    if ("treemapNetwork"%in%fun_tabs) {
        string.tabPanel <- paste0('tabPanel("treemapNetwork", \n',
                                    ## 'div(id = "treemap", style = "display:inline;position:absolute",\n',
                                      'treemapNetworkOutput(treemapnetwork_name, width = 850, height = 550) \n',
                                    ## ')',
                                  ',verbatimTextOutput(sum_name) \n',
                                  ')')
        string.tabsetPanel <- paste0(string.tabsetPanel, string.tabPanel, sep = ',\n')
    }


    string.tabsetPanel <- substring(string.tabsetPanel, 1, nchar(string.tabsetPanel)-2)

    return(eval(parse(text = paste0('tabsetPanel(id = "tabs_', fun_label, '",\n', string.tabsetPanel, ')'))))
}


statTabPanel <- function(menu_name, fun_name, rfun_label, fun_label,
                         ## rChart_lib,
                         fun_tabs = c("Plots", "Tables", "Maps", "PolyCharts", "HighCharts", "NVD3Charts"),
                         widthFun = "plotWidth", heightFun = "plotHeight")
{
    isolate({
        sidebarLayout(

            sidebarPanel(
                ## based on https://groups.google.com/forum/?fromgroups=#!topic/shiny-discuss/PzlSAmAxxwo
                div(class = "busy",
                    p("Calculation in progress ..."),
                    img(src="ajaxloaderq.gif")
                    ),
                wellPanel(
                    HTML(paste("<label><strong>Menu:",menu_name,"</strong></label>")),
                    HTML(paste("<label><strong>Indicator:",isolate(input$nav_radiant),"</strong></label>")) # tabPanel id value
                    ## ,HTML(paste("<label><strong>Data:",isolate(input$datasets),"</strong></label>"))
                    ),
                ## wellPanel(
                ##     h5("Plot options"),
                ##     sliderInput(inputId = "viz_plot_height", label = "Height:", min = 400, max = 1000, value = 750, step = 50),
                ##     sliderInput(inputId = "viz_plot_width", label = "Width:", min = 400, max = 1200, value = 750, step = 50)
                ##     ),
                uiOutput(paste0("ui_",fun_label))
                ),
            mainPanel(
                statPanel(fun_name, rfun_label, fun_label,
                          ## rChart_lib,
                          fun_tabs,
                          ## fun_tabs = c("Plots", "Tables", "Maps", "PolyCharts", "HighCharts", "NVD3Charts"),
                          widthFun, heightFun)
                )
            )
    })
}
