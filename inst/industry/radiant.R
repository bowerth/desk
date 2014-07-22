################################################################
## functions used across tools in radiant
################################################################
changedata <- function(addCol, addColName = "")
{
    if(nrow(getdata()) == nrow(addCol) && addColName[1] != "")
    {
        return(values[[input$datasets]][,addColName] <- addCol)
    }
}

changedata_names <- function(oldnames, newnames)
{
    upnames <- colnames(values[[input$datasets]])
    upnames[which(upnames %in% oldnames)] <- newnames
    return(colnames(values[[input$datasets]]) <- upnames)
}

date2character_dat <- function(dat)
{
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
    ## eval(parse(text = paste0('ifelse(is.null(input$', width_name, '), return(values$plotWidth), return(input$', width_name, '))')))
    ifelse(is.null(input[[width_name]]), return(values$plotWidth), return(input[[width_name]]))
}
plotHeight <- function(height_name) {
    ## ifelse(is.null(input$viz_plot_height), return(values$plotHeight), return(input$viz_plot_height))
    ## eval(parse(text = paste0('ifelse(is.null(input$', height_name, '), return(values$plotHeight), return(input$', height_name, '))')))
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
    width_name <- paste0(fun_label, '_viz_plot_width') # lower
    height_name <- paste0(fun_label, '_viz_plot_height') # lower
    sum_name <- paste0("summary_", fun_label) # lower
    table_name <- paste0("tables_", fun_label) # lower
    datatable_name <- paste0("datatables_", fun_label)
    plot_name <- paste0("plots_", fun_label) # lower
    polychart_name <- paste0("polycharts_", fun_label) # lower
    highchart_name <- paste0("highcharts_", fun_label) # lower
    nvd3chart_name <- paste0("nvd3charts_", fun_label) # lower
    morrischart_name <- paste0("morrischarts_", fun_label) # lower
    map_name <- paste0("maps_", fun_label) # lower
    download_name <- paste0("download_", fun_label) # lower
    ## Generate output for the summary tab
    output[[sum_name]] <- renderPrint({
        result <- get(rfun_label)() # period_lower
        ## result <- isolate(get(rfun_label)())
        ## when no analysis was conducted (e.g., no variables selected)
        if(is.character(result)) return(cat(result,"\n"))
        get(sum_name)()
        ## isolate(get(sum_name)())
    })
    ## Generate output for the tables tab
    output[[table_name]] <- renderTable({
        result <- get(rfun_label)() # period_lower
        ## result <- isolate(get(rfun_label)())
        ## when no analysis was conducted (e.g., no variables selected)
        if(is.character(result)) return(cat(result,"\n"))
        get(table_name)()
        ## isolate(get(table_name)())
    })
    ## Generate output for the data tables tab
    output[[datatable_name]] <- renderDataTable({
        result <- get(rfun_label)() # period_lower
        ## result <- isolate(get(rfun_label)())
        ## when no analysis was conducted (e.g., no variables selected)
        if(is.character(result)) return(cat(result,"\n"))
        get(datatable_name)()
        ## isolate(get(datatable_name)())
    })
    ## Generate output for the plots tab
    output[[plot_name]] <- renderPlot({
        result <- get(rfun_label)() # period_lower
        ## w    n no analysis was conducted (e.g., no variables selected)
        if(is.character(result)) return(plot(x = 1, type = 'n', main=result, axes = FALSE, xlab = "", ylab = ""))
        get(plot_name)()
        ## isolate(get(plot_name)())
    },
                                      ## width=get(widthFun),
                                      ## height=get(heightFun)
                                      width=get(widthFun)(width_name),
                                      height=get(heightFun)(height_name)
                                      )
    ## Generate output for the polycharts tab
    output[[polychart_name]] <- renderChart({
        result <- get(rfun_label)() # period_lower
        ## w    n no analysis was conducted (e.g., no variables selected)
        ## if(is.character(result)) return(plot(x = 1, type = 'n', main=result, axes = FALSE, xlab = "", ylab = ""))
        if(is.character(result)) return()
        get(polychart_name)()
    })
    ## Generate output for the highcharts tab
    output[[highchart_name]] <- renderChart({
        result <- get(rfun_label)() # period_lower
        ## w    n no analysis was conducted (e.g., no variables selected)
        ## if(is.character(result)) return(plot(x = 1, type = 'n', main=result, axes = FALSE, xlab = "", ylab = ""))
        if(is.character(result)) return()
        get(highchart_name)()
    })
    ## Generate output for the nvd3charts tab
    output[[nvd3chart_name]] <- renderChart({
        result <- get(rfun_label)() # period_lower
        ## w    n no analysis was conducted (e.g., no variables selected)
        ## if(is.character(result)) return(plot(x = 1, type = 'n', main=result, axes = FALSE, xlab = "", ylab = ""))
        if(is.character(result)) return()
        get(nvd3chart_name)()
    })
    ## Generate output for the morrischarts tab
    output[[morrischart_name]] <- renderChart({
        result <- get(rfun_label)() # period_lower
        ## w    n no analysis was conducted (e.g., no variables selected)
        ## if(is.character(result)) return(plot(x = 1, type = 'n', main=result, axes = FALSE, xlab = "", ylab = ""))
        if(is.character(result)) return()
        get(morrischart_name)()
    })
    ## Generate output for the maps tab
    output[[map_name]] <- renderChart2({
        result <- get(rfun_label)() # period_lower
        ## w    n no analysis was conducted (e.g., no variables selected)
        ## if(is.character(result)) return(plot(x = 1, type = 'n', main=result, axes = FALSE, xlab = "", ylab = ""))
        if(is.character(result)) return()
        get(map_name)()
    })
    output[[download_name]] <- downloadHandler(
        filename = function() { "output.zip" },
        ## result <- get(rfun_label)() # period_lower
        ## result <- isolate(get(rfun_label)())
        ## result
        ## w    n no analysis was conducted (e.g., no variables selected)
        ## if(is.character(result)) return(plot(x = 1, type = 'n', main=result, axes = FALSE, xlab = "", ylab = ""))
        ## if(is.character(result)) return()
        content = function(file) {
                        fname <- paste0(file, '.zip')
                        get(download_name)(zipfile=fname)
                        file.rename(fname,file)
                    }
        ## isolate(get(download_name)(file="test.xls"))
    )

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
                                  'plotOutput(plot_name, height = "100%"),\n',
                                  'verbatimTextOutput(sum_name)))')
        string.tabsetPanel <- paste0(string.tabsetPanel, string.tabPanel, sep = ',\n')
    }
    if ("PolyCharts"%in%fun_tabs) {
        string.tabPanel <- paste0('tabPanel("PolyCharts", conditionalPanel(condition="!$(\'html\').hasClass(\'shiny-busy\')",\n',
                                  'showOutput(polychart_name, "polycharts")))')
        string.tabsetPanel <- paste0(string.tabsetPanel, string.tabPanel, sep = ',\n')
    }
    if ("HighCharts"%in%fun_tabs) {
        string.tabPanel <- paste0('tabPanel("HighCharts", conditionalPanel(condition="!$(\'html\').hasClass(\'shiny-busy\')",\n',
                                  'showOutput(highchart_name, "highcharts")))')
        string.tabsetPanel <- paste0(string.tabsetPanel, string.tabPanel, sep = ',\n')
    }
    if ("NVD3Charts"%in%fun_tabs) {
        string.tabPanel <- paste0('tabPanel("NVD3Charts", conditionalPanel(condition="!$(\'html\').hasClass(\'shiny-busy\')",\n',
                                  'showOutput(nvd3chart_name, "nvd3")))')
        string.tabsetPanel <- paste0(string.tabsetPanel, string.tabPanel, sep = ',\n')
    }
    if ("MorrisCharts"%in%fun_tabs) {
        string.tabPanel <- paste0('tabPanel("MorrisCharts", conditionalPanel(condition="!$(\'html\').hasClass(\'shiny-busy\')",\n',
                                  'showOutput(morrischart_name, "morris")))')
        string.tabsetPanel <- paste0(string.tabsetPanel, string.tabPanel, sep = ',\n')
    }
    if ("Maps"%in%fun_tabs) {
        string.tabPanel <- paste0('tabPanel("Maps",\n',
                                  'div(id = "myplot", style = "display:inline;position:absolute",\n',
                                  'showOutput(map_name, "datamaps")))')
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
