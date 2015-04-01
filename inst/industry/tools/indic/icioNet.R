
## path <- file.path(dbpath, "GitHub", "desk", "inst", "industry")
## setwd(path)
## require(data.table)
## require(dplyr)
## require(networkD3)


## system.file("miserables.json", package = "networkD3")
## path <- file.path(dbpath, "GitHub", "desk", "inst", "industry")

## ## Load miserables data for forceNetwork()
## ui.icionet.miserables <- file.path("data", "data_init", "miserables.json")
## ui.icionet.MisLinks <- JSONtoDF(file = ui.icionet.miserables, array = "links")
## ui.icionet.MisNodes <- JSONtoDF(file = ui.icionet.miserables, array = "nodes")

## ## Load energy data for sankeyNetwork()
## ui.icionet.Energy <- file.path("data", "data_init", "energy.json")
## ui.icionet.EngLinks <- JSONtoDF(file = ui.icionet.Energy, array = "links", simplify = FALSE)
## ui.icionet.EngNodes <- JSONtoDF(file = ui.icionet.Energy, array = "nodes", simplify = FALSE)

## ## Load flare data for treeNetwork()
## ui.icionet.Flare <- file.path("data", "data_init", "flare.json")
## ui.icionet.Flare <- rjson::fromJSON(file = ui.icionet.Flare)

## ## Load flare data for clusterNetwork(), treemapNetwork()
## ui.icionet.FlareImports <- file.path("data", "data_init", "readme-flare-imports.json")
## ui.icionet.FlareImports <- rjson::fromJSON(file = ui.icionet.FlareImports, simplify = FALSE)

if (!exists("icioIndic_data")) {
    load(file.path("data", "data_init", "icioIndic.rda"))
    icioIndic_data <- data.table(icioIndic_data)
}

## icioNet_namecou <- icioNet_namecou[!icioNet_namecou%in%c("CYP", "ROW", "Total")] # missing values
excl <- c("CYP", "ROW")
icioNet_data <- subset(icioIndic_data, !cou%in%excl & !par%in%excl)

icioNet_nameindic <- as.character(unique(icioNet_data$indic))
icioNet_nameyear <- unique(icioNet_data$year)
icioNet_namecou <- as.character(unique(icioNet_data$cou))
## icioNet_namecou <- icioNet_namecou[!icioNet_namecou%in%c("CYP", "ROW")] # missing values
## icioNet_namepar <- unique(icioNet_data$par)
icioNet_nameind <- as.character(unique(icioNet_data$ind))

icioNet_namereg <- read.csv(file.path("data", "data_init", "icioIndic_namereg.csv"))
icioNet_namereg <- subset(data.table(icioNet_namereg), select = c("cou", "region"))

icioNet_namereg <- icioNet_namereg[icioNet_namereg$cou%in%c(icioNet_namecou, "Total"),]
icioNet_namereg$region <- gsub("[ .-]", "", icioNet_namereg$region)

setnames(icioNet_namereg, "region", "couregion")
icioNet_namereg$parregion <- icioNet_namereg$couregion
icioNet_namereg$par <- icioNet_namereg$cou

## h(icioNet_namereg)

icioNet_data <- left_join(icioNet_data,
                            subset(icioNet_namereg, select = c("cou", "couregion")),
                            by = c("cou" = "cou"))
icioNet_data <- left_join(icioNet_data,
                            subset(icioNet_namereg, select = c("par", "parregion")),
                            by = c("par" = "par"))

icioNet_data$cou <- paste(icioNet_data$couregion, icioNet_data$cou, sep = '.')
icioNet_data$par <- paste(icioNet_data$parregion, icioNet_data$par, sep = '.')
## unique(icioNet_data$par)
## h(icioNet_data)
## par: NA.Total because parregion == NA

## table <- read.csv(system.file(file.path("data", "cities.csv"), package = "networkD3"))
## cat(gsub(', ', '", "', toString(unique(table$color))))
icioNet_colorpalette <- c("#E41A1C", "#FFFF33", "#FF7F00", "#999999", "#984EA3", "#377EB8", "#4DAF4A", "#F781BF", "#A65628")
icioNet_namereg_col <- sort(unique(icioNet_namereg$couregion[!is.na(icioNet_namereg$couregion)]))
icioNet_namereg_col <- data.frame(region = icioNet_namereg_col, color = icioNet_colorpalette[1:length(icioNet_namereg_col)])


output$ui_icioNet <- renderUI({

    ## doLogin()
    ## if (loginData$LoggedIn) {

    list(

       ##  wellPanel(
       ##      checkboxInput("icioNet_viz_plot_controls", "Plot options", FALSE),
       ##      conditionalPanel(condition = "input.icioNet_viz_plot_controls == true",
       ##                       sliderInput(inputId = "icioNet_viz_plot_width", label = "Width:", min = 400, max = 1200, value = 800, step = 50),
       ##                       sliderInput(inputId = "icioNet_viz_plot_height", label = "Height:", min = 300, max = 1200, value = 600, step = 50)
       ##                       )
       ##  )
       ## ,

        ## selectInput("icionet_charttype", "Choose chart type", c("network", "tree", "sankey"), selected = "network"),
       ##  sliderInput("icionet_opacity", label = "Choose node opacity",
       ##              min = 0, max = 1, step = 0.01, value = 0.5
       ##              )
       ## ,

        selectInput("icionet_indic", "Indicator:", icioNet_nameindic, selected = "FDDVA", multiple = FALSE)
       ,
        selectInput("icionet_ind", "Industry:", icioNet_nameind, selected = "Total", multiple = FALSE)
       ,
        selectInput("icionet_year", "Year:", icioNet_nameyear, selected = 2009, multiple = FALSE)
       ,
        selectInput("icionet_nimports", "Number of imports flows:", c(1:10), selected = 5, multiple = FALSE)
       ,

        helpAndReport("ICIO Networks", "icioNet", inclMD(file.path("tools", "help", "icioNet.md")))
    ) # list(...

})

icioNet_widthSize <- reactive({
    ifelse(is.null(input$icioNet_viz_plot_width), return(values$plotWidth), return(input$icioNet_viz_plot_width))
})
## icioNet_heightSize <- reactive({
##     ifelse(is.null(input$icioNet_viz_plot_height), return(values$plotHeight), return(input$icioNet_viz_plot_height))
## })
icioNet_heightSize <- reactive({
    ifelse(is.null(input$icioNet_viz_plot_height), return(values$plotHeight), return(input$icioNet_viz_plot_height))
})

output$icioNet <- renderUI({
  ## for input-output
  statTabPanel(menu_name = "ICIO", # menu_name: for side bar - coincide with navbarMenu
               fun_name = "ICIO Networks",           # fun_name
               rfun_label = ".icioNet",         # rfun_label
               fun_label = "icioNet"           # fun_label
               ## ,fun_tabs = c("forceNetwork", "sankeyNetwork", "treeNetwork", "clusterNetwork", "treemapNetwork")
               ,fun_tabs = c("clusterNetwork", "treemapNetwork", "chordNetwork")
               ,widthFun = "icioNet_widthSize"
               ,heightFun = "icioNet_heightSize"
               )
})


.icioNet <- reactive({

    icioNet(
        ## icionet_charttype = input$icionet_charttype,
        ## icionet_opacity = input$icionet_opacity,
        icionet_indic = input$icionet_indic,
        icionet_ind = input$icionet_ind,
        icionet_year = input$icionet_year,
        icionet_nimports = input$icionet_nimports,
        icioNet_viz_plot_width = input$icioNet_viz_plot_width,
        icioNet_viz_plot_height = input$icioNet_viz_plot_height
        )
})

icioNet <- function(
    ## icionet_charttype = icionet_charttype,
    ## icionet_opacity = icionet_opacity,
    icionet_indic = icionet_indic,
    icionet_ind = icionet_ind,
    icionet_year = icionet_year,
    icionet_nimports = icionet_nimports,
    icioNet_viz_plot_width = icioNet_viz_plot_width,
    icioNet_viz_plot_height = icioNet_viz_plot_height
    ) {

    ## input <- list(icionet_indic = "FDDVA",
    ##               icionet_ind = "Total",
    ##               icionet_year = 2009,
    ##               icionet_nimports = 5)
    ## icionet_indic = input$icionet_indic
    ## icionet_ind = input$icionet_ind
    ## icionet_year = input$icionet_year
    ## icionet_nimports = input$icionet_nimports

    data.select <- subset(icioNet_data,
                          indic == icionet_indic &
                              ind == icionet_ind &
                                  year == icionet_year,
                          select = c("cou", "par", "value"))

    require(data.table)
    data.list <- NULL
    namereg <- sort(unique(icioNet_namereg$couregion))
    ## reg <- namereg[1]
    for (reg in namereg) {
        reg.namecou <- sort(icioNet_namereg$cou[icioNet_namereg$couregion==reg])
        ## country <- paste0(reg[1], '.', reg.namecou[1])
        for (country in paste0(reg, '.', reg.namecou)) {
            data.select.cou <- subset(data.select, cou == country)

            ## cou.size <- data.select.cou[par == "NA.Total", value] # data.table syntax
            cou.size <- data.select.cou$value[data.select.cou$par == "NA.Total"]
            
            cou.imports <- subset(data.select.cou, !par %in% c("NA.Total", "NA.ROW", "NA.CYP"))
            cou.imports <- cou.imports[order(-cou.imports$value),][1:icionet_nimports, "par"] # data.table syntax
            cou.imports.list <- list()
            for (import in cou.imports) {
                cou.imports.list <- c(cou.imports.list, list(import))
            }
            data.cou <- list(name = country,
                             size = cou.size,
                             imports = cou.imports.list)
            data.list <- c(data.list, list(data.cou))
        }
    }
    
    ## unique(data.select$cou)
    ## unique(data.select$par)
    ## names(data.select)
    data.matrix <- dcast(subset(data.select, par!="NA.Total"), cou ~ par, value.var = "value")
    rownames <- data.matrix$cou
    data.matrix <- data.matrix[, !colnames(data.matrix)=="cou"]
    data.matrix <- as.matrix(data.matrix)
    rownames(data.matrix) <- rownames

    X <- strsplit(rownames, split = "[.]")
    ## data.table <- data.frame(cou = sub(".+[.]", "", rownames), region = sub("[.].+", "", rownames))
    data.table <- data.frame(name = sapply(X, "[[", 2), region = sapply(X, "[[", 1))
    data.table <- merge(data.table, icioNet_namereg_col)

    return(
        list(## icionet_charttype = icionet_charttype,
            ## icionet_opacity = icionet_opacity,
            data.list = data.list,
            data.matrix = data.matrix,
            data.table = data.table,
            icioNet_viz_plot_width = icioNet_viz_plot_width,
            icioNet_viz_plot_height = icioNet_viz_plot_height
        )
           )
}

summary_icioNet <- function(result = .icioNet()) {
    if (length(result) > 0) {

        data.list <- result$data.list

        list.print <- NULL

        ## icionet_opacity <- result$icionet_opacity
        icioNet_viz_plot_width = result$icioNet_viz_plot_width
        ## icioNet_viz_plot_height = result$icioNet_viz_plot_height

        icioNet_viz_plot_height <- isolate(icioNet_heightSize())

        ## list.print <- c(list.print,
        ##                 list(## Opacity = icionet_opacity,
        ##                     Data = toString(data.list[[1]]),
        ##                     ## Data = toString("test"),
        ##                      Width = icioNet_viz_plot_width,
        ##                      Height = icioNet_viz_plot_height)
        ##                 )
        ## list.print <- toString(str(data.list))
        list.print <- toJSON(data.list)
        ## list.print <- toJSON(ui.icionet.FlareImports)

        ## return(list.print)
        return(cat(list.print))
    }
}


chordnetwork_icioNet <- function(result = .icioNet()) {
    if (length(result) > 0) {

        data.matrix = result$data.matrix
        data.table = result$data.table

        icioNet_viz_plot_width <- result$icioNet_viz_plot_width
        icioNet_viz_plot_height <- result$icioNet_viz_plot_height

        ## table <- read.csv(system.file(file.path("data", "cities.csv"), package = "networkD3"))
        ## matrix <- JSONtoMatrix(file = system.file(file.path("data", "matrix.json"), package = "networkD3"))
        ## nrow(matrix)
        ## nrow(data.matrix)
        ## ncol(matrix)
        ## ncol(data.matrix)
        ## setdiff(colnames(data.matrix), rownames(data.matrix))

        rownames(data.matrix) <- NULL
        colnames(data.matrix) <- NULL

        ## ## test with chordNetwork.R
        ## matrix <- data.matrix
        ## digits <- 5
        ## h(matrix)
        ## df <- data.table

        chordNetwork(
            ## matrix = matrix
            matrix = data.matrix
           ,
            digits = 5
           ,
            ## df = table
            df = data.table
           ,
            ## height = icioNet_viz_plot_width
            height = NULL
           ,
            ## width = icioNet_viz_plot_height
            width = NULL
           ,
            fontSize = 10
           ,
            linkColour = "#ccc"
           ,
            nodeColour = "#fff"
           ,
            nodeStroke = "steelblue"
           ,
            textColour = "#111"
           ,
            opacity = 0.9
           ,
            margin = 0
        )

    }
}

clusternetwork_icioNet <- function(result = .icioNet()) {
    if (length(result) > 0) {

        ## devtools::load_all(file.path(dbpath, "GitHub", "networkD3"))
        ## devtools::document(file.path(dbpath, "GitHub", "networkD3"))
        ## devtools::install(file.path(dbpath, "GitHub", "networkD3"))
        ## library(rjson)

        data.list <- result$data.list

        icioNet_viz_plot_width <- result$icioNet_viz_plot_width
        icioNet_viz_plot_height <- result$icioNet_viz_plot_height

        icionet_opacity = ifelse(is.null(result$icionet_opacity), 0.5, result$icionet_opacity)

        ## library(networkD3)

        clusterNetwork(
            ## List = ui.icionet.FlareImports
            List = data.list
           ,
            ## height = icioNet_viz_plot_width
            height = NULL
           ,
            ## width = icioNet_viz_plot_height
            width = NULL
           ,
            fontSize = 10
           ,
            linkColour = "#ccc"
           ,
            nodeColour = "#fff"
           ,
            nodeStroke = "steelblue"
           ,
            textColour = "#111"
           ,
            opacity = 0.9
           ,
            margin = 0
        )



        }
}

treemapnetwork_icioNet <- function(result = .icioNet()) {
    if (length(result) > 0) {

        ## devtools::load_all(file.path(dbpath, "GitHub", "networkD3"))
        ## devtools::document(file.path(dbpath, "GitHub", "networkD3"))
        ## devtools::install(file.path(dbpath, "GitHub", "networkD3"))
        ## library(rjson)

        data.list <- result$data.list

        icionet_opacity = ifelse(is.null(result$icionet_opacity), 0.5, result$icionet_opacity)

        treemapNetwork(
            ## List = ui.icionet.FlareImports
            List = data.list
           ,
            height = NULL
           ,
            width = NULL
           ,
            fontSize = 10
           ,
            linkColour = "#ccc"
           ,
            nodeColour = "#fff"
           ,
            nodeStroke = "steelblue"
           ,
            textColour = "#111"
           ,
            opacity = 0.9
           ,
            margin = 0
        )

        }
}

## ## ########################
## ## export to Jekyll site
## ## ########################

## require(networkD3)

## ui.icionet.FlareImports <- file.path(dbpath, "GitHub", "networkD3", "JSONdata", "readme-flare-imports.json")
## ui.icionet.FlareImports <- rjson::fromJSON(file = ui.icionet.FlareImports, simplify = FALSE)
## data.list <- ui.icionet.FlareImports

## type <- "chordNetwork"
## widget <- chordNetwork(matrix = data.matrix, digits = 5, df = data.table, height = NULL, width = NULL, fontSize = 10, linkColour = "#ccc", nodeColour = "#fff", nodeStroke = "steelblue", textColour = "#111", opacity = 0.9, margin = 0)

## type <- "clusterNetwork"
## widget <- clusterNetwork(List = data.list, height = 700, width = 700, fontSize = 10, linkColour = "#ccc", nodeColour = "#fff", nodeStroke = "steelblue", textColour = "#111", opacity = 0.9, margin = 0)

## type <- "treemapNetwork"
## widget <- treemapNetwork(List = data.list, height = 400, width = 700, fontSize = 10, linkColour = "#ccc", nodeColour = "#fff", nodeStroke = "steelblue", textColour = "#111", opacity = 0.9, margin = 0)

## outpath <- file.path(dbpath, "GitHub", "jekyll", "industry", "figures", "app_icioNet", type)
## if (!file.exists(outpath)) dir.create(outpath)
## file <- file.path(outpath, "index.html")
## lib <- "index_files"
## libpath <- file.path(outpath, lib)
## if (!file.exists(libpath)) dir.create(libpath)

## htmlwidgets::saveWidget(widget = widget, file = file, selfcontained = FALSE, libdir = NULL)

## for (folder in list.files(libpath)) {
##     file.copy(from = file.path(libpath, folder), to = file.path(dbpath, "GitHub", "jekyll", "industry", "www", "htmlwidgets"), recursive = TRUE)
## }

## unlink(libpath, recursive = TRUE)

## fileCon <- file(file)
## text.body <- readLines(fileCon)
## text.body <- sub(lib, "/www/htmlwidgets", text.body)
## ## text.body <- sub("./index_files", "/www", text.body)
## writeLines(text = text.body, con = fileCon)
## close(fileCon)

## ## copy file manually to _site:
## file.copy(from = file,
##           to = file.path(dbpath, "GitHub", "jekyll", "industry", "_site", "figures", "app_icioNet", "clusterNetwork", "index.html"), overwrite = TRUE)

## ## print data
## cat(rjson::toJSON(data.list))
## ## copy console output and paste in http://jsbeautifier.org/, click "Beautify JavaScript or HTML"
## ## paste result in jekyll page with ```json ... ``` block fencing

## ## ########################
## ## END
## ## export to Jekyll site
## ## ########################

forcenetwork_icioNet <- function(result = .icioNet()) {
    if (length(result) > 0) {

        ## icioNet_viz_plot_width <- isolate(icioNet_widthSize())
        ## icioNet_viz_plot_height <- isolate(icioNet_heightSize())

        ## icioNet_viz_plot_width <- result$icioNet_viz_plot_width
        ## icioNet_viz_plot_height <- result$icioNet_viz_plot_height

        icionet_opacity = ifelse(is.null(result$icionet_opacity), 0.5, result$icionet_opacity)

        forceNetwork(Links = ui.icionet.MisLinks,
                     Nodes = ui.icionet.MisNodes,
                     Source = "source",
                     Target = "target",
                     Value = "value",
                     NodeID = "name",
                     Group = "group",
                     ## width = icioNet_viz_plot_width,
                     ## height = icioNet_viz_plot_height,
                     ## opacity = 0.5
                     opacity = icionet_opacity
                     )

      }
}

sankeynetwork_icioNet <- function(result = .icioNet()) {
    if (length(result) > 0) {

        ## icioNet_viz_plot_width <- isolate(icioNet_widthSize())
        ## icioNet_viz_plot_height <- isolate(icioNet_heightSize())
        ## fontsize = min(icioNet_viz_plot_width, icioNet_viz_plot_height) / 50

        sankeyNetwork(Links = ui.icionet.EngLinks,
                     Nodes = ui.icionet.EngNodes,
                     Source = "source",
                     Target = "target",
                     Value = "value",
                     NodeID = "name",
                     fontsize = 12,
                     ## fontsize = fontsize,
                     nodeWidth = 30,
                     ## width = 700,
                     ## width = icioNet_viz_plot_width,
                     ## height = icioNet_viz_plot_height
                     )

    }
}

treenetwork_icioNet <- function(result = .icioNet()) {
    if (length(result) > 0) {

        ## icioNet_viz_plot_width <- isolate(icioNet_widthSize())
        ## icioNet_viz_plot_height <- isolate(icioNet_heightSize())
        ## ##
        ## diameter <- min(icioNet_viz_plot_height, icioNet_viz_plot_width)
        ## fontsize <- diameter / 100

        icionet_opacity = ifelse(is.null(result$icionet_opacity), 0.5, result$icionet_opacity)

        treeNetwork(List = ui.icionet.Flare,
                    fontSize = 10,
                    ## fontSize = fontsize,
                    opacity = 0.9,
                    ## opacity = icionet_opacity,
                    ## diameter = diameter,
                    ## width = icioNet_viz_plot_width,
                    ## height = icioNet_viz_plot_height,
                    margin=0)

        ## library(magrittr)
        ## library(networkD3)
        ## treeNetwork(List = ui.icionet.Flare) %>% saveNetwork(file = "Net1.html")

        ## treeNetworkOutput(outputId, width = "100%", height = "800px") {
        ##     shinyWidgetOutput(outputId, "treeNetwork", width, height,
        ##                       package = "networkD3")

    }
}

## #######################################
## ## Attempt to increase data preparation
## #######################################

## makeList <- function(x) {
##     item <- c(
##         ## as.list(test2[1, c("name", "size")])
##         list(name = x$name)
##        ,
##         list(size = x$size)
##        ,
##         list(imports = unname(as.list(as.character(x[c(2:6)]))))
##     )
##     return(item)
## }


## prepareData <- function(icioNet_data,
##                         icionet_indic,
##                         icionet_ind,
##                         icionet_year
##                         ) {

##     data.select <- icioNet_data %>%
##         filter(indic==icionet_indic & ind==icionet_ind & year==icionet_year) %>%
##             select(cou, par, value)
##     ##
##     cou.size <- data.select %>% filter(par == "NA.Total") %>% select(cou, size = value)
##     ##
##     cou.imports <- data.select
##     cou.imports <- arrange(cou.imports, cou, desc(value))
##     cou.imports <- group_by(cou.imports, cou)
##     ##
##     cou.imports <- filter(cou.imports, !par%in%c("NA.Total", "NA.ROW"))
##     cou.imports <- filter(cou.imports, min_rank(desc(value)) <= 5)
##     ##
##     cou.imports.m <- matrix(cou.imports$par, ncol = 5, byrow = TRUE)
##     ##
##     test <- data.frame(cou = unique(cou.imports$cou),
##                        par1 = cou.imports.m[, 1],
##                        par2 = cou.imports.m[, 2],
##                        par3 = cou.imports.m[, 3],
##                        par4 = cou.imports.m[, 4],
##                        par5 = cou.imports.m[, 5],
##                        stringsAsFactors = FALSE)
##     ##
##     test2 <- left_join(test, cou.size)
##     setnames(test2, "cou", "name")
##     ##
##     data.list <- list()
##     for (i in c(1:nrow(test2))) data.list <- c(data.list, list(makeList(test2[i, ])))

##     return(data.list)
## }



## create hierarchical list of industries

# require(networkD3)

# obj <- STANi3.HIERARCHY

# list.all <- list(name = "CTOTAL")
# list.all$children <- lapply(obj[["CTOTAL"]],
#                             function (x) {
#                               if(length(obj[[x]])==0)
#                                 list(name = x, size = 1)
#                               else
#                                 list(name = x,
#                                      children = lapply(obj[[x]],
#                                        function (x) {
#                                          if(length(obj[[x]])==0)
#                                            list(name = x, size = 1)
#                                          else
#                                            list(name = x,
#                                                 children = lapply(obj[[x]],
#                                                   function (x) {
#                                                     if(length(obj[[x]])==0)
#                                                       list(name = x, size = 1)
#                                                     else
#                                                       list(name = x,
#                                                            children = lapply(obj[[x]],
#                                                              function (x) {
#                                                                if(length(obj[[x]])==0)
#                                                                  list(name = x, size = 1)
#                                                                else
#                                                                  list(name = x,
#                                                                       children = lapply(obj[[x]],
#                                                                         function (x) {
#                                                                           if(length(obj[[x]])==0)
#                                                                             list(name = x, size = 1)
#                                                                           else
#                                                                             list(name = x,
#                                                                                  children = lapply(obj[[x]],
#                                                                                    function (y) {
#                                                                                      list(name = y, size = 1)
#                                                                                    }
#                                                                                    )
#                                                                                  )
#                                                                         }
#                                                                         )
#                                                                       )
#                                                              }
#                                                              )
#                                                            )
#                                                   }
#                                                   )
#                                                 )
#                                        }
#                                        )
#                                      )
#                             }
#                             )

## str(list.all)
        # treeNetwork(List = list.all,
        #             fontSize = 10,
        #             opacity = 0.9,
        #             margin = 0)
