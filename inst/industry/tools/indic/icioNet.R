
## Load miserables data for forceNetwork()

## system.file("miserables.json", package = "networkD3")
## path <- file.path(dbpath, "GitHub", "desk", "inst", "industry")
## setwd(path)
## library(networkD3)
## library(rjson)
## library(shiny)
## library(data.table)
## improve speed
## data.table
## install.packages("data.table")

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

## name = cou
## size = sum(par==total, ind==total)
## imports = top x par where ind==total
load(file.path("data", "data_init", "icioIndic.rda"))
icioIndic_data <- data.table(icioIndic_data)

icioIndic_nameindic <- as.character(unique(icioIndic_data$indic)) # only select indicators where par dimension available
icioIndic_nameyear <- unique(icioIndic_data$year)
icioIndic_namecou <- as.character(unique(icioIndic_data$cou))
icioIndic_namecou <- icioIndic_namecou[!icioIndic_namecou%in%c("CYP", "ROW")]
## icioIndic_namepar <- unique(icioIndic_data$par)
icioIndic_nameind <- as.character(unique(icioIndic_data$ind))

icioNet_namereg <- read.csv(file.path("data", "data_init", "icioIndic_namereg.csv"))
icioNet_namereg <- subset(data.table(icioNet_namereg), select = c("cou", "region"))



icioNet_namereg <- icioNet_namereg[icioNet_namereg$cou%in%c(icioIndic_namecou, "Total"),]
icioNet_namereg$region <- gsub("[ .-]", "", icioNet_namereg$region)

library(dplyr)

h(left_join(icioIndic_data, icioNet_namereg, by = c("cou" = "cou")))

setnames(icioNet_namereg, "region", "couregion")
icioNet_namereg$parregion <- icioNet_namereg$couregion
icioNet_namereg$par <- icioNet_namereg$cou

## names(icioIndic_data)


## data.fddva.totalind.2009 <- icioIndic_data[indic==icionet_indic &
##                                                ind==icionet_ind &
##                                                    year==icionet_year,]

data.fddva.totalind.2009.region <- data.fddva.totalind.2009

## h(data.fddva.totalind.2009.region)
## h(icioNet_namereg)

data.fddva.totalind.2009.region <- merge(data.fddva.totalind.2009.region,
                                         subset(icioNet_namereg, select = c("cou", "region")),
                                         by = "cou")
## names(data.fddva.totalind.2009.region) <- sub("region", "couregion", names(data.fddva.totalind.2009.region))
setnames(data.fddva.totalind.2009.region, "region", "couregion")

data.fddva.totalind.2009.region$cou <- paste0('flare.',
                                              as.character(data.fddva.totalind.2009.region$couregion),
                                              '.',
                                              as.character(data.fddva.totalind.2009.region$cou))

## h(data.fddva.totalind.2009.region)
## h(subset(icioNet_namereg, select = c("cou", "region")))
## h(data.fddva.totalind.2009.region)
data.fddva.totalind.2009.region <- merge(data.fddva.totalind.2009.region,
                                         subset(icioNet_namereg, select = c("cou", "region")),
                                         by.x = "par", by.y = "cou")
## by = c("par" = "cou"))
## data.fddva.totalind.2009.region <- data.table(data.fddva.totalind.2009.region)
## names(data.fddva.totalind.2009.region) <- sub("region", "parregion", names(data.fddva.totalind.2009.region))
setnames(data.fddva.totalind.2009.region, "region", "parregion")

data.fddva.totalind.2009.region$par <- paste0('flare.',
                                              as.character(data.fddva.totalind.2009.region$parregion),
                                              '.',
                                              as.character(data.fddva.totalind.2009.region$par))









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

        selectInput("icionet_indic", "Indicator:", icioIndic_nameindic, selected = "FDDVA", multiple = FALSE)
       ,
        selectInput("icionet_ind", "Industry:", icioIndic_nameind, selected = "Total", multiple = FALSE)
       ,
        selectInput("icionet_year", "Year:", icioIndic_nameyear, selected = 2009, multiple = FALSE)
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
               ,fun_tabs = c("clusterNetwork", "treemapNetwork")
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


    data.fddva.totalind.2009 <- subset(icioIndic_data,
                                       indic == icionet_indic &
                                           ind == icionet_ind &
                                               year == icionet_year)

    data.all <- NULL
    namereg <- sort(unique(icioNet_namereg$region))
    ## reg <- namereg[1]
    for (reg in namereg) {
        reg.namecou <- sort(icioNet_namereg$cou[icioNet_namereg$region==reg])

        ## country <- paste0("flare.", reg[1], '.', reg.namecou[1])
        for (country in paste0("flare.", reg, '.', reg.namecou)) {
            ## for (country in icioIndic_namecou) {
            data.fddva.totalind.2009.cou <- subset(data.fddva.totalind.2009.region, cou == country)

            ## data.fddva.totalind.2009.cou <- data.fddva.totalind.2009.region[cou==country,]
            ## flare: random, NA: no region in namereg
            cou.size <- subset(data.fddva.totalind.2009.cou, par == "flare.NA.Total")[, "value"]
            ## cou.size <- data.fddva.totalind.2009.cou[par == "flare.NA.Total", value]
            ##
            cou.imports <- subset(data.fddva.totalind.2009.cou, !par %in% c("flare.NA.Total", "ROW"))
            cou.imports <- cou.imports[order(-cou.imports$value),][1:icionet_nimports, "par"]
            ## cou.imports <- cou.imports[order(-cou.imports$value),][1:icionet_nimports, par]
            cou.imports.list <- list()
            for (import in cou.imports) {
                cou.imports.list <- c(cou.imports.list, list(import))
            }
            data.cou <- list(name = country,
            ## data.cou <- list(name = paste0("flare.", reglabel, ".", country),
                             size = cou.size,
                             imports = cou.imports.list)
            data.all <- c(data.all, list(data.cou))
        }
    }

    ## data.all
    ## str(data.all)
    ## data.all[[1]]
    ## ui.icionet.FlareImports[[1]]
    ## str(ui.icionet.FlareImports)


    return(
        list(## icionet_charttype = icionet_charttype,
            ## icionet_opacity = icionet_opacity,
            data.all = data.all,
            icioNet_viz_plot_width = icioNet_viz_plot_width,
            icioNet_viz_plot_height = icioNet_viz_plot_height
        )
           )
}

summary_icioNet <- function(result = .icioNet()) {
    if (length(result) > 0) {

        data.all <- result$data.all

        list.print <- NULL

        ## icionet_opacity <- result$icionet_opacity
        icioNet_viz_plot_width = result$icioNet_viz_plot_width
        ## icioNet_viz_plot_height = result$icioNet_viz_plot_height

        icioNet_viz_plot_height <- isolate(icioNet_heightSize())

        ## list.print <- c(list.print,
        ##                 list(## Opacity = icionet_opacity,
        ##                     Data = toString(data.all[[1]]),
        ##                     ## Data = toString("test"),
        ##                      Width = icioNet_viz_plot_width,
        ##                      Height = icioNet_viz_plot_height)
        ##                 )
        ## list.print <- toString(str(data.all))
        list.print <- toJSON(data.all)
        ## list.print <- toJSON(ui.icionet.FlareImports)

        ## return(list.print)
        return(cat(list.print))
    }
}

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

clusternetwork_icioNet <- function(result = .icioNet()) {
    if (length(result) > 0) {

        ## devtools::load_all(file.path(dbpath, "GitHub", "networkD3"))
        ## devtools::document(file.path(dbpath, "GitHub", "networkD3"))
        ## devtools::install(file.path(dbpath, "GitHub", "networkD3"))
        ## library(rjson)

        data.all <- result$data.all

        icioNet_viz_plot_width <- result$icioNet_viz_plot_width
        icioNet_viz_plot_height <- result$icioNet_viz_plot_height

        icionet_opacity = ifelse(is.null(result$icionet_opacity), 0.5, result$icionet_opacity)

        clusterNetwork(
            ## List = ui.icionet.FlareImports
            List = data.all
           ,
            height = icioNet_viz_plot_width
           ,
            width = icioNet_viz_plot_height
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

        data.all <- result$data.all

        icionet_opacity = ifelse(is.null(result$icionet_opacity), 0.5, result$icionet_opacity)

        treemapNetwork(
            ## List = ui.icionet.FlareImports
            List = data.all
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


        ## icionet_charttype <- result$icionet_charttype
        ## if (icionet_charttype=="network") {
        ## d3ForceNetwork(Nodes = ui.icionet.MisNodes,
        ##              Links = ui.icionet.MisLinks,
        ##              Source = "source",
        ##              Target = "target",
        ##              Value = "value",
        ##              NodeID = "name",
        ##              Group = "group",
        ##              ## width = 400,
        ##              ## height = 500,
        ##              width = icioNet_viz_plot_width,
        ##              height = icioNet_viz_plot_height,
        ##              opacity = icionet_opacity
        ##             ,
        ##              standAlone = FALSE,
        ##              parentElement = '#html_icioNet' # needs to be function name!!
        ##              )
        ## }
        ## if (icionet_charttype=="tree") {
        ##     diameter <- min(icioNet_viz_plot_height, icioNet_viz_plot_width)
        ##     fontsize <- diameter / 100
        ##     d3Tree(List = ui.icionet.Flare,
        ##            ## fontsize = 8,
        ##            fontsize = fontsize,
        ##            ## diameter = 800,
        ##            width = icioNet_viz_plot_width,
        ##            height = icioNet_viz_plot_height,
        ##            diameter = diameter,
        ##            opacity = icionet_opacity,
        ##            standAlone = FALSE,
        ##            parentElement = '#html_icioNet' # needs to be function name!!
        ##            )
        ## }
        ## if (icionet_charttype=="sankey") {
        ##     fontsize = min(icioNet_viz_plot_width, icioNet_viz_plot_height) / 50
        ##     d3Sankey(Links = ui.icionet.EngLinks,
        ##              Nodes = ui.icionet.EngNodes,
        ##              Source = "source",
        ##              Target = "target",
        ##              Value = "value",
        ##              NodeID = "name",
        ##              ## fontsize = 12,
        ##              fontsize = fontsize,
        ##              nodeWidth = 30,
        ##              ## width = 700,
        ##              width = icioNet_viz_plot_width,
        ##              height = icioNet_viz_plot_height,
        ##              standAlone = FALSE,
        ##              parentElement = '#html_icioNet' # needs to be function name!!
        ##              )
        ## }
