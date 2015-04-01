## ################
## Data preparation
## ################
data(stanDim)
##

## Custom industry conversion
ui.stani3Estimate.convind.table <- read.csv(file = file.path("data", "data_init", "stani3Estimate_convind.csv"), na = "")
## convind.string <- paste(readLines(file("data/data_init/stani3Estimate_convind.csv")), collapse = '\n')
## Table to drop information
ui.stani3Estimate.drop.table <- read.csv(file = file.path("data", "data_init", "stani3Estimate_drop.csv"), na = "")
## Source array
load(file = file.path("data", "data_init", "stani3Estimate_srcarray.rda"))
## ## Source data stani3Estimate.data.array
## load(file = file.path("data", "data_init", "stani3Estimate_dataarray.rda"))

names.VIS.SOU <- c("sou", "label", "col", "type", "lty", "pch", "in.ui", "estim", "unit", "isic")
stani3Estimate.VIS.SOU <- rbind.data.frame(
    c("",              "",                  "#4F81BD", "l", 5, NA,  FALSE, FALSE, 6, 3),
    c("",              "",                  "#C0504D", "l", 5, NA,  FALSE, FALSE, 6, 3),
    c("",              "",                  "#9BBB59", "l", 5, NA,  FALSE, FALSE, 6, 3),
    c("",              "",                  "#8064A2", "l", 5, NA,  FALSE, FALSE, 6, 3),
    c("",              "",                  "#4BACC6", "l", 5, NA,  FALSE, FALSE, 6, 3),
    c("",              "",                  "#F79646", "l", 5, NA,  FALSE, FALSE, 6, 3),
    c("",              "",                  "#95B3D7", "l", 5, NA,  FALSE, FALSE, 6, 3),
    c("",              "",                  "#DA9694", "l", 5, NA,  FALSE, FALSE, 6, 3),
    c("",              "",                  "#C4D79B", "l", 5, NA,  FALSE, FALSE, 6, 3),
    c("",              "",                  "#FABF8F", "l", 5, NA,  FALSE, FALSE, 6, 3),
    c("",              "",                  "#B1A0C7", "l", 5, NA,  FALSE, FALSE, 6, 3),
    c("",              "",                  "#92CDDC", "l", 5, NA,  FALSE, FALSE, 6, 3),
    c("BTDIXE",        "BTD\nIXE",          "#C0504D", "l", 1, NA,  FALSE, FALSE, 6, 3), # red
    c("EUNAIOR1",      "EU\nNAIO\nR1",      "black",   "l", 5, NA,  TRUE,  FALSE, 6, 3), # black, dashed
    c("EUNAMAR1",      "EU\nNAMA\nR1",      "#333399", "l", 1, NA,  TRUE,  FALSE, 6, 3), # dark blue
    c("EUNAMAR2",      "EU\nNAMA\nR2",      "#333399", "l", 1, NA,  TRUE,  FALSE, 6, 4), # dark blue
    c("ICIO052013",    "ICIO\n052013",      "black",   "p", 0, 19,  TRUE,  FALSE, 0, 3), # black, dots
    c("INDSTAT32",     "IND\nSTAT\n32",     "#8064A2", "l", 1, NA,  TRUE,  FALSE, 0, 3), # magenta
    c("NSONAPATCHi3",  "NSO\nNAPATCHi3",    "#C0504D", "l", 1, NA,  TRUE,  FALSE, 6, 3), # red
    c("NSONAPATCHi4",  "NSO\nNAPATCHi4",    "#C0504D", "l", 3, NA,  TRUE,  FALSE, 6, 4), # red
    c("OECDSUT112013", "OECD\nSUT\n112013", "#4F81BD", "l", 5, NA,  TRUE,  FALSE, 6, 3), # blue, dashed
    c("STANandBTD",    "STAN\nand\nBTD",    "#C0504D", "l", 1, NA,  TRUE,  FALSE, 6, 3), # red
    c("STANandBTDi4",  "STAN\nand\nBTDi4",  "#C0504D", "l", 5, NA,  TRUE,  FALSE, 6, 4), # red, dashed
    c("STDSNAi3",      "STD\nSNA\ni3",      "#333399", "l", 1, NA,  TRUE,  FALSE, 6, 3), # dark blue
    c("STDSNAi4",      "STD\nSNA\ni4",      "#333399", "l", 1, NA,  TRUE,  FALSE, 6, 4), # dark blue
    c("UNSDSNA2013",   "UNSD\nSNA\n2013",   "#F79646", "l", 1, NA,  TRUE,  FALSE, 6, 3), # orange
    c("UNDATA203CON",  "UNDATA\n203\nCON",  "#F79646", "l", 5, NA,  TRUE,  FALSE, 6, 3), # orange, dashed
    c("UNDATA203100",  "UNDATA\n203\n100",  "#F79646", "l", 1, NA,  TRUE,  FALSE, 6, 3), # orange
    c("UNDATA203200",  "UNDATA\n203\n200",  "#F79646", "l", 1, NA,  TRUE,  FALSE, 6, 3), # orange
    c("UNDATA203300",  "UNDATA\n203\n300",  "#F79646", "l", 1, NA,  TRUE,  FALSE, 6, 3), # orange
    c("UNDATA203400",  "UNDATA\n203\n400",  "#F79646", "l", 1, NA,  TRUE,  FALSE, 6, 3), # orange
    c("UNDATA203500",  "UNDATA\n203\n500",  "#F79646", "l", 1, NA,  TRUE,  FALSE, 6, 3), # orange
    c("WIOT042012",    "WIOT\n042012",      "#9BBB59", "l", 1, NA,  TRUE,  FALSE, 0, 3), # green
    c("WIOT112013",    "WIOT\n112013",      "#9BBB59", "l", 1, NA,  TRUE,  FALSE, 0, 3), # green
    c("PATCHEXT",      "PATCH\nEXT",        "#4F81BD", "b", 5, 0,  TRUE,  TRUE,  6, 3), # blue, dashed
    c("PATCHDET",      "PATCH\nDET",        "#4F81BD", "b", 5, 0,  TRUE,  TRUE,  6, 3) # blue, dashed
    ) # #FF3300 dark orange
names(stani3Estimate.VIS.SOU) <- names.VIS.SOU
stani3Estimate.VIS.SOU$col <- as.character(stani3Estimate.VIS.SOU$col)
stani3Estimate.VIS.SOU$type <- as.character(stani3Estimate.VIS.SOU$type)
stani3Estimate.VIS.SOU$lty <- as.numeric(as.character(stani3Estimate.VIS.SOU$lty))
stani3Estimate.VIS.SOU$pch <- as.numeric(as.character(stani3Estimate.VIS.SOU$pch))
##
ui.stani3Estimate.sou <- as.character(stani3Estimate.VIS.SOU$sou[stani3Estimate.VIS.SOU$in.ui==TRUE])
ui.stani3Estimate.sou.estim <- as.character(stani3Estimate.VIS.SOU$sou[stani3Estimate.VIS.SOU$estim==TRUE])
ui.stani3Estimate.sou.isic4 <- as.character(stani3Estimate.VIS.SOU$sou[stani3Estimate.VIS.SOU$isic==4])
##
ui.stani3Estimate.sou.ext <- c("", ui.stani3Estimate.sou)
ui.stani3Estimate.sou.det <- c("", ui.stani3Estimate.sou)
##
## names(stani3Estimate.VIS.SOU)

## UI lists
ui.stani3estimate.tabletype <- c("Data", "Calculation")
ui.stani3estimate.datatabletype <- c("Conversion", "Sources", "Hierarchy")
ui.stani3estimate.plottype <- c("Lines", "Bars")
##
ui.stani3Estimate.cou.agg <- names(STAN.COU)
ui.stani3Estimate.cou <- STAN.COU[["ICIO"]]
ui.stani3Estimate.cou <- ui.stani3Estimate.cou[!ui.stani3Estimate.cou=="ROW"]
##
## "A46" list not included; UNSDSNA C65T99 missing
## ui.stani3Estimate.ind.agg <- c("A6", "A18", "A34", "A34All")
ui.stani3Estimate.ind.agg <- c("A6", "A18", "A34", "A34All", "A60All")
ui.stani3Estimate.ind <- STANi3.INDA34All # reduce output
## ui.stani3Estimate.ind <- STANi3.INDA60All # defined in estimation array
ui.stani3Estimate.ind <- as.character(ui.stani3Estimate.ind)
ui.stani3Estimate.ind2 <- union("", ui.stani3Estimate.ind)
##
ui.stani3Estimate.var <- rbind.data.frame(
    c("VALU", "Value-Added"),
    c("PROD", "Output"),
    c("EMPN", "Total Engaged"),
    c("EMPE", "Employees"),
    c("GFCF", "Gross fixed capital formation"),
    c("LABR", "Labour cost"),
    c("NOPS", "Net operating profits"),
    c("OTXS", "Other taxes less subsidies"),
    c("EXPO", "Gross Exports"),
    c("IMPO", "Gross Imports"),
    c("FDDE", "Final Domestic Demand Expenditure"),
    c("FHHE", "Final Household Demand Expenditure"),
    c("FGGE", "Final Gouvernment Demand Expenditure")
    )
## tools/help/stani3Estimate.md
ui.stani3Estimate.var <- ui.stani3Estimate.var[,1]
ui.stani3Estimate.var <- as.character(ui.stani3Estimate.var)
ui.stani3Estimate.var2 <- union("", ui.stani3Estimate.var)

## ## Array for estimation
## ui.stani3Estimate.est <- c("EXT", "DET")
## ui.stani3Estimate.est.sou <- c("MAIN", "SEC")
## ui.stani3Estimate.est.array <- array(dim = c(
##                                      length(ui.stani3Estimate.cou),
##                                      length(ui.stani3Estimate.var),
##                                      length(ui.stani3Estimate.ind),
##                                      length(ui.stani3Estimate.est),
##                                      length(ui.stani3Estimate.est.sou)),
##                                      dimnames = list(
##                                      cou = ui.stani3Estimate.cou,
##                                      var = ui.stani3Estimate.var,
##                                      ind = ui.stani3Estimate.ind,
##                                      est = ui.stani3Estimate.est,
##                                      sou = ui.stani3Estimate.est.sou))

## ## ####################################################################### ##
## ## procedure to copy array settings to new array with different dimensions ##
## ## ####################################################################### ##
## dimnames(ui.stani3Estimate.est.array)
## ui.stani3Estimate.est.array.old <- ui.stani3Estimate.est.array
## dim(ui.stani3Estimate.est.array.old)
## ## List for estimation
## ui.stani3Estimate.est <- c("EXT", "DET")
## ui.stani3Estimate.est.sou <- c("MAIN", "SEC")
## ui.stani3Estimate.est.array <- array(dim = c(
##                                      length(ui.stani3Estimate.cou),
##                                      length(ui.stani3Estimate.var),
##                                      length(ui.stani3Estimate.ind),
##                                      length(ui.stani3Estimate.est),
##                                      length(ui.stani3Estimate.est.sou)),
##                                      dimnames = list(
##                                      cou = ui.stani3Estimate.cou,
##                                      var = ui.stani3Estimate.var,
##                                      ind = ui.stani3Estimate.ind,
##                                      est = ui.stani3Estimate.est,
##                                      sou = ui.stani3Estimate.est.sou))
## dim(ui.stani3Estimate.est.array)
## ##
## ## format: cou, var, ind, ext, sou
## for (ind in dimnames(ui.stani3Estimate.est.array)[["ind"]]) {
##     ui.stani3Estimate.est.array[, , ind, , ] <- ui.stani3Estimate.est.array.old[, , "CTOTAL", , ]
## }
## dimnames(ui.stani3Estimate.est.array)
## ## save(ui.stani3Estimate.est.array.old, file = "data/data_init/stani3Estimate_srcarray_prev.rda")
## save(ui.stani3Estimate.est.array, file = "data/data_init/stani3Estimate_srcarray.rda")

## ## ###################### ##
## ## Export srcarray to csv ##
## ## ###################### ##
## load("data/data_init/stani3Estimate_srcarray.rda") # moved to top
## ## Example modify:
## ## ui.stani3Estimate.est.array["AUT", "VALU", "C15T37", "EXT",] <- c("STANandBTD", "STANandBTDi4")
## ## Export to csv:
## srcarray.export <- melt(ui.stani3Estimate.est.array)
## srcarray.export <- subset(srcarray.export, ind == "CTOTAL" & var%in%c("VALU", "PROD", "EMPN", "LABR"))
## srcarray.export <- srcarray.export[order(srcarray.export$cou, srcarray.export$var, srcarray.export$est, srcarray.export$sou),]
## write.csv(srcarray.export, file = "data/data_init/stani3Estimate_srcarray.csv", row.names = FALSE)

## ####################################################################### ##
## procedure to change name of source, e.g. "NSONAPATCH" to "NSONAPATCHi3" ##
## ####################################################################### ##
## ui.stani3Estimate.est.array[ui.stani3Estimate.est.array=="NSONAPATCH"] <- "NSONAPATCHi3"
## ui.stani3Estimate.est.array[ui.stani3Estimate.est.array=="NSONAPATCHi3"]
## save(ui.stani3Estimate.est.array, file = "data/data_init/stani3Estimate_srcarray.rda")



## UIs
output$uiSe_diff <- renderUI({
    wellPanel(
        h5("Calculation:"),
        checkboxInput("stani3estimate_diff", "Difference (Default: Ratio)", FALSE)
        )
})
##
output$uiSe_plot <- renderUI({
    if ((input$tabs_stani3Estimate=="Plots" & input$stani3estimate_plottype=="Lines") |
        (input$tabs_stani3Estimate=="Tables" & input$stani3estimate_tabletype=="Calculation"))
        ## if (input$stani3estimate_var2!="" | input$stani3estimate_ind2!="")
    {
        list(
            sliderInput("stani3estimate_yrange",
                        "Y-axis Range (Ratio):",
                        value = c(0,1),
                        min = 0,
                        max = 1,
                        step = 0.05,
                        format="#.##"),
            checkboxInput("stani3estimate_limit.yrange.max", "Y-axis Range: Activate Maximum", FALSE),
            checkboxInput("stani3estimate_limit.yrange.min", "Y-axis Range: Activate Minimum", FALSE)
            )
    }
})
##
output$uiSe_ind <- renderUI({
    ## if (length(result) > 0) {
        if ((input$tabs_stani3Estimate=="Plots" & input$stani3estimate_plottype=="Lines") |
            (input$tabs_stani3Estimate=="Tables" & input$stani3estimate_tabletype=="Calculation") )
        {
            selectInput("stani3estimate_ind", "Industry:", ui.stani3Estimate.ind,
                        ## selected = state_init_list("stani3estimate_ind", "C15T37", ui.stani3Estimate.ind),
                        selected = "C15T37",
                        ## selected = "C75T99",
                        multiple = FALSE,
                        selectize = FALSE
                        )
        } else {
            selectInput("stani3estimate_ind", "Industry:", union(ui.stani3Estimate.ind.agg, ui.stani3Estimate.ind),
                        ## selected = state_init_multvar("stani3estimate_ind", "CTOTAL", union(ui.stani3Estimate.ind.agg, ui.stani3Estimate.ind)),
                        ## selected = "CTOTAL",
                        selected = "C15T37",
                        multiple = TRUE
                        )
        }
    ## }
})
##
output$uiSe_indParent <- renderUI({
    ## if (length(result) > 0) {
    ## ind.parent <- result$ind.parent
    ind.parent <- as.character(STANi3.HIERARCHYINV[[input$stani3estimate_ind]])
        if (input$stani3estimate_detail==TRUE) {
            selectInput("stani3estimate_ind.parent.select", "Show parent level", ind.parent,
                        ## selected = state_init_multvar("stani3estimate_ind.parent.select", ind.parent[1], ind.parent), # ind.parent[1]
                        ## selected = "C10T41",
                        selected = ind.parent[1],
                        multiple = TRUE)
        }
    ## }
})
##
output$uiSe_indPeers <- renderUI({
    ## if (length(result) > 0) {
        ## ind.parent <- result$ind.parent
        ind.parent <- input$stani3estimate_ind.parent.select
        ## ind.peers <- ind.peers()
        if (input$stani3estimate_detail==TRUE) {
            ind.peers <- as.character(STANi3.HIERARCHY[[ind.parent[1]]])
            selectInput("stani3estimate_ind.peers.select", "Selected peer industries", ind.peers,
                        ## selected = state_init_multvar("stani3estimate_ind.peers.select", input$stani3estimate_ind, ind.peers), # input$stani3estimate_ind
                        ## selected = "C15T37",
                        selected = input$stani3estimate_ind,
                        multiple = TRUE)
        }
    ## }
})
##
output$uiSe_estExtend <- renderUI({
    ##
    if (input$stani3estimate_sou_ext_reloadButton != 0) {
        load(file = file.path("data", "data_init", "stani3Estimate_srcarray.rda"))
    }
    ##
    sou.ext <- ui.stani3Estimate.est.array[input$stani3estimate_cou,
                                           input$stani3estimate_var,
                                           input$stani3estimate_ind,
                                           "EXT",]
    sou.ext.main <- ifelse(is.na(sou.ext[1]), '', sou.ext[1])
    if(is.na(sou.ext[2])) sou.ext.sec <- '' else sou.ext.sec <- strsplit(sou.ext[2], split = ", ")[[1]]
    ##
    if (input$stani3estimate_extend==TRUE)
    {
        list(
            selectInput("stani3estimate_sou_ext_main", "Extend: Main Source", ui.stani3Estimate.sou.ext,
                        selected = sou.ext.main, multiple = FALSE) # sou.ext.main
            ,
            conditionalPanel(condition="input.stani3estimate_sou_ext_main!=''",
                             selectInput("stani3estimate_sou_ext_sec", "Extend: Secondary Sources", ui.stani3Estimate.sou.ext,
                                         ## selected = state_multvar(sou.ext.sec, ui.stani3Estimate.sou.ext),
                                         selected = sou.ext.sec, # sou.ext.sec
                                         multiple = TRUE)
                             )
            )
    }
})
##
output$uiSe_estDetail <- renderUI({
    ##
    if (input$stani3estimate_sou_det_reloadButton != 0) {
        load(file = file.path("data", "data_init", "stani3Estimate_srcarray.rda"))
    }
    ##
    sou.det <- ui.stani3Estimate.est.array[input$stani3estimate_cou,
                                           input$stani3estimate_var,
                                           input$stani3estimate_ind,
                                           "DET",]
    sou.det.main <- ifelse(is.na(sou.det[1]), '', sou.det[1])
    if(is.na(sou.det[2])) sou.det.sec <- '' else sou.det.sec <- strsplit(sou.det[2], split = ", ")[[1]]
    ##
    if (input$stani3estimate_detail==TRUE)
    {
        list(
            selectInput("stani3estimate_sou_det_main", "Detail: Main Source", ui.stani3Estimate.sou.det,
                        selected = sou.det.main,
                        multiple = FALSE)
            ,
            conditionalPanel(condition="input.stani3estimate_sou_det_main!=''",
                             selectInput("stani3estimate_sou_det_sec", "Detail: Secondary Sources", ui.stani3Estimate.sou.det,
                                         selected = sou.det.sec,
                                         multiple = TRUE)
                             )
            )
    }
})

output$uiSe_aceEditor <- renderUI({

    if (input$stani3estimate_edit_reloadButton==0) {
        return()
    }
    if (input$stani3estimate_edit_reloadButton!=0) {
        ## convind or drop
        csvfile <- file.path("data", "data_init", paste0('stani3Estimate_', input$stani3estimate_edit_selectfile, '.csv'))
        csvfile.string <- paste(readLines(file(csvfile)), collapse = '\n')
        close(con = file(csvfile))
    }

    aceEditor("stani3estimate_aceeditor", mode = "r", height = "400px", value = csvfile.string)

})

output$ui_stani3Estimate <- renderUI({

    ## doLogin()
    ## if (loginData$LoggedIn) {

        list(

            conditionalPanel(condition="input.tabs_stani3Estimate=='Plots'",
                             wellPanel(
                                 checkboxInput("stani3estimate_viz_plot_controls", "Plot options", FALSE),
                                 conditionalPanel(condition = "input.stani3estimate_viz_plot_controls==true",
                                                  sliderInput(inputId = "stani3Estimate_viz_plot_height", label = "Height:", min = 400, max = 2000, value = 750, step = 50),
                                                  sliderInput(inputId = "stani3Estimate_viz_plot_width", label = "Width:", min = 600, max = 1200, value = 850, step = 50),
                                                  conditionalPanel(condition="input.stani3estimate_plottype=='Bars'",
                                                                   checkboxInput("stani3estimate_horiz", "Horizontal plot", FALSE)
                                                                   )
                                                  ) # conditionalPanel
                                 ) # wellPanel
                             ,
                             selectInput("stani3estimate_plottype", "Plottype:", ui.stani3estimate.plottype,
                                         selected = "Lines", multiple = FALSE),
                             conditionalPanel(condition="input.stani3estimate_plottype=='Bars'",
                                              selectInput("stani3estimate_singlesou", "Source:", ui.stani3Estimate.sou,
                                                          selected = "STANandBTD", multiple = FALSE)
                                              ) # conditionalPanel
                             )
            ,
            conditionalPanel(condition="input.tabs_stani3Estimate=='Tables'",
                             selectInput("stani3estimate_tabletype", "Tabletype:", ui.stani3estimate.tabletype,
                                         selected = "Data", multiple = FALSE)
                             ) # conditionalPanel
            ,
            conditionalPanel(condition="input.tabs_stani3Estimate=='DataTables'",
                             selectInput("stani3estimate_datatabletype", "DataTabletype:", ui.stani3estimate.datatabletype,
                                         selected = "Conversion", multiple = FALSE) # "Sources"
                             ) # conditionalPanel
            ,
            wellPanel(
                selectInput("stani3estimate_cou", "Country:", ui.stani3Estimate.cou, selected = "AUT", multiple = FALSE)
                )
            ,
            ## Variables
            wellPanel(
                selectInput("stani3estimate_var", "Variable:", ui.stani3Estimate.var, selected = "VALU", multiple = FALSE),
                conditionalPanel(condition="input.stani3estimate_ind2==''",
                                 selectInput("stani3estimate_var2", "Denominator or substractor variable:", ui.stani3Estimate.var2)
                                 ) # conditionalPanel
                )
            ,
            ## Industries
            wellPanel(
                htmlOutput("uiSe_ind"),
                conditionalPanel(condition="input.tabs_stani3Estimate=='DataTables'",
                                 checkboxInput("stani3estimate_STANi3.INDLABEL", "ISIC3 Industry label", FALSE)
                                 ) # conditionalPanel
                ,
                conditionalPanel(condition="input.tabs_stani3Estimate=='Tables'",
                                 checkboxInput("stani3estimate_missing", "Show missing industries", FALSE)
                                 ) # conditionalPanel
                ,
                conditionalPanel(condition="input.stani3estimate_var2==''",
                                 selectInput("stani3estimate_ind2", "Denominator or substractor industry:", ui.stani3Estimate.ind2)
                                 ) # conditionalPanel
                )
            ,
            conditionalPanel(condition="input.stani3estimate_var2!='' | input.stani3estimate_ind2!=''",
                             htmlOutput("uiSe_diff")
                             ) # conditionalPanel
            ,
            htmlOutput("uiSe_plot"),
            wellPanel(
                sliderInput("stani3estimate_time", "Time Range:", value = c(1994,2012), min = 1980, max = 2012, step = 1, format="#")
                )
            ,
            wellPanel(
                checkboxInput("stani3estimate_extend", "Extend series", FALSE)
                ,
                conditionalPanel(condition="input.stani3estimate_extend==true",
                                 actionButton("stani3estimate_sou_ext_reloadButton", "reload"),
                                 htmlOutput("uiSe_estExtend"),
                                 actionButton("stani3estimate_sou_ext_saveButton", "save"),
                                 helpText('\n"save" overwrites current setup of sources for dimension members selected in export section.')
                                 ) # conditionalPanel
                )
            ,
            wellPanel(
                checkboxInput("stani3estimate_detail", "Apply detailed industry share", FALSE),
                conditionalPanel(condition="input.stani3estimate_detail==true",
                                 actionButton("stani3estimate_sou_det_reloadButton", "reload"),
                                 htmlOutput("uiSe_estDetail"),
                                 actionButton("stani3estimate_sou_det_saveButton", "save"),
                                 helpText('\n"save" overwrites current setup of sources for dimension members selected in export section.'),
                                 wellPanel(
                                     htmlOutput("uiSe_indParent"),
                                     htmlOutput("uiSe_indPeers")
                                     )  # wellPanel
                                 ) # conditionalPanel
                )
            ,
            wellPanel(
                checkboxInput("stani3estimate_souall", "Select all sources", TRUE),
                conditionalPanel(condition="input.stani3estimate_souall==false",
                                 selectInput("stani3estimate_sou", "Source:", as.list(ui.stani3Estimate.sou), selected = c("STANandBTD", "STANandBTDi4", "EUNAMAR1", "EUNAMAR2"), multiple = TRUE)
                                 ) # conditionalPanel
                ) # wellPanel
            ,
            wellPanel(
                h5("Export Estimates"),
                wellPanel(
                    helpText("Sources for estimation will be saved for all countries and variables selected in this section. The country selection for region aggregates can be seen from the summary text. By default, estimation sources are updated for all industries."),
                    checkboxInput("stani3estimate_allind", "'Save' applies source selection to all industries", TRUE),
                    selectInput("stani3estimate_exportcou", "Export countries:", union(ui.stani3Estimate.cou.agg, ui.stani3Estimate.cou),
                                selected = c(""), multiple = TRUE),
                    selectInput("stani3estimate_morevar", "Export variables:", ui.stani3Estimate.var, selected = c("LABR", "EMPN", "PROD", "VALU"), multiple = TRUE)
                    ) # wellPanel
                ,
                wellPanel(
                    checkboxInput("stani3estimate_update", "Update results (check to perform estimation, uncheck to export stored results)", TRUE)
                    ) # wellPanel
                ,
                wellPanel(
                    checkboxInput("stani3estimate_exportcsv", "Export flat file (csv)", TRUE),
                    checkboxInput("stani3estimate_exportxls", "Export Excel table(s)", TRUE),
                    checkboxInput("stani3estimate_exportcalc", "Include calculations", TRUE)
                    ) # wellPanel
                ,
                downloadButton('download_stani3Estimate', 'Download ZIP file') # from radiant.R: paste0('download_', fun_label)
                ) # wellPanel
            ,

            wellPanel(
                h5("Edit system files"),
                selectInput("stani3estimate_edit_selectfile", "select file",
                            choices = c("convind", "drop"), multiple = FALSE),
                htmlOutput("uiSe_aceEditor"),
                actionButton("stani3estimate_edit_reloadButton", "load file"),
                ## conditionalPanel(condition="input.stani3estimate_edit_reloadButton!=0",
                actionButton("stani3estimate_edit_saveButton", "save changes"),
                ## conditionalPanel(condition="input.stani3estimate_edit_saveButton!=0",
                actionButton("stani3estimate_edit_applyButton", "export source data"),
                ## conditionalPanel(condition="input.stani3estimate_edit_applyButton!=0,"
                ## actionButton("stani3estimate_srcdata_reloadButton", "reload source data")
                actionButton("stani3estimate_dataarray_reloadButton", "reload source data")
                ##                                   )
                ##                  )
                ## )
                ) # wellPanel
            ,

            helpAndReport("STAN ISIC3 Estimate","stani3Estimate",inclMD(file.path("tools", "help", "stani3Estimate.md")))
            ) # list(...

    ## } else
    ## {
    ##     h3("Please log in")
    ## }

})

stani3Estimate_widthSize <- reactive({
    ifelse(is.null(input$stani3Estimate_viz_plot_width), return(values$plotWidth), return(input$stani3Estimate_viz_plot_width))
})
stani3Estimate_heightSize <- reactive({
    ifelse(is.null(input$stani3Estimate_viz_plot_height), return(values$plotHeight), return(input$stani3Estimate_viz_plot_height))
})

output$stani3Estimate <- renderUI({
    ## for input-output
    statTabPanel(menu_name = "STAN", # menu_name: for side bar - coincide with navbarMenu
                 fun_name = "STAN ISIC3 Estimate",   # fun_name
                 rfun_label = ".stani3Estimate", # rfun_label
                 fun_label = "stani3Estimate" # fun_label
                 ## ,rChart_lib = input$stani3estimate_rchartlib
                 ,fun_tabs = c("Tables", "Plots", "DataTables") # , "Ace")
                 ,widthFun = "stani3Estimate_widthSize"
                 ,heightFun = "stani3Estimate_heightSize"
                 )
})


## ## ######################
## ## Input test
## ## ######################
## input <- list(
##     nav_radiant="STAN ISIC3 Estimate",
##     ## stani3estimate_allcou=FALSE,
##     stani3estimate_exportcou=c("COU","COUENO"),
##     ## stani3estimate_allvar=FALSE,
##     stani3estimate_morevar=c("VALU","PROD"),
##     stani3estimate_allind=TRUE,
##     tabs_stani3Estimate=1,
##     stani3estimate_cou="JPN", # VNM
##     stani3estimate_detail=FALSE,
##     stani3estimate_diff=FALSE,
##     stani3estimate_extend=TRUE,
##     stani3estimate_horiz=FALSE,
##     stani3estimate_ind="C30", # C15T16
##     stani3estimate_ind2="",
##     stani3estimate_ind.peers.select="C15T16",
##     stani3estimate_ind.parent.select=NULL,
##     stani3estimate_limit.yrange.max=FALSE,
##     stani3estimate_limit.yrange.min=FALSE,
##     stani3estimate_missing=FALSE,
##     stani3estimate_singlesou="UNSDSNA2013",
##     stani3estimate_sou=c("INDSTAT32", "UNSDSNA2013"),
##     stani3estimate_souall=FALSE,
##     stani3estimate_sou_det_main="PATCHEXT",
##     stani3estimate_sou_det_sec="INDSTAT32",
##     stani3estimate_sou_det_sec2="",
##     stani3estimate_sou_det_sec3="",
##     stani3estimate_sou_ext_main="STANandBTD",
##     stani3estimate_sou_ext_sec=c("STANandBTDi4", "UNDATA203CON", "UNSDSNA2013"),
##     stani3estimate_sou_ext_sec2="",
##     stani3estimate_sou_ext_sec3="",
##     stani3estimate_STANi3.INDA6=FALSE,
##     stani3estimate_STANi3.INDA18=FALSE,
##     stani3estimate_STANi3.INDA34=FALSE,
##     stani3estimate_STANi3.INDA46=FALSE,
##     stani3estimate_STANi3.INDA60=FALSE,
##     stani3estimate_STANi3.INDLABEL=FALSE,
##     stani3estimate_time=c(1994, 2011),
##     stani3estimate_var="VALU",
##     stani3estimate_var2="",
##     stani3Estimate_viz_plot_height = 650,
##     stani3Estimate_viz_plot_width = 650
##     )
## tabs_stani3Estimate = input$tabs_stani3Estimate
## ##
## ## stani3estimate_allcou = input$stani3estimate_allcou
## stani3estimate_exportcou = input$stani3estimate_exportcou
## stani3estimate_allind = input$stani3estimate_allind
## stani3estimate_cou = input$stani3estimate_cou
## stani3estimate_detail = input$stani3estimate_detail
## stani3estimate_diff = input$stani3estimate_diff
## stani3estimate_extend = input$stani3estimate_extend
## stani3estimate_horiz = input$stani3estimate_horiz
## stani3estimate_ind = input$stani3estimate_ind
## stani3estimate_ind2 = input$stani3estimate_ind2
## stani3estimate_ind.peers.select = input$stani3estimate_ind.peers.select
## stani3estimate_ind.parent.select = input$stani3estimate_ind.parent.select
## stani3estimate_limit.yrange.max = input$stani3estimate_limit.yrange.max
## stani3estimate_limit.yrange.min = input$stani3estimate_limit.yrange.min
## stani3estimate_missing = input$stani3estimate_missing
## stani3estimate_singlesou = input$stani3estimate_singlesou
## stani3estimate_sou = input$stani3estimate_sou
## stani3estimate_souall = input$stani3estimate_souall
## stani3estimate_sou_det_main = input$stani3estimate_sou_det_main
## stani3estimate_sou_det_sec = input$stani3estimate_sou_det_sec
## stani3estimate_sou_det_sec2 = input$stani3estimate_sou_det_sec2
## stani3estimate_sou_det_sec3 = input$stani3estimate_sou_det_sec3
## stani3estimate_sou_ext_main = input$stani3estimate_sou_ext_main
## stani3estimate_sou_ext_sec = input$stani3estimate_sou_ext_sec
## stani3estimate_sou_ext_sec2 = input$stani3estimate_sou_ext_sec2
## stani3estimate_sou_ext_sec3 = input$stani3estimate_sou_ext_sec3
## stani3estimate_STANi3.INDA6 = input$stani3estimate_STANi3.INDA6
## stani3estimate_STANi3.INDA18 = input$stani3estimate_STANi3.INDA18
## stani3estimate_STANi3.INDA34 = input$stani3estimate_STANi3.INDA34
## stani3estimate_STANi3.INDA46 = input$stani3estimate_STANi3.INDA46
## stani3estimate_STANi3.INDA60 = input$stani3estimate_STANi3.INDA60
## stani3estimate_STANi3.INDLABEL = input$stani3estimate_STANi3.INDLABEL
## stani3estimate_time = input$stani3estimate_time
## stani3estimate_var = input$stani3estimate_var
## stani3estimate_var2 = input$stani3estimate_var2
## stani3Estimate_viz_plot_height = input$stani3Estimate_viz_plot_height
## stani3Estimate_viz_plot_width = input$stani3Estimate_viz_plot_width


.stani3Estimate <- reactive({
    ## reactive that calls the function for main analysis
    ## . used to indicate this is an 'internal' function
    ##
    ## if (length(input$stani3estimate_dimS) == 0) return ()
    ##

    stani3Estimate(
        stani3estimate_allind = input$stani3estimate_allind,
        stani3estimate_cou = input$stani3estimate_cou,
        stani3estimate_datatabletype = input$stani3estimate_datatabletype,
        stani3estimate_detail = input$stani3estimate_detail,
        stani3estimate_diff = input$stani3estimate_diff,
        stani3estimate_exportcalc = input$stani3estimate_exportcalc,
        stani3estimate_exportcou = input$stani3estimate_exportcou,
        stani3estimate_exportcsv = input$stani3estimate_exportcsv,
        stani3estimate_exportxls = input$stani3estimate_exportxls,
        stani3estimate_extend = input$stani3estimate_extend,
        stani3estimate_horiz = input$stani3estimate_horiz,
        stani3estimate_ind = input$stani3estimate_ind,
        stani3estimate_ind2 = input$stani3estimate_ind2,
        stani3estimate_ind.peers.select = input$stani3estimate_ind.peers.select,
        stani3estimate_ind.parent.select = input$stani3estimate_ind.parent.select,
        stani3estimate_limit.yrange.max = input$stani3estimate_limit.yrange.max,
        stani3estimate_limit.yrange.min = input$stani3estimate_limit.yrange.min,
        stani3estimate_missing = input$stani3estimate_missing,
        stani3estimate_morevar = input$stani3estimate_morevar,
        stani3estimate_plottype = input$stani3estimate_plottype,
        stani3estimate_singlesou = input$stani3estimate_singlesou,
        stani3estimate_sou = input$stani3estimate_sou,
        stani3estimate_souall = input$stani3estimate_souall,
        stani3estimate_sou_det_main = input$stani3estimate_sou_det_main,
        stani3estimate_sou_det_sec = input$stani3estimate_sou_det_sec,
        stani3estimate_sou_ext_main = input$stani3estimate_sou_ext_main,
        stani3estimate_sou_ext_sec = input$stani3estimate_sou_ext_sec,
        stani3estimate_sou_ext_reloadButton = input$stani3estimate_sou_ext_reloadButton,
        stani3estimate_sou_ext_saveButton = input$stani3estimate_sou_ext_saveButton,
        ## stani3estimate_STANi3.INDA6 = input$stani3estimate_STANi3.INDA6,
        ## stani3estimate_STANi3.INDA18 = input$stani3estimate_STANi3.INDA18,
        ## stani3estimate_STANi3.INDA34 = input$stani3estimate_STANi3.INDA34,
        ## stani3estimate_STANi3.INDA46 = input$stani3estimate_STANi3.INDA46,
        ## stani3estimate_STANi3.INDA60 = input$stani3estimate_STANi3.INDA60,
        stani3estimate_STANi3.INDLABEL = input$stani3estimate_STANi3.INDLABEL,
        stani3estimate_tabletype = input$stani3estimate_tabletype,
        stani3estimate_time = input$stani3estimate_time,
        stani3estimate_update = input$stani3estimate_update,
        stani3estimate_var = input$stani3estimate_var,
        stani3estimate_var2 = input$stani3estimate_var2,
        stani3estimate_yrange = input$stani3estimate_yrange,
        ##
        stani3Estimate_viz_plot_height = input$stani3Estimate_viz_plot_height,
        stani3Estimate_viz_plot_width = input$stani3Estimate_viz_plot_width
        )

})


## observe({
##     ## ui.stani3Estimate.convind.table <- read.csv("data/data_init/stani3Estimate_convind.csv", na = "")
##     readLines(file("data/data_init/stani3Estimate_convind.csv"))
## })

## Source Array modification
observe({
    if (is.null(input$stani3estimate_sou_ext_saveButton) || input$stani3estimate_sou_ext_saveButton == 0) return()
    ## apply settings to all selected countries: dangerous!!
    ## cou <- match(union(input$stani3estimate_cou, input$stani3estimate_exportcou), ui.stani3Estimate.cou)
    cou <- match(input$stani3estimate_cou, ui.stani3Estimate.cou)
    var <- match(union(input$stani3estimate_var, input$stani3estimate_morevar), ui.stani3Estimate.var)
    if (input$stani3estimate_allind==TRUE) {
        ind <- seq(along = ui.stani3Estimate.ind)
    } else {
        ind <- match(input$stani3estimate_ind, ui.stani3Estimate.ind)
    }
    load(file = file.path("data", "data_init", "stani3Estimate_srcarray.rda"))
    ui.stani3Estimate.est.array[cou,var,ind,"EXT","MAIN"] <- input$stani3estimate_sou_ext_main
    ui.stani3Estimate.est.array[cou,var,ind,"EXT","SEC"] <- toString(input$stani3estimate_sou_ext_sec)
    save(ui.stani3Estimate.est.array, file = file.path("data", "data_init", "stani3Estimate_srcarray.rda"))
    print("saved extend information")
})
##
observe({
    if (is.null(input$stani3estimate_sou_det_saveButton) || input$stani3estimate_sou_det_saveButton == 0) return()
    ## apply settings to all selected countries: dangerous!!
    ## cou <- match(union(input$stani3estimate_cou, input$stani3estimate_exportcou), ui.stani3Estimate.cou)
    cou <- match(input$stani3estimate_cou, ui.stani3Estimate.cou)
    var <- match(union(input$stani3estimate_var, input$stani3estimate_morevar), ui.stani3Estimate.var)
    if (input$stani3estimate_allind==TRUE) {
        ind <- seq(along = ui.stani3Estimate.ind)
    } else {
        ind <- match(input$stani3estimate_ind, ui.stani3Estimate.ind)
    }
    ## REDUNDANT - COPY FROM ABOVE
    load(file = file.path("data", "data_init", "stani3Estimate_srcarray.rda"))
    ui.stani3Estimate.est.array[cou,var,ind,"DET","MAIN"] <- input$stani3estimate_sou_det_main
    ui.stani3Estimate.est.array[cou,var,ind,"DET","SEC"] <- toString(input$stani3estimate_sou_det_sec)
    save(ui.stani3Estimate.est.array, file = file.path("data", "data_init", "stani3Estimate_srcarray.rda"))
    print("saved detail information")
})

## save modifications to industry conversion
observe({
    if (is.null(input$stani3estimate_edit_saveButton) || input$stani3estimate_edit_saveButton == 0) return()

    csvfile <- file.path("data", "data_init", paste0('stani3Estimate_', input$stani3estimate_edit_selectfile, '.csv'))
    csvfile.string <- input$stani3estimate_aceeditor
    ## readLines(file(file.path("data", "data_init", "stani3Estimate_convind.csv")))
    writeLines(text = csvfile.string, con = file(csvfile))
    close(con = file(csvfile))
    print(paste('saved editor text to', csvfile))
    ## revise - stored "drop" as "convind"
    if (input$stani3estimate_edit_selectfile=="convind") {
        ui.stani3Estimate.convind.table <- read.csv(file = file.path("data", "data_init", "stani3Estimate_convind.csv"), na = "")
    } else if (input$stani3estimate_edit_selectfile=="drop") {
        ui.stani3Estimate.drop.table <- read.csv(file = file.path("data", "data_init", "stani3Estimate_drop.csv"), na = "")
    }

})

## reload data to apply modified industry conversion
observe({
    if (is.null(input$stani3estimate_edit_applyButton) || input$stani3estimate_edit_applyButton == 0) return()

    ## necessary to reload here?
    ui.stani3Estimate.convind.table <- read.csv(file = file.path("data", "data_init", "stani3Estimate_convind.csv"), na = "")
    ui.stani3Estimate.drop.table <- read.csv(file = file.path("data", "data_init", "stani3Estimate_drop.csv"))

    dat <- isolate(values[["STANNAi3"]])
    ## create data sets in global memory from "values" object
    for (sou in names(dat)) {
        eval(parse(text = paste0(sou, ' <- dat$', sou)))
    }
    DATA.STANandBTD <- rbind(DATA.STANi3, DATA.BTDi3)
    ## data in national currency units - not in USD - need to apply USD conversion
    dat <- isolate(values[["STANNAi4"]])
    ## sou <- "DATA.EUNAMAR2"
    for (sou in names(dat)) {
        eval(parse(text = paste0(sou, ' <- dat$', sou)))
    }
    DATA.STANi4 <- DATA.STANi4[DATA.STANi4$var%in%DATA.STANandBTD$var,]
    DATA.BTDi4 <- DATA.BTDi4[DATA.BTDi4$var%in%DATA.STANandBTD$var,]

    ## ## STAN ISIC Rev. 4 now in USD - don't apply EXCH here
    ## ## currency exchange rates
    ## dat <- isolate(values[["STANNAi0"]])
    ## for (sou in names(dat)) {
    ##     eval(parse(text = paste0(sou, ' <- dat$', sou)))
    ## }
    ## DATA.STANi4 <- merge(DATA.STANi4, DATA.XRATES[DATA.XRATES$var=="EXCH",], by = c("cou", "year"))
    ## names(DATA.STANi4) <-  sub("var.x", "var", names(DATA.STANi4))
    ## names(DATA.STANi4) <-  sub("value.x", "value", names(DATA.STANi4))
    ## DATA.STANi4$value[DATA.STANi4$var%in%STAN.VARMON] <- DATA.STANi4$value[DATA.STANi4$var%in%STAN.VARMON] / DATA.STANi4$value.y[DATA.STANi4$var%in%STAN.VARMON]
    ## DATA.STANi4 <- subset(DATA.STANi4, select = c("cou", "var", "ind", "year", "value"))

    DATA.STANandBTDi4 <- rbind(DATA.STANi4, DATA.BTDi4)

    ## ISIC Rev. 3 sources: add zeros or convert STAN industry to new aggregate
    ## ISIC Rev. 4 sources: pre-agggregate and convert to ISIC Rev. 3
    ## souconv <- "INDSTAT32"
    ## souconv <- "NSONAPATCH"
    ## souconv <- "EUNAMAR1"
    ## souconv <- "STDSNAi3"
    ## souconv <- "STDSNAi4"

    for (souconv in unique(ui.stani3Estimate.convind.table$sou)) {

        eval(parse(text = paste0('data <- DATA.', souconv)))
        id.vars <- c("cou", "var", "year")
        data <- dcast(data, cou + var + year ~ ind, value.var = "value")
        ## View(data)
        ## ISIC Rev. 4 pre-aggregation: e.g. D20T21 missing from EUNAMAR2
        if (souconv%in%ui.stani3Estimate.sou.isic4) {
            ## data <- indAggregate(data, isic = 4, naAsZero = NULL, fill2D = TRUE, missing.2d = c("D97T98", "D99"))
            data <- indAggregate(data, isic = 4, naAsZero = c("D97T98", "D99"), fill2D = TRUE, missing.2d = c("D97T98", "D99"))
        }
        ## country-specific industry conversion (by source)
        formula.indi3.cou <- subset(ui.stani3Estimate.convind.table, select = c("cou", "ind", "formula"), sou==souconv & !is.na(cou))
        data.conv <- NULL
        ## convcou <- "CAN"
        for (convcou in formula.indi3.cou$cou) {
            data.conv.cou <- calcFormula(data=subset(data, cou==convcou),
                                         formula=subset(formula.indi3.cou, select = c("ind", "formula"), cou==convcou),
                                         id.vars=id.vars)
            data.conv.cou <- melt(data.conv.cou, id.vars = id.vars, variable.name = "ind", na.rm = TRUE)
            data.conv.cou <- subset(data.conv.cou, select = c("cou", "var", "ind", "year", "value"))
            data.conv <- rbind(data.conv, data.conv.cou)
        }
        ## general industry conversion (all countries in source)
        formula.indi3.allcou <- subset(ui.stani3Estimate.convind.table, select = c("ind", "formula"), sou==souconv & is.na(cou))
        if (nrow(formula.indi3.allcou) > 0) {
            data.conv.allcou <- calcFormula(data=data, formula=formula.indi3.allcou, id.vars=id.vars)
            data.conv.allcou <- melt(data.conv.allcou, id.vars = id.vars, variable.name = "ind", na.rm = TRUE)
            data.conv.allcou <- subset(data.conv.allcou, select = c("cou", "var", "ind", "year", "value"))
            data.conv <- rbind(data.conv, data.conv.allcou)
            data.conv <- data.conv[!duplicated(data.conv[,colnames(data.conv)%in%c("cou", "var", "ind", "year")]),]
        }
        if (souconv%in%ui.stani3Estimate.sou.isic4) {
            eval(parse(text = paste0('DATA.', souconv, ' <- data.conv')))
        } else {
            ## include test: warn about duplicated
            eval(parse(text = paste0('DATA.', souconv, ' <- rbind(DATA.', souconv, ', data.conv)')))
            eval(parse(text = paste0('DATA.', souconv, ' <- DATA.', souconv, '[!duplicated(DATA.', souconv, '[,colnames(DATA.', souconv, ')%in%c("cou", "var", "ind", "year")]),]')))
        }

    }

    ## sou <- "NSONAPATCH"
    ## sou <- "EUNAMAR1"
    for (sou in ui.stani3Estimate.sou[!ui.stani3Estimate.sou%in%ui.stani3Estimate.sou.estim]) {

        ## add source name
        eval(parse(text = paste0('DATA.', sou, '$sou <- "', sou, '"')))
        ## modify power of values for monetary variables
        eval(parse(text = paste0('DATA.', sou, '$value[DATA.', sou, '$var%in%STAN.VARMON] <- DATA.', sou, '$value[DATA.', sou, '$var%in%STAN.VARMON] * 10^(-', stani3Estimate.VIS.SOU$unit[stani3Estimate.VIS.SOU$sou==sou], ')')))
        ## pivot to add aggregates
        eval(parse(text = paste0('DATA.', sou, ' <- dcast(DATA.', sou, ', cou + sou + var + year ~ ind, value.var = "value")')))
        eval(parse(text = paste0('DATA.', sou, ' <- indAggregate(DATA.', sou, ', isic = 3, naAsZero = c("C73T74", "C95", "C99"), fill2D = TRUE, missing.2d = c("C95", "C99"))')))
        ## C75T99, C65T99
        ##
        ## naAsZero: specify aggregates where missings are treated as zero for aggregation
        ## missing.2d  C37     AUS
        ## naAsZero    C23T26  IRL
        ##             C73T74  JPN
        ## ## create C75T99 with modified indAggregate function where only selected 2-digit sectors are filled with zero
        ## pivot back to long format
        eval(parse(text = paste0('DATA.', sou, ' <- melt(DATA.', sou, ', id.vars = c("cou", "sou", "var", "year"), variable.name = "ind", na.rm = TRUE)')))
        ##
        ## Table to drop information
        drop.sou.list <- ui.stani3Estimate.drop.table[ui.stani3Estimate.drop.table$sou==sou,]
        if (nrow(drop.sou.list) > 0) {
            for (i in 1:nrow(drop.sou.list)) {
                ##
                drop.cou <- as.character(drop.sou.list$cou[i])
                drop.var <- as.character(drop.sou.list$var[i])
                drop.yearmin <- drop.sou.list$yearmin[i]
                drop.yearmax <- drop.sou.list$yearmax[i]
                ##
                if (!is.na(drop.yearmin)) {
                    eval(parse(text = paste0('DATA.', sou, ' <- DATA.', sou, '[!(DATA.', sou, '$cou==drop.cou & DATA.', sou, '$var==drop.var & DATA.', sou, '$year < drop.yearmin),]')))
                }
                if (!is.na(drop.yearmax)) {
                    eval(parse(text = paste0('DATA.', sou, ' <- DATA.', sou, '[!(DATA.', sou, '$cou==drop.cou & DATA.', sou, '$var==drop.var & DATA.', sou, '$year > drop.yearmax),]')))
                }
            }
        }

    }
    ## combine all data sources
    command.data.all <- NULL
    for (sou in c(ui.stani3Estimate.sou[!ui.stani3Estimate.sou%in%ui.stani3Estimate.sou.estim])) {
        command.data.all <- paste0(command.data.all, paste0('DATA.', sou, ','))
    }
    ## remove trailing comma from last data set
    command.data.all <- toString(paste0('stani3Estimate.data.all <- rbind(', substr(command.data.all, 1, nchar(command.data.all)-1), ')'))
    eval(parse(text = command.data.all))
    ## h(stani3Estimate.data.all)


    ## testing ##

    ## ## previous ##
    ## save(stani3Estimate.data.all, file = file.path("data", "data_init", "stani3Estimate_srcdata.rda"))
    ## ## end previous ##

    ## new ##
    dimnames <- names(stani3Estimate.data.all)
    dimnames <- dimnames[dimnames!="value"]
    ## data.array <- acast(stani3Estimate.data.all, cou ~ sou ~ var ~ year ~ ind, value.var = "value")
    stani3Estimate.data.array <- acast(stani3Estimate.data.all, cou ~ sou ~ var ~ year ~ ind, value.var = "value")
    names(dimnames(stani3Estimate.data.array)) <- dimnames
    save(stani3Estimate.data.array, file = file.path("data", "data_init", "stani3Estimate_dataarray.rda"))
    ## end new ##

    ## end testing ##

})

## stani3Estimate.data.all <- reactive({
stani3Estimate.data.array <- reactive({
    if (is.null(input$stani3estimate_dataarray_reloadButton) || input$stani3estimate_dataarray_reloadButton==0) {
        ## Source data stani3Estimate.data.all
        ## load(file = file.path("data", "data_init", "stani3Estimate_srcdata.rda"))
        load(file = file.path("data", "data_init", "stani3Estimate_dataarray.rda"))
        ## dimnames(stani3Estimate.data.array)
        ## stani3Estimate.data.array[, "STDSNAi3", "VALU", "2000", "CTOTAL"]
        ## stani3Estimate.data.array[, "STDSNAi4", "VALU", "2000", "CTOTAL"]
        ## stani3Estimate.data.array[, , "VALU", "2000", "DTOTAL"]
    } else {
        ## Source data stani3Estimate.data.all
        ## load(file = file.path("data", "data_init", "stani3Estimate_srcdata.rda"))
        load(file = file.path("data", "data_init", "stani3Estimate_dataarray.rda"))
    }

    ## return(stani3Estimate.data.all)
    return(stani3Estimate.data.array)

})

## isolate(.stani3Estimate())
##
## observe({
##   if(is.null(input$stani3EstimateReport) || input$stani3EstimateReport == 0) return()
##   isolate({
##     inp <- list(
##       input$datasets,
##
##       input$stani3estimate_indic,
##       ui.stani3Estimate.year[as.numeric(input$stani3estimate_time)],
##       input$stani3estimate_demand,
##       ui.stani3Estimate.namesec.agg[as.numeric(input$stani3estimate_indX)],
##       ## indS = ui.stani3Estimate.namesec.agg[as.numeric(input$indS)],
##       names(ui.stani3Estimate.namereg.agg)[as.numeric(input$stani3estimate_couS)],
##       names(ui.stani3Estimate.namereg.agg)[as.numeric(input$stani3estimate_couX)],
##       names(ui.stani3Estimate.namereg.agg)[as.numeric(input$stani3estimate_couD)]
##       )
##
##     updateReport(inp,"stani3Estimate")
##   })
## })

stani3Estimate <- function(
    stani3estimate_allind = stani3estimate_allind,
    stani3estimate_cou = stani3estimate_cou,
    stani3estimate_datatabletype = stani3estimate_datatabletype,
    stani3estimate_detail = stani3estimate_detail,
    stani3estimate_diff = stani3estimate_diff,
    stani3estimate_exportcalc = stani3estimate_exportcalc,
    stani3estimate_exportcsv = stani3estimate_exportcsv,
    stani3estimate_exportcou = stani3estimate_exportcou,
    stani3estimate_exportxls = stani3estimate_exportxls,
    stani3estimate_extend = stani3estimate_extend,
    stani3estimate_horiz = stani3estimate_horiz,
    stani3estimate_ind = stani3estimate_ind,
    stani3estimate_ind2 = stani3estimate_ind2,
    stani3estimate_ind.peers.select = stani3estimate_ind.peers.select,
    stani3estimate_ind.parent.select = stani3estimate_ind.parent.select,
    stani3estimate_limit.yrange.max = stani3estimate_limit.yrange.max,
    stani3estimate_limit.yrange.min = stani3estimate_limit.yrange.min,
    stani3estimate_missing = stani3estimate_missing,
    stani3estimate_morevar = stani3estimate_morevar,
    stani3estimate_plottype = stani3estimate_plottype,
    stani3estimate_singlesou = stani3estimate_singlesou,
    stani3estimate_sou = stani3estimate_sou,
    stani3estimate_souall = stani3estimate_souall,
    stani3estimate_sou_det_main = stani3estimate_sou_det_main,
    stani3estimate_sou_det_sec = stani3estimate_sou_det_sec,
    stani3estimate_sou_det_reloadButton = stani3estimate_sou_det_reloadButton,
    stani3estimate_sou_det_saveButton = stani3estimate_sou_det_saveButton,
    stani3estimate_sou_ext_main = stani3estimate_sou_ext_main,
    stani3estimate_sou_ext_sec = stani3estimate_sou_ext_sec,
    stani3estimate_sou_ext_reloadButton = stani3estimate_sou_ext_reloadButton,
    stani3estimate_sou_ext_saveButton = stani3estimate_sou_ext_saveButton,
    ## stani3estimate_STANi3.INDA6 = stani3estimate_STANi3.INDA6,
    ## stani3estimate_STANi3.INDA18 = stani3estimate_STANi3.INDA18,
    ## stani3estimate_STANi3.INDA34 = stani3estimate_STANi3.INDA34,
    ## stani3estimate_STANi3.INDA46 = stani3estimate_STANi3.INDA46,
    ## stani3estimate_STANi3.INDA60 = stani3estimate_STANi3.INDA60,
    stani3estimate_STANi3.INDLABEL = stani3estimate_STANi3.INDLABEL,
    stani3estimate_tabletype = stani3estimate_tabletype,
    stani3estimate_time = stani3estimate_time,
    stani3estimate_update = stani3estimate_update,
    stani3estimate_var = stani3estimate_var,
    stani3estimate_var2 = stani3estimate_var2,
    stani3estimate_yrange = stani3estimate_yrange,
    ##
    stani3Estimate_viz_plot_height = stani3Estimate_viz_plot_height,
    stani3Estimate_viz_plot_width = stani3Estimate_viz_plot_width
    )
{

    ## data.all <- stani3Estimate.data.all

    namecou <- stani3estimate_cou

    ## exportcou <- input$stani3estimate_exportcou # exportcou <- "USA"
    exportcou <- union(namecou, stani3estimate_exportcou) # exportcou <- "USA"
    for (cou in exportcou[!exportcou%in%ui.stani3Estimate.cou]) {
        exportcou <- exportcou[exportcou!=cou]
        exportcou <- union(exportcou, STAN.COU[[cou]])
    }
    exportcou <- sort(exportcou)
    exportcou <- exportcou[exportcou%in%dimnames(ui.stani3Estimate.est.array)$cou]

    ## sources
    if (stani3estimate_extend==TRUE & length(stani3estimate_sou_ext_main) > 0 & length(stani3estimate_sou_ext_sec) > 0) {
        namesou.ext <- c(stani3estimate_sou_ext_main, stani3estimate_sou_ext_sec)
    } else namesou.ext <- NULL
    if (stani3estimate_detail==TRUE & length(stani3estimate_sou_det_main) > 0 & length(stani3estimate_sou_det_sec) > 0) {
        namesou.det <- c(stani3estimate_sou_det_main, stani3estimate_sou_det_sec)
    } else namesou.det <- NULL

    namesou <- NULL
    if (stani3estimate_souall==FALSE) {
        namesou <- stani3estimate_sou
        if (stani3estimate_extend==TRUE) {
            namesou <- union(namesou, namesou.ext)
            namesou <- c(namesou, "PATCHEXT")
        }
        if (stani3estimate_detail==TRUE) {
            namesou <- union(namesou, namesou.det)
            namesou <- c(namesou, "PATCHDET")
        }
    } else {
        namesou <- ui.stani3Estimate.sou
        if (stani3estimate_extend==FALSE) namesou <- namesou[namesou!="PATCHEXT"]
        if (stani3estimate_detail==FALSE) namesou <- namesou[namesou!="PATCHDET"]
    }

    ## variables
    if (stani3estimate_var2 != "") {
        namevar <- c(stani3estimate_var, stani3estimate_var2)
    } else {
        namevar <- stani3estimate_var
    }
    ##############
    ## industries
    ##############
    if (stani3estimate_detail==TRUE) {
        ind.parent <- as.character(STANi3.HIERARCHYINV[[stani3estimate_ind]])
    } else {
        ind.parent <- NULL
    }
    ##
    ind.peers <- stani3estimate_ind.peers.select
    ##
    ind.parent.select <- stani3estimate_ind.parent.select
    ##

    nameind <- stani3estimate_ind
    if (stani3estimate_detail==TRUE) {
        nameind <- c(ind.parent.select, ind.peers) # ind.parent[1]
        nameind <- factor(nameind, levels = STANi3.INDA60All[STANi3.INDA60All%in%nameind])
        nameind <- nameind[order(nameind)]
    } else {
        for (indlist in ui.stani3Estimate.ind.agg) {
            eval(parse(text = paste0('if ("', indlist, '"%in%nameind) nameind <- union(nameind[!nameind%in%"', indlist, '"], STANi3.IND', indlist, ')')))
        }
    }

    ## years
    nameyear <- c(stani3estimate_time[1]:stani3estimate_time[2])
    ## plot limits
    if (stani3estimate_var2=="" & stani3estimate_ind2=="") {
        limit.yrange.min <- FALSE
    } else {
        limit.yrange.min <- stani3estimate_limit.yrange.min
    }
    if (stani3estimate_var2=="" & stani3estimate_ind2=="") {
        limit.yrange.max <- FALSE
    } else {
        limit.yrange.max <- stani3estimate_limit.yrange.max
    }
    yrange <- NULL
    if (limit.yrange.min==TRUE) yrange[1] <- stani3estimate_yrange[1] # slider value
    if (limit.yrange.max==TRUE) yrange[2] <- stani3estimate_yrange[2]


    ## ## set back ##
    ## ## TESTING ##
    ## namecou <- c("AUT")
    ## namesou <- c("EUNAMAR1")
    ## namevar <- c("VALU")
    ## nameyear <- c(2000:2002)
    ## nameind <- c("CTOTAL", "C01T02", "C05")

    ## data.all <- subset(isolate(stani3Estimate.data.all()),
    ##                    cou%in%namecou &
    ##                    var%in%namevar &
    ##                    ind%in%nameind &
    ##                    year%in%nameyear &
    ##                    sou%in%namesou)

    ## data.all <- subset(stani3Estimate.data.all(),
    ##                    cou%in%namecou &
    ##                    var%in%namevar &
    ##                    ind%in%nameind &
    ##                    year%in%nameyear &
    ##                    sou%in%namesou)

    ## end set back ##

    ## ## ## test <- melt(stani3Estimate.data.array[namecou, namesou, namevar, nameyear, nameind])
    ## ## ## h(test)
    ## ## h(melt(isolate(stani3Estimate.data.array())))

    ## ## TESTING ##
    ## namecou <- c("AUT")
    ## namesou <- c("STANandBTD")
    ## namevar <- c("VALU")
    ## nameyear <- c(2000:2010)
    ## nameind <- c("C10T41", "C15T37")

    ## data.all <- melt(isolate(stani3Estimate.data.array())[namecou,
    ##                                             namesou[!namesou%in%c("PATCHEXT", "PATCHDET")],
    ##                                             namevar,
    ##                                             as.character(nameyear),
    ##                                             nameind])

    ## data.all

    ## END TESTING ##

    data.all <- melt(stani3Estimate.data.array()[namecou,
                                                 namesou[!namesou%in%c("PATCHEXT", "PATCHDET")],
                                                 namevar,
                                                 as.character(nameyear),
                                                 as.character(nameind)])

    data.all <- data.all[!is.na(data.all$value),]

    if (!"cou"%in%names(data.all)) data.all$cou <- namecou
    if (!"sou"%in%names(data.all)) data.all$sou <- namesou
    if (!"var"%in%names(data.all)) data.all$var <- namevar
    if (!"year"%in%names(data.all)) data.all$year <- as.numeric(nameyear)
    if (!"ind"%in%names(data.all)) data.all$ind <- nameind

    data.all <- subset(data.all, select = c("cou", "sou", "var", "year", "ind", "value"))

    ## cat("***************\nprint data\n***************")
    ## print(data.all)

    ## ## ############# ##
    ## ## END DEV ARRAY ##
    ## ## ############# ##



    ## Create estimates: extend
    if (stani3estimate_extend==TRUE & length(namesou.ext) >= 2) {

                sources.ext <- data.frame(cou = stani3estimate_cou,
                                  var = stani3estimate_var,
                                  ind = stani3estimate_ind,
                                  est = "EXT",
                                  MAIN = namesou.ext[1],
                                  SEC = toString(namesou.ext[2:length(namesou.ext)]),
                                  stringsAsFactors = FALSE)

                data.patch.ext <- estimate(data = data.all, sources = sources.ext, period = nameyear, isic = 3)

    } else {
        data.patch.ext <- NULL
    }

    ## Create estimates: detail
    ## stani3estimate_detail <- TRUE
    ## namesou.det <- c("STANandBTD", "STANandBTDi4")
    ## namesou
    if (stani3estimate_detail==TRUE & length(namesou.det) >= 2) {

        sources.det <- data.frame(cou = stani3estimate_cou,
                                  var = stani3estimate_var,
                                  ind = stani3estimate_ind,
                                  est = "DET",
                                  MAIN = namesou.det[1],
                                  SEC = toString(namesou.det[2:length(namesou.det)]),
                                  stringsAsFactors = FALSE)

        data.patch.det <- estimate(data = data.all, sources = sources.det, period = nameyear, isic = 3)
    } else {
        data.patch.det <- NULL
    }

    if (stani3estimate_var2 != "") {
        data.table <- data.all[data.all$cou%in%namecou &
                               data.all$var%in%namevar &
                               data.all$ind%in%nameind &
                               data.all$year%in%nameyear &
                               data.all$sou%in%namesou, ]
    } else {
        data.table <- data.all[data.all$cou%in%namecou &
                               data.all$var%in%namevar &
                               data.all$ind%in%union(nameind, stani3estimate_ind2) &
                               data.all$year%in%nameyear &
                               data.all$sou%in%namesou, ]
    }
    if (stani3estimate_extend==TRUE & length(namesou.ext) >= 2) {
        data.table <- rbind(data.table, data.patch.ext)
    }
    if (stani3estimate_detail==TRUE & length(namesou.det) >= 2) {
        data.table <- rbind(data.table, data.patch.det)
    }

    ## calculate share according to selection
    if (stani3estimate_var2 != "")
    {
        data.table2 <- mergeCalc(
            dimMerge(
                data=data.table,
                dim="var",
                dim1=stani3estimate_var,
                dim2=stani3estimate_var2),
            diff=stani3estimate_diff)
    } else if (stani3estimate_var2 == "" & stani3estimate_ind2 != "")
    {
        data.table2 <- mergeCalc(
            dimMerge(
                data=data.table,
                dim="ind",
                dim1=nameind,
                dim2=stani3estimate_ind2),
            diff=stani3estimate_diff)
    } else {
        data.table2 <- NULL
    }


    return(list(data.table = data.table,
                data.table2 = data.table2,
                ## data.table.pivot = data.table.pivot,
                exportcou = exportcou,
                ind.parent = ind.parent,
                namecou = namecou,
                nameind = nameind,
                namesou = namesou,
                namesou.det = namesou.det,
                namesou.ext = namesou.ext,
                namevar = namevar,
                nameyear = nameyear,
                yrange = yrange,
                ## stani3estimate_STANi3.INDA6 = stani3estimate_STANi3.INDA6,
                ## stani3estimate_STANi3.INDA18 = stani3estimate_STANi3.INDA18,
                ## stani3estimate_STANi3.INDA34 = stani3estimate_STANi3.INDA34,
                ## stani3estimate_STANi3.INDA46 = stani3estimate_STANi3.INDA46,
                stani3estimate_STANi3.INDLABEL = stani3estimate_STANi3.INDLABEL,
                ##
                stani3estimate_datatabletype = stani3estimate_datatabletype,
                stani3estimate_detail = stani3estimate_detail,
                stani3estimate_exportcsv = stani3estimate_exportcsv,
                stani3estimate_exportxls = stani3estimate_exportxls,
                stani3estimate_exportcalc = stani3estimate_exportcalc,
                stani3estimate_extend = stani3estimate_extend,
                stani3estimate_horiz = stani3estimate_horiz,
                stani3estimate_ind = stani3estimate_ind,
                stani3estimate_ind2 = stani3estimate_ind2,
                stani3estimate_limit.yrange.min = stani3estimate_limit.yrange.min,
                stani3estimate_limit.yrange.max = stani3estimate_limit.yrange.max,
                stani3estimate_missing = stani3estimate_missing,
                stani3estimate_morevar = stani3estimate_morevar,
                stani3estimate_plottype = stani3estimate_plottype,
                stani3estimate_singlesou = stani3estimate_singlesou,
                stani3estimate_tabletype = stani3estimate_tabletype,
                stani3estimate_update = stani3estimate_update,
                stani3estimate_var2 = stani3estimate_var2
                ## stani3estimate_sou_all_print = stani3estimate_sou_all_print,
                )
           )
}

summary_stani3Estimate <- function(result = .stani3Estimate()) {
    if (length(result) > 0) {

        exportcou <- result$exportcou
        data.table <- result$data.table
        namesou.det <- result$namesou.det
        namesou.ext <- result$namesou.ext
        ##
        stani3estimate_detail <- result$stani3estimate_detail
        stani3estimate_extend <- result$stani3estimate_extend

        list.print <- NULL
        if (length(exportcou) > 0) {
            list.print <- c(list.print, list(Exportcou = exportcou))
        }
        if (stani3estimate_detail==TRUE | stani3estimate_extend==TRUE) {
            namesou <- NULL
            if (stani3estimate_detail==TRUE) namesou <- c(namesou, namesou.det, "PATCHDET")
            if (stani3estimate_extend==TRUE) namesou <- c(namesou, namesou.ext, "PATCHEXT")
            data.table <- data.table[data.table$sou%in%namesou,]
            data.table.d <- dcast(data.table, cou + var + ind + year ~ sou, value.var = "value")
            ## print(data.table.d)
            ## return(data.table.d)
            if (stani3estimate_detail==TRUE) list.print <- c(list.print, list(namesou.det = namesou.det))
            if (stani3estimate_extend==TRUE) list.print <- c(list.print, list(namesou.ext = namesou.ext))
            list.print <- c(list.print, list(Data = data.table.d))
            ## if (stani3estimate_sou_ext_print==TRUE) {
            ##     ui.stani3Estimate.est.array[cou,var,ind,"EXT","MAIN"]
            ##     list.print <- c(list.print, list(Estimation =
            ## }
        }
        return(list.print)

}}

tables_stani3Estimate <- function(result = .stani3Estimate()) {
    if (length(result) > 0) {

    ## data.table.pivot <- result$data.table.pivot
    data.table <- result$data.table
    data.table2 <- result$data.table2
    stani3estimate_tabletype <- result$stani3estimate_tabletype
    stani3estimate_missing <- result$stani3estimate_missing
    namecou <- result$namecou
    namesou <- result$namesou
    namevar <- result$namevar
    nameind <- result$nameind

    if (stani3estimate_tabletype=="Data")
    {
        ## Tab: Table: Data pivot
        data.table.pivot <- rbind(dcast(data.table, cou + sou + var + ind ~ year, value.var="value"))
        if (stani3estimate_missing==TRUE)
            {
                data.table.pivot <- data.table.pivot[data.table.pivot$ind%in%nameind,] # remove ind2
                namecou.namesou <- merge(as.data.frame(namecou), as.data.frame(namesou))
                namecou.namesou.namevar <- merge(as.data.frame(namecou.namesou), as.data.frame(namevar))
                namecou.namesou.namevar.nameind <- merge(namecou.namesou.namevar, as.data.frame(nameind))
                names(namecou.namesou.namevar.nameind) <- sub("name", "", names(namecou.namesou.namevar.nameind))
                data.table.missing <- merge(data.table.pivot, namecou.namesou.namevar.nameind, all = TRUE)
                data.table.pivot <- data.table.missing
            }
        data.table.pivot$cou <- factor(data.table.pivot$cou, levels = ui.stani3Estimate.cou) # namecou
        data.table.pivot$sou <- factor(data.table.pivot$sou, levels = ui.stani3Estimate.sou) # namesou
        data.table.pivot$var <- factor(data.table.pivot$var, levels = ui.stani3Estimate.var) # namevar
        ## data.table.pivot$ind <- factor(data.table.pivot$ind, levels = ui.stani3Estimate.ind) # nameind
        ## BUG: X.C15T37. appearing ??
        data.table.pivot$ind <- factor(data.table.pivot$ind, levels = STANi3.INDA60All[STANi3.INDA60All%in%data.table.pivot$ind]) # nameind
        data.table.pivot <- data.table.pivot[order(data.table.pivot$cou, data.table.pivot$sou, data.table.pivot$var, data.table.pivot$ind),]
        ## table <- data.table.pivot
        return(data.table.pivot)

    } else if (stani3estimate_tabletype=="Calculation") {
        ## Tab: Table: Calc (display merged data in table)
        ## table <- data.table2
        return(data.table2)
    }

}}

datatables_stani3Estimate <- function(result = .stani3Estimate()) {
    if (length(result) > 0) {


        ## print array with estimation sources
        ## stani3estimate_sou_all_print <- result$stani3estimate_sou_all_print
        ## if (stani3estimate_sou_all_print==TRUE) return(melt(ui.stani3Estimate.est.array))
        stani3estimate_datatabletype <- result$stani3estimate_datatabletype
        stani3estimate_ind <- result$stani3estimate_ind
        stani3estimate_STANi3.INDLABEL <- result$stani3estimate_STANi3.INDLABEL

        if (stani3estimate_datatabletype=="Conversion") {
            ui.stani3Estimate.convind.table.d <- dcast(ui.stani3Estimate.convind.table, sou + ind ~ cou, value.var = "formula")
            return(ui.stani3Estimate.convind.table.d)
        }

        if (stani3estimate_datatabletype=="Sources") {
            load("data/data_init/stani3Estimate_srcarray.rda")
            return(melt(ui.stani3Estimate.est.array))
        }


        table.ind <- data.frame(STANi3.INDA60All = STANi3.INDA60All)
        for (indlist in ui.stani3Estimate.ind.agg) {
            eval(parse(text = paste0('if ("', indlist, '"%in%stani3estimate_ind) table.ind <- merge(table.ind, data.frame(STANi3.INDA60All=STANi3.IND', indlist, ', STANi3.IND', indlist, '=STANi3.IND', indlist, '), all = TRUE)')))
        }

        if (stani3estimate_STANi3.INDLABEL==TRUE)
            {
                table.ind <- merge(table.ind, data.frame(STANi3.INDA60All=STANi3.INDLABEL$ind, STANi3.INDLABEL=STANi3.INDLABEL$label), all = FALSE)
                ## required by "merge(..., all = FALSE")
                table.ind$STANi3.INDA60All <- factor(table.ind$STANi3.INDA60All, levels = STANi3.INDA60All)
                table.ind <- table.ind[order(table.ind$STANi3.INDA60All),]
            }

        return(table.ind)

    }}

plots_stani3Estimate <- function(result = .stani3Estimate()) {
    if (length(result) > 0) {

    VIS.SOU <- stani3Estimate.VIS.SOU # for shortness

    data.table <- result$data.table
    data.table2 <- result$data.table2
    nameind <- result$nameind
    namesou <- result$namesou
    nameyear <- result$nameyear
    yrange <- result$yrange

    ## print(data.table)

    stani3estimate_detail <- result$stani3estimate_detail
    stani3estimate_horiz <- result$stani3estimate_horiz
    stani3estimate_ind2 <- result$stani3estimate_ind2
    stani3estimate_limit.yrange.min <- result$stani3estimate_limit.yrange.min
    stani3estimate_limit.yrange.max <- result$stani3estimate_limit.yrange.max
    stani3estimate_plottype <- result$stani3estimate_plottype
    stani3estimate_singlesou <- result$stani3estimate_singlesou
    stani3estimate_var2 <- result$stani3estimate_var2

    if (stani3estimate_plottype=="Bars") {
        ## Tab: Plot: Bars
        ## output$plot_bar <- renderPlot({
        ##     ## if var2 selected, use data from data.table2
        ##     data.table <- data.table()
        ##     data.table2 <- data.table2()
        ## nameind <- nameind()
        if (stani3estimate_var2 != "" | stani3estimate_ind2 != "") {
            data <- data.table2
            if (stani3estimate_var2 != "") names(data) <- sub("dim.x", "var", names(data))
            if (stani3estimate_var2 == "" & stani3estimate_ind2 != "") names(data) <- sub("dim.x", "ind", names(data))
        } else {
            data <- data.table
        }
        ## define source
        data <- data[data$sou==stani3estimate_singlesou,]
        ##
        data$ind <- factor(data$ind, levels = sort(nameind, decreasing = FALSE))
        data.plot <- (dcast(data, ind ~ year, value.var = "value"))
        row.names(data.plot) <- data.plot[,1]
        data.plot <- data.plot[,-1]
        data.plot <- as.matrix(data.plot)
        ##
        op <- par(oma=c(0,0,1,0))      # Room for the title and legend
        ## bg=NA)               # transparent background
        barplot(data.plot,
                col=VIS.SOU$col[1:12],
                horiz=stani3estimate_horiz)
        par(op) # Leave the last plot
        op <- par(usr=c(0,1,0,1), # Reset the coordinates
                  xpd=NA)         # Allow plotting outside the plot region
        legend(0, 1.1, legend=rownames(data.plot), cex=0.8, fill=VIS.SOU$col, box.col = NA, horiz = TRUE)

    } else if (stani3estimate_plottype=="Lines") {
        ## ## Tab: Plot: Lines
        ## output$plot_line<- renderPlot({
        ##     ## data: data of one industry
        if (stani3estimate_detail==TRUE) {
            ## nameind <- nameind()
            op <- par(oma=c(0,0,3,0),
                      mfrow=c(length(nameind),1),
                      mar=c(2,2,4,2))    # Room for the title and legend
            for (ind in nameind)
            {
                data <- data.table[data.table$ind==ind,]

                if (stani3estimate_limit.yrange.min==TRUE) ymin <- yrange[1] else ymin <- min(data$value[data$sou%in%namesou])
                if (stani3estimate_limit.yrange.max==TRUE) ymax <- yrange[2] else ymax <- max(data$value[data$sou%in%namesou])
                ##
                ## create empty canvas with final dimensions - add different series using loop over selected sources
                plot(nameyear, rep(NA, length(nameyear)),
                     ylim=c(ymin, ymax),
                     xlab='', ylab='', main=ind)
                ##
                for (sou in namesou)            # add layer for each selected source
                {
                    lines(intersect(data[data$sou==sou,]$year, nameyear), # x-axis value
                          data$value[data$year%in%intersect(data$year, nameyear) & data$sou==sou], # y-axis value
                          col = VIS.SOU$col[VIS.SOU$sou==sou], # series colour
                          type = VIS.SOU$type[VIS.SOU$sou==sou], # series type: l = line, p = point
                          lty = VIS.SOU$lty[VIS.SOU$sou==sou], # line type: 5 = dashed
                          pch = VIS.SOU$pch[VIS.SOU$sou==sou]  # symbol type: 19 = filled dot
                          )
                }
            }
            ##
            par(op)                         # Leave the last plot
            op <- par(usr = c(0,1,0,1),       # Reset the coordinates (above plot)
                      xpd = NA)     # Allow plotting outside the plot region
            ##
            legend(0, 1.1,          # legend position
                   legend = VIS.SOU$label[VIS.SOU$sou%in%namesou], # legend item labels
                   cex = 0.7,               # legend label font size
                   col = VIS.SOU$col[VIS.SOU$sou%in%namesou], # legend item fill colour
                   lty = VIS.SOU$lty[VIS.SOU$sou%in%namesou],
                   pch = VIS.SOU$pch[VIS.SOU$sou%in%namesou],
                   box.col = NA,            # invisible box around legend
                   horiz = TRUE)            # legend orientation
        } else {
            if (stani3estimate_var2 != "" | stani3estimate_ind2 != "") {
                data <- data.table2
            } else {
                data <- data.table
            }
            ##
            if (stani3estimate_limit.yrange.min==TRUE) ymin <- yrange[1] else ymin <- min(data$value[data$sou%in%namesou])
            if (stani3estimate_limit.yrange.max==TRUE) ymax <- yrange[2] else ymax <- max(data$value[data$sou%in%namesou])

            ## op <- par(oma=c(0,0,1,0))      # Room for the title and legend
            ## test: legend on right-hand side
            ## 1:South, 2:WEST, 3:NORTH, 4:EAST
            op <- par(oma=c(0,0,0,7))      # Room for the title and legend
            ## end test ##
            ## create empty canvas with final dimensions - add different series using loop over selected sources
            plot(nameyear, rep(NA, length(nameyear)),
                 ylim=c(ymin, ymax),
                 ## xlab='', ylab='')
                 xlab='', ylab='', main=nameind)
            ## http://research.stowers-institute.org/efg/R/Color/Chart/ColorChart.pdf
            for (sou in namesou)            # add layer for each selected source
            {
                lines(intersect(data[data$sou==sou,]$year, nameyear), # x-axis value
                      data$value[data$year%in%intersect(data$year, nameyear) & data$sou==sou], # y-axis value
                      col = VIS.SOU$col[VIS.SOU$sou==sou], # series colour
                      type = VIS.SOU$type[VIS.SOU$sou==sou], # series type: l = line, p = point
                      lty = VIS.SOU$lty[VIS.SOU$sou==sou], # line type: 5 = dashed
                      pch = VIS.SOU$pch[VIS.SOU$sou==sou]  # symbol type: 19 = filled dot
                      )
            }
            par(op)                         # Leave the last plot

            ## op <- par(usr = c(0,1,0,1),       # Reset the coordinates (above plot)
            ##           xpd = NA)     # Allow plotting outside the plot region
            ## legend(0, 1.1,          # legend position
            ##        legend = VIS.SOU$label[VIS.SOU$sou%in%namesou], # legend item labels
            ##        cex = 0.7,               # legend label font size
            ##        col = VIS.SOU$col[VIS.SOU$sou%in%namesou], # legend item fill colour
            ##        lty = VIS.SOU$lty[VIS.SOU$sou%in%namesou],
            ##        pch = VIS.SOU$pch[VIS.SOU$sou%in%namesou],
            ##        box.col = NA,            # invisible box around legend
            ##        horiz = TRUE)            # legend orientation

            ## test: legend on right-hand side of plot
            ## 1:South, 2:WEST, 3:NORTH, 4:EAST
            op <- par(usr = c(0,1,0,1.01),       # Reset the coordinates (above plot)
                      xpd = NA)     # Allow plotting outside the plot region
            legend(.87, 1,          # legend position
                   ## legend = VIS.SOU$label[VIS.SOU$sou%in%namesou], # legend item labels
                   legend = VIS.SOU$sou[VIS.SOU$sou%in%namesou], # legend item labels
                   cex = 1,               # legend label font size
                   col = VIS.SOU$col[VIS.SOU$sou%in%namesou], # legend item fill colour
                   lty = VIS.SOU$lty[VIS.SOU$sou%in%namesou],
                   pch = VIS.SOU$pch[VIS.SOU$sou%in%namesou],
                   box.col = NA,            # invisible box around legend
                   horiz = FALSE)            # legend orientation

        }
    }

}}

download_stani3Estimate <- function(result = .stani3Estimate(), zipfile = fname) {
    if (length(result) > 0) {

        exportcou <- result$exportcou # contains 'namecou' and 'exportcou'
        exportvar <- union(result$namevar, result$stani3estimate_morevar) # namevar <- "VALU"
        nameyear <- result$nameyear
        nameind <- as.character(ui.stani3Estimate.ind)
        ##
        stani3estimate_extend <- result$stani3estimate_extend
        stani3estimate_detail <- result$stani3estimate_detail

        stani3estimate_update = result$stani3estimate_update
        stani3estimate_exportcsv = result$stani3estimate_exportcsv
        stani3estimate_exportxls = result$stani3estimate_exportxls
        stani3estimate_exportcalc = result$stani3estimate_exportcalc

        ## loads object "stani3Estimate_results"
        ## if update==FALSE, only use previously estimated figures

        load("data/data_init/stani3Estimate_results.rda")

        if (stani3estimate_update==TRUE) {

            stani3Estimate_results <- stani3Estimate_results[!(stani3Estimate_results$cou%in%exportcou &
                                                               stani3Estimate_results$var%in%exportvar), ]

            ## ## #######################
            ## ## begin testing
            ## ## #######################
            ## path <- file.path(dbpath, "GitHub", "desk", "inst", "industry")
            ## setwd(path)
            ## load(file = file.path("data", "data_init", "stani3Estimate_dataarray.rda"))
            ## require(reshape2)
            ## exportcou <- c("AUT")
            ## ## exportcou <- c("AUT", "BEL")
            ## exportvar <- c("VALU", "PROD")
            ## nameind <- "CTOTAL"
            ## nameyear <- c(1980:2012)
            ## stani3estimate_extend <- TRUE
            ## stani3estimate_detail <- FALSE
            ## ## #######################
            ## ## end testing
            ## ## #######################

            load("data/data_init/stani3Estimate_srcarray.rda")

            nameest <- NULL
            if (stani3estimate_extend==TRUE) nameest <- c(nameest, "EXT")
            if (stani3estimate_detail==TRUE) nameest <- c(nameest, "DET")
            est.array <- ui.stani3Estimate.est.array[exportcou, exportvar, nameind, nameest,,drop = FALSE]
            est.array.m <- melt(est.array, id.vars = c("cou", "var", "ind", "est"), variable.name = "sou")
            est.array.m$ind <- factor(est.array.m$ind, levels = STANi3.INDA60All)
            est.array.d <- dcast(est.array.m, cou + var + ind + est ~ sou, value.var = "value")
            ## subset data to array selection
            ## data.all <- stani3Estimate.data.all()

            x <- match(unique(est.array.m$ind), names(STANi3.HIERARCHYINV))
            x <- x[!is.na(x)]
            ind.parent <- unique(unlist(STANi3.HIERARCHYINV[x]))

            ## ## previous ##
            ## data.all <- stani3Estimate.data.all()
            ## data.all <- data.all[data.all$cou%in%as.character(unique(est.array.m$cou)) &
            ##                      data.all$var%in%unique(est.array.m$var) &
            ##                      data.all$ind%in%union(unique(est.array.m$ind), ind.parent) &
            ##                      data.all$year%in%nameyear &
            ##                      data.all$sou%in%unique(unlist(strsplit(as.character(est.array.m$value), split = ", "))),]
            ## end previous ##

            ## new ##
            namesou <- unique(unlist(strsplit(as.character(est.array.m$value), split = ", ")))
            ## data.all <- melt(stani3Estimate.data.array[as.character(unique(est.array.m$cou)),
            data.all <- melt(stani3Estimate.data.array()[as.character(unique(est.array.m$cou)),
                                                         namesou[!namesou%in%c("PATCHEXT", "PATCHDET")],
                                                         as.character(unique(est.array.m$var)),
                                                         as.character(nameyear),
                                                         as.character(union(unique(est.array.m$ind), ind.parent))])

            data.all <- data.all[!is.na(data.all$value),]

            if (!"cou"%in%names(data.all)) data.all$cou <- as.character(unique(est.array.m$cou))
            if (!"sou"%in%names(data.all)) data.all$sou <- namesou
            if (!"var"%in%names(data.all)) data.all$var <- unique(est.array.m$var)
            if (!"year"%in%names(data.all)) data.all$year <- as.numeric(nameyear)
            if (!"ind"%in%names(data.all)) data.all$ind <- union(unique(est.array.m$ind), ind.parent)
            ## end new ##

            ## cat("***************\nprint data\n***************")
            ## print(data.all)

            res.ext <- NULL
            if (stani3estimate_extend==TRUE) {
                ## does this need to be filtered to contain only sources considered for extension?
                data.ext <- data.all
                print(est.array.d[est.array.d$est=="EXT",])
                res.ext <- stan::estimate(
                    data=data.ext
                   ,
                    sources=est.array.d[est.array.d$est=="EXT",]
                   ,
                    period=nameyear
                   ,
                    isic=3
                )
                ## add back rows from main source
                sou.main.ext <- melt(ui.stani3Estimate.est.array[exportcou, exportvar, nameind, "EXT", "MAIN"])
                names(sou.main.ext) <- sub("value", "sou", names(sou.main.ext))
                data.all.ext.main <- merge(data.all, sou.main.ext)
                res.ext <- rbind(res.ext, data.all.ext.main) # adds around 6000 rows from data.all
                res.ext <- res.ext[!duplicated(res.ext[,!colnames(res.ext)%in%c("sou", "value")]),]
                res.ext$sou <- "PATCHEXT"
            }
            res.ext <- rbind(res.ext, data.all)

            res.det <- NULL
            if (stani3estimate_detail==TRUE) {
                data.det <- res.ext
                print(est.array.d[est.array.d$est=="DET",])

                res.det <- stan::estimate(data=data.det
                                    ,
                                    sources=est.array.d[est.array.d$est=="DET",]
                                    ,
                                    period=nameyear
                                    )

                res.det <- rbind(res.det, res.ext)
            }
            data.est <- rbind(res.det, res.ext)

            data.est <- data.est[!duplicated(data.est[,!colnames(data.est)%in%c("sou", "value")]),]
            data.est$ind <- factor(data.est$ind, levels = nameind)
            data.est <- data.est[order(data.est$ind),]

            data <- data.est

            ## data adjustment: create function
            data <- dcast(data, cou + var + year ~ ind, value.var = "value")
            nameagg <- "CTOTAL"
            while(length(nameagg) > 0) {
                parts.all <- NULL
                for (agg in nameagg) {
                    parts <- as.character(STANi3.HIERARCHY[[agg]])
                    if (length(parts) > 0) {
                        temp <- data[, colnames(data) %in% parts]
                        ## returns vector if only one sector available and next command fails
                        if (all(is.element(parts, colnames(temp))==TRUE))
                            {
                                sum.parts <- unname(apply(as.matrix(temp), 1, "sum"))
                                ratio <- cbind.data.frame(data[, colnames(data) %in% agg], sum.parts)
                                names(ratio) <- c(agg, "sum.parts")
                                ratio <- ratio[,agg] / ratio$sum
                                ## cat(paste0('Mean ratio: ', mean(ratio, na.rm = TRUE), '\n\n'))
                                temp <- cbind(temp, ratio)
                                for (col in names(temp)[names(temp)!="ratio"]) {
                                    temp[, col] <- temp[, col] * temp[, "ratio"]
                                }
                                temp <- temp[,!colnames(temp)=="ratio"]
                                ##
                                data <- data[,!colnames(data)%in%names(temp)]
                                data <- cbind(data, temp)
                            } else {
                                ## remove parts columns from data
                                data <- data[, !colnames(data) %in% parts]
                            }
                        parts.all <- c(parts.all, parts)
                    }
                }
                nameagg <- parts.all
            }
            data.adj <- melt(data, id.vars = c("cou", "var", "year"), variable.name = "ind")

            data <- data.adj


            stani3Estimate_results <- rbind(stani3Estimate_results, data)
            save(stani3Estimate_results, file = "data/data_init/stani3Estimate_results.rda")
        } # if (stani3estimate_update==TRUE) {...}

        ## export data from stored object
        ## exportcou <- c("AUT", "BEL")
        ## exportcou <- c("GBR")
        ## exportvar <- c("VALU", "PROD")
        ## nrow(data)
        data <- stani3Estimate_results[stani3Estimate_results$cou%in%exportcou &
                                       stani3Estimate_results$var%in%exportvar,]

        ## add share calculations
        ## stani3estimate_exportcalc=TRUE
        if (stani3estimate_exportcalc==TRUE) {
            data.d <- dcast(data, cou + ind + year ~ var, value.var = "value")
            if ("VALU"%in%names(data.d) & "PROD"%in%names(data.d)) {
                attach(data.d)
                data.d[["VALUshPROD"]] <- VALU / PROD * 100
                detach(data.d)
            }
            if ("LABR"%in%names(data.d) & "VALU"%in%names(data.d)) {
                attach(data.d)
                data.d[["LABRshVALU"]] <- LABR / VALU * 100
                detach(data.d)
            }
            if ("VALU"%in%names(data.d) & "EMPN"%in%names(data.d)) {
                attach(data.d)
                data.d[["VALUperEMPN"]] <- VALU / EMPN * 10^6
                detach(data.d)
            }
            data <- melt(data.d, id.vars = c("cou", "ind", "year"), variable.name = "var")
            ##
            data.share <- merge(data, data[data$ind=="CTOTAL" & data$var=="VALU", colnames(data)!="ind"], by = c("cou", "var", "year"))
            data.share$value <- data.share$value.x / data.share$value.y * 100
            data.share$var <- "VALUshCTOTAL"
            data.share <- data.share[,!colnames(data.share)%in%c("value.x", "value.y")]
            ##
            data <- rbind(data, data.share)
        }

        ## empty temp directory
        tempdir = tempdir()
        unlink(paste0(tempdir, list.files(tempdir)))
        file.remove(file.path(tempdir, list.files(tempdir)))

        ## export flat file if selected
        if (stani3estimate_exportcsv==TRUE) {
            file.csv <- file.path(tempdir, paste0('STAN3_4IO.csv'))
            data.out <- data
            data.out <- data.out[!is.na(data.out$value),]
            write.csv(data.out, file = file.csv, row.names = FALSE, na = "")
        }

        ## export XLS tables if selected
        if (stani3estimate_exportxls==TRUE) {

            ## cou <- exportcou[1] # temporary
            ## cou <- "GBR"
            for (cou in unique(data$cou)) {
                ##
                ## cat(paste0(cou, '\n'))
                data.cou <- data[data$cou==cou,]

                ## file = file.path(path.out, paste0('STAN_', cou), paste0('STAN_', cou, '.xls'))
                file.xls <- file.path(tempdir, paste0('STAN3_4IO_', cou, '.xls'))

                ## wb <- loadWorkbook(filename = file , create = TRUE)
                wb <- loadWorkbook(filename = file.xls , create = TRUE)
                setMissingValue(wb, value = "...")
                ##
                ## Set the data format for numeric columns (cells)
                ## (keeping the defaults for all other data types)
                ## setDataFormatForType(wb, type = XLC$"DATA_TYPE.NUMERIC", format = "0.00")
                setDataFormatForType(wb, type = XLC$"DATA_TYPE.NUMERIC", format = "0")
                ## Set style action to 'data format only'
                setStyleAction(wb, XLC$"STYLE_ACTION.DATA_FORMAT_ONLY")


                for (var in unique(data.cou$var)) {
                    ## var <- "VALU"
                    data.var <- data.cou[data.cou$var==var,]
                    data.var <- dcast(data.var, ind ~ year, value.var = "value")
                    ## add missing industries
                    ## nameind.out <- as.character(STANi3.INDA34All)
                    nameind.out <- nameind
                    data.var <- merge(data.var, data.frame(ind = nameind.out), all.y = TRUE)
                    data.var$ind <- factor(data.var$ind, levels = nameind.out)
                    data.var <- data.var[order(data.var$ind),]
                    ## can we remove underscore from year?
                    ## names(data.var)[2:length(names(data.var))] <- paste0('_',names(data.var)[2:length(names(data.var))])

                    ## class(data.var[3,])

                    createSheet(object = wb, name = as.character(var))

                    writeWorksheet(wb, data = data.var, sheet = as.character(var), startRow=1, startCol=1, header=TRUE)

                    createFreezePane(wb, sheet = as.character(var), colSplit = 2, rowSplit = 2, leftColumn = 2, topRow = 2)

                }
                saveWorkbook(wb)
                ## file.xls
                ## C:\Users\werth_b\AppData\Local\Temp\RtmpOqSCLY\STAN3_4IO_GBR.xls
                ## C:\Users\werth_b\AppData\Local\Temp\RtmpOqSCLY\STAN3_4IO_BEL.xls
                ## file.remove(file = "C:\\Users\\werth_b\\AppData\\Local\\Temp\\RtmpOqSCLY\\STAN3_4IO_GBR.xls")
                ## }
            }

        }

        zip(zipfile = zipfile, files = tempdir, extras = "-j")

    }}

ace_stani3Estimate <- function(result = .stani3Estimate()) {
    if (length(result) > 0) {
        return("test")
    }}
