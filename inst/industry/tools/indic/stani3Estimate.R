## ################
## Data preparation
## ################
data(stanDim)
##
if(testingRadiant==FALSE)
{
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
        c("STANandBTD",    "STAN\nand\nBTD",    "#C0504D", "l", 1, NA,  TRUE,  FALSE, 6, 3), # red
        c("STANandBTDi4",  "STAN\nand\nBTDi4",  "#C0504D", "l", 5, NA,  TRUE,  FALSE, 6, 4), # red, dashed
        c("BTDIXE",        "BTD\nIXE",          "#C0504D", "l", 1, NA,  FALSE, FALSE, 6, 3), # red
        c("OECDSUT112013", "OECD\nSUT\n112013", "#4F81BD", "l", 5, NA,  TRUE,  FALSE, 6, 3), # blue, dashed
        c("WIOT042012",    "WIOT\n042012",      "#9BBB59", "l", 1, NA,  TRUE,  FALSE, 0, 3), # green
        c("WIOT112013",    "WIOT\n112013",      "#9BBB59", "l", 1, NA,  TRUE,  FALSE, 0, 3), # green
        c("INDSTAT32",     "IND\nSTAT\n32",     "#8064A2", "l", 1, NA,  TRUE,  FALSE, 0, 3), # magenta
        c("UNSDSNA2013",   "UNSD\nSNA\n2013",   "#F79646", "l", 1, NA,  TRUE,  FALSE, 6, 3), # orange
        c("UNDATA203CON",  "UNDATA\n203\nCON",  "#F79646", "l", 5, NA,  TRUE,  FALSE, 6, 3), # orange, dashed
        c("UNDATA203100",  "UNDATA\n203\n100",  "#F79646", "l", 1, NA,  TRUE,  FALSE, 6, 3), # orange
        c("UNDATA203200",  "UNDATA\n203\n200",  "#F79646", "l", 1, NA,  TRUE,  FALSE, 6, 3), # orange
        c("UNDATA203300",  "UNDATA\n203\n300",  "#F79646", "l", 1, NA,  TRUE,  FALSE, 6, 3), # orange
        c("UNDATA203400",  "UNDATA\n203\n400",  "#F79646", "l", 1, NA,  TRUE,  FALSE, 6, 3), # orange
        c("UNDATA203500",  "UNDATA\n203\n500",  "#F79646", "l", 1, NA,  TRUE,  FALSE, 6, 3), # orange
        c("NSONAPATCH",    "NSO\nNAPATCH",      "#C0504D", "l", 1, NA,  TRUE,  FALSE, 6, 3), # red
        c("EUNAIOR1",      "EU\nNAIO\nR1",      "black",   "l", 5, NA,  TRUE,  FALSE, 6, 3), # black, dashed
        c("ICIO052013",    "ICIO\n052013",      "black",   "p", 0, 19, TRUE,  FALSE, 0, 3), # black, dots
        c("PATCHEXT",      "PATCH\nEXT",        "#4F81BD", "b", 5, 0,  TRUE,  TRUE,  6, 3), # blue, dashed
        c("PATCHDET",      "PATCH\nDET",        "#4F81BD", "b", 5, 0,  TRUE,  TRUE,  6, 3) # blue, dashed
        )
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
    ##
    dat <- isolate(values[["STANNAi3"]])
    ## class(dat)
    ## sou <- names(dat)[1]
    for (sou in names(dat))
    {
        eval(parse(text = paste0(sou, ' <- dat$', sou)))
    }
    DATA.STANandBTD <- rbind(DATA.STAN, DATA.BTD)
    ## data(STANNAi4)
    dat <- isolate(values[["STANNAi4"]])
    for (sou in names(dat))
    {
        eval(parse(text = paste0(sou, ' <- dat$', sou)))
    }
    DATA.STAN <- DATA.STAN[DATA.STAN$var%in%DATA.STANandBTD$var,]
    DATA.BTD <- DATA.BTD[DATA.BTD$var%in%DATA.STANandBTD$var,]
    ## load(paste0(PATH.SASi4,'DATA_in\\SNA\\SNA_PPEX.rda'))
    DATA.STAN <- merge(DATA.STAN, DATA.XRATES[DATA.XRATES$var=="EXCH",], by = c("cou", "year"))
    names(DATA.STAN) <-  sub("var.x", "var", names(DATA.STAN))
    names(DATA.STAN) <-  sub("value.x", "value", names(DATA.STAN))
    DATA.STAN$value[DATA.STAN$var%in%STAN.VARMON] <- DATA.STAN$value[DATA.STAN$var%in%STAN.VARMON] / DATA.STAN$value.y[DATA.STAN$var%in%STAN.VARMON]
    DATA.STAN <- subset(DATA.STAN, select = c("cou", "var", "ind", "year", "value"))
    DATA.STANandBTDi4 <- rbind(DATA.STAN, DATA.BTD)
    ## file <- paste0(PATH.SASi4, "Lists\\MDL_STAN_i4_i3_A64.txt")
    ## line.remove <- c("/* aggregate */")
    ## formula.ind <- SASformula(file=file, line.remove=line.remove)
    ## cat(paste0('c("', formula.ind$ind, '","', formula.ind$formula, '"),\n'))
    ## STANi3.HIERARCHY
    formula.indi4i3 <- rbind.data.frame(
        c("CTOTAL","DTOTAL"),
        c("C01T05","D01T03"),
        c("C10T14","D05T09"),
        c("C15T16","D10T12"),
        c("C17T19","D13T15"),
        c("C20","D16"),
        c("C21T22","D17+D18+D58"),
        c("C23","D19"),
        c("C24","D20T21"),
        c("C25","D22"),
        c("C26","D23"),
        c("C27","D24"),
        c("C28","D25"),
        c("C29","D28"),
        c("C30T33X","D26"),
        c("C31","D27"),
        c("C34","D29"),
        c("C35","D30"),
        c("C36T37","D31T33"),
        c("C40T41","D35+D36"),
        c("C45","D41T43"),
        c("C50T52","D45T47+D95"),
        c("C55","D55T56"),
        c("C60T63","D49+D50+D51+D52+D79"),
        c("C64","D53+D61"),
        c("C65T67","D64T66"),
        c("C70","D68"),
        c("C71","D77"),
        c("C72","D62T63"),
        c("C73","D72"),
        c("C74","D69T71+D73T75+D78+D80T82"),
        c("C75T99","D84+D85+D86T88+D37T39+D59T60+D90T93+D94+D96+D97T98"),
        c("C75","D84"),
        c("C80","D85"),
        c("C85","D86T88"),
        c("C90T93","D37T39+D59T60+D90T93+D94+D96"),
        c("C95","D97T98")
        ## c("C75T99","C75+C80+C85+C90T93+C95")
        )
    names(formula.indi4i3) <- c("ind", "formula")
    ## sou <- ui.stani3Estimate.sou.isic4[1]
    for (sou in ui.stani3Estimate.sou.isic4)
    {
        eval(parse(text = paste0('data <- DATA.', sou)))
        data <- dcast(data, cou + var + year ~ ind, value.var = "value")
        id.vars <- c("cou", "var", "year")
        data <- calcFormula(data=data, formula=formula.indi4i3, id.vars=id.vars)
        data <- melt(data, id.vars = id.vars, variable.name = "ind", na.rm = TRUE)
        data <- subset(data, select = c("cou", "var", "ind", "year", "value"))
        ## data <- data[!is.na(data$ind),]
        eval(parse(text = paste0('DATA.', sou, ' <- data')))
    }
    ##
    for (sou in ui.stani3Estimate.sou[!ui.stani3Estimate.sou%in%ui.stani3Estimate.sou.estim])
    {
        ## add source name
        eval(parse(text = paste0('DATA.', sou, '$sou <- "', sou, '"')))
        ## modify value unit of monetary variables
        ## eval(parse(text = paste0('DATA.', sou, '$value <- DATA.', sou, '$value * 10^(-', stani3Estimate.VIS.SOU$unit[stani3Estimate.VIS.SOU$sou==sou], ')')))
        eval(parse(text = paste0('DATA.', sou, '$value[DATA.', sou, '$var%in%STAN.VARMON] <- DATA.', sou, '$value[DATA.', sou, '$var%in%STAN.VARMON] * 10^(-', stani3Estimate.VIS.SOU$unit[stani3Estimate.VIS.SOU$sou==sou], ')')))
        ## ## order by factor levels -> not required, taking place in data table
        ## eval(parse(text = paste0('DATA.', sou, ' <- DATA.', sou, '[order(DATA.', sou, '$cou, DATA.', sou, '$var, DATA.', sou, '$ind, DATA.', sou, '$year),]')))
        ## pivot to add aggregates
        eval(parse(text = paste0('DATA.', sou, ' <- dcast(DATA.', sou, ', cou + sou + var + year ~ ind, value.var = "value")')))
        ## add aggregates
        ## eval(parse(text = paste0('DATA.', sou, ' <- indAggregate(DATA.', sou, ', isic = 3)')))
        eval(parse(text = paste0('DATA.', sou, ' <- indAggregate(DATA.', sou, ', isic = 3, naAsZero = TRUE, fill2D = TRUE, missing.2d = c("C95", "C99"))')))
        ## ## create C75T99 with modified indAggregate function where only selected 2-digit sectors are filled with zero
        ## DATA.STANandBTD.d <- dcast(DATA.STANandBTD, cou + sou + var + year ~ ind, value.var = "value")
        ## n(DATA.STANandBTD.d)
        ## test <- indAggregate(
        ##     data=DATA.STANandBTD.d
        ##     ,
        ##     isic=3
        ##     ,
        ##     cumulative=FALSE
        ##     ,
        ##     naAsZero=TRUE
        ##     ,
        ##     fill2D=TRUE
        ##     ,
        ##     missing.2d=c("C95", "C99")
        ##     )
        ## n(test)
        ## View(test)

        ## pivot back to long format
        eval(parse(text = paste0('DATA.', sou, ' <- melt(DATA.', sou, ', id.vars = c("cou", "sou", "var", "year"), variable.name = "ind", na.rm = TRUE)')))
        ## ## remove missing values
        ## eval(parse(text = paste0('DATA.', sou, ' <- DATA.', sou, '[!is.na(DATA.', sou, '$value),]')))
    }
    ## for (sou in ui.stani3Estimate.sou[!ui.stani3Estimate.sou%in%ui.stani3Estimate.sou.estim]) eval(parse(text = paste0('print("', sou, '");print(n(DATA.', sou, '))')))
    ## combine all data sources
    command.data.all <- NULL
    for (sou in c(ui.stani3Estimate.sou[!ui.stani3Estimate.sou%in%ui.stani3Estimate.sou.estim]))
    {
        command.data.all <- paste0(command.data.all, paste0('DATA.', sou, ','))
    }
    ## remove trailing comma from last data set
    command.data.all <- toString(paste0('stani3Estimate.data.all <- rbind(', substr(command.data.all, 1, nchar(command.data.all)-1), ')'))
    eval(parse(text = command.data.all))
}
## save(stani3Estimate.data.all, file = file.path(PATH.SASi3, "DATA_in", "R", "STANNAi3.rda"))
## write.csv(stani3Estimate.data.all, file = file.path(PATH.SASi3, "DATA_in", "R", "STANNAi3.csv"))

## UI lists

ui.stani3estimate.tabletype <- c("Data", "Calculation")
ui.stani3estimate.datatabletype <- c("Sources", "Hierarchy")
ui.stani3estimate.plottype <- c("Lines", "Bars")
##
## ui.stani3Estimate.cou <- c("AUS", "AUT", "BEL", "CAN", "CHL", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ISL", "ISR", "ITA", "JPN", "KOR", "LUX", "MEX", "NLD", "NZL", "NOR", "POL", "PRT", "SVK", "SVN", "ESP", "SWE", "CHE", "TUR", "GBR", "USA", "BRA", "CHN", "IND", "IDN", "RUS", "ZAF", "BGR", "BRN", "CYP", "HKG", "KHM", "LKA", "LTU", "LVA", "MLT", "MYS", "ROU", "SAU", "SGP", "THA", "VNM")

## ui.stani3Estimate.cou.agg <- c("COU", "COUENO", "COUKPC", "COUSEA", "COUOTH")
ui.stani3Estimate.cou.agg <- names(STAN.COU)
ui.stani3Estimate.cou <- STAN.COU[["ICIO"]]
ui.stani3Estimate.cou <- ui.stani3Estimate.cou[!ui.stani3Estimate.cou=="ROW"]
## List Nori: \\asap5\STI\Progs\STAN\I-O\2014sut-io\doc\icio-target cou.xlsx

## ind1 <- STANi3.INDA60All
## STANi3.INDA60All <- unique(c(STANi3.IND[["A6"]], STANi3.IND[["A18"]], STANi3.IND[["A34"]], STANi3.IND[["A60All"]]))
## STANi3.INDA60All <- factor(STANi3.INDA60All, levels = STANi3.IND[["levels"]])
## STANi3.INDA60All <- STANi3.INDA60All[order(STANi3.INDA60All)]
## ind2 <- STANi3.INDA60All
merge(data.frame(ind=ind1, vec="ind1"), data.frame(ind=ind2, vec="ind2"), by = "ind", all = TRUE)

ui.stani3Estimate.ind.agg <- c("A6", "A18", "A34", "A60All") # "A46" list not included; UNSDSNA C65T99 missing
ui.stani3Estimate.ind <- STANi3.INDA60All

## ui.stani3Estimate.ind <- NULL
## for (ind in as.character(STANi3.INDA60All))
## {
##     ind.list <- list(ind)
##     names(ind.list) <- paste0(ind, ": ", STANi3.INDLABEL$label[STANi3.INDLABEL$ind==ind])
##     ui.stani3Estimate.ind <- c(ui.stani3Estimate.ind, ind.list)
## }
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
ui.stani3Estimate.var2 <- union("", ui.stani3Estimate.var)

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
## ##
## ui.stani3Estimate.est.array[, , , "EXT", "MAIN"] <- ""
## ui.stani3Estimate.est.array[, , , "EXT", "SEC"] <- ""
## ui.stani3Estimate.est.array[, , , "DET", "MAIN"] <- "UNDATA203CON" # UNSDSNA2013
## ui.stani3Estimate.est.array[, , , "DET", "SEC"] <- "INDSTAT32"
## ##
## namecou <- STAN.COU[["OECD"]]
## ui.stani3Estimate.est.array[namecou, , , "EXT", "MAIN"] <- "STANandBTD"
## ui.stani3Estimate.est.array[namecou, , , "EXT", "SEC"] <- "STANandBTDi4"
## ui.stani3Estimate.est.array[namecou, , , "DET", "MAIN"] <- "PATCHEXT"
## ui.stani3Estimate.est.array[namecou, , , "DET", "SEC"] <- "INDSTAT32"
## ##
## namecou <- c("BRN", "MYS")
## ui.stani3Estimate.est.array[namecou, , , "EXT", "MAIN"] <- ""
## ui.stani3Estimate.est.array[namecou, , , "EXT", "SEC"] <- ""
## ui.stani3Estimate.est.array[namecou, , , "DET", "MAIN"] <- "NSONAPATCH"
## ui.stani3Estimate.est.array[namecou, , , "DET", "SEC"] <- "INDSTAT32"
## ##
## namecou <- c("CHN", "IDN", "KHM", "VNM")
## ui.stani3Estimate.est.array[namecou, , , "EXT", "MAIN"] <- ""
## ui.stani3Estimate.est.array[namecou, , , "EXT", "SEC"] <- ""
## ui.stani3Estimate.est.array[namecou, , , "DET", "MAIN"] <- "UNSDSNA2013"
## ui.stani3Estimate.est.array[namecou, , , "DET", "SEC"] <- "INDSTAT32"
## ##
## save(ui.stani3Estimate.est.array, file = "data/data_init/stani3Estimate.rda")

load("data/data_init/stani3Estimate.rda")
## Example:
## ui.stani3Estimate.est.array["AUT", "VALU", "C15T37", "EXT",] <- c("STANandBTD", "STANandBTDi4")

##
## UIs
##
output$uiSe_diff <- renderUI({
    wellPanel(
        h5("Calculation:"),
        checkboxInput("stani3estimate_diff", "Difference (Default: Ratio)", FALSE)
        )
})

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

output$uiSe_ind <- renderUI({
    ## if (length(result) > 0) {
        if ((input$tabs_stani3Estimate=="Plots" & input$stani3estimate_plottype=="Lines") |
            (input$tabs_stani3Estimate=="Tables" & input$stani3estimate_tabletype=="Calculation") )
        {
            selectInput("stani3estimate_ind", "Industry:", ui.stani3Estimate.ind,
                        selected = state_init_list("stani3estimate_ind", "C15T37", ui.stani3Estimate.ind),
                        multiple = FALSE)
        } else {
            selectInput("stani3estimate_ind", "Industry:", union(ui.stani3Estimate.ind.agg, ui.stani3Estimate.ind),
                        ## selected = "A6",
                        selected = state_init_multvar("stani3estimate_ind", "CTOTAL", union(ui.stani3Estimate.ind.agg, ui.stani3Estimate.ind)),
                        multiple = TRUE)
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
                        selected = state_init_multvar("stani3estimate_ind.parent.select", ind.parent[1], ind.parent), # ind.parent[1]
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
                        selected = state_init_multvar("stani3estimate_ind.peers.select", input$stani3estimate_ind, ind.peers), # input$stani3estimate_ind
                        multiple = TRUE)
        }
    ## }
})

output$uiSe_estExtend <- renderUI({
    ##
    if (input$stani3estimate_sou_ext_reloadButton != 0) {
        load("data/data_init/stani3Estimate.rda")
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

output$uiSe_estDetail <- renderUI({
    ##
    if (input$stani3estimate_sou_det_reloadButton != 0) {
        load("data/data_init/stani3Estimate.rda")
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



output$ui_stani3Estimate <- renderUI({

    doLogin()
    if (loginData$LoggedIn) {

        list(

            conditionalPanel(condition="input.tabs_stani3Estimate=='Plots'",
                             wellPanel(
                                 checkboxInput("stani3estimate_viz_plot_controls", "Plot options", FALSE),
                                 conditionalPanel(condition = "input.stani3estimate_viz_plot_controls==true",
                                                  ## htmlOutput("ui_plot_options"),
                                                  sliderInput(inputId = "stani3Estimate_viz_plot_height", label = "Height:", min = 400, max = 1000, value = 750, step = 50),
                                                  sliderInput(inputId = "stani3Estimate_viz_plot_width", label = "Width:", min = 400, max = 1200, value = 750, step = 50),
                                                  conditionalPanel(condition="input.stani3estimate_plottype=='Bars'",
                                                                   checkboxInput("stani3estimate_horiz", "Horizontal plot", FALSE)
                                                                   )
                                                  )
                                 ),
                             selectInput("stani3estimate_plottype", "Plottype:", ui.stani3estimate.plottype,
                                         selected = "Lines", multiple = FALSE),
                             conditionalPanel(condition="input.stani3estimate_plottype=='Bars'",
                                              selectInput("stani3estimate_singlesou", "Source:", ui.stani3Estimate.sou,
                                                          selected = "STANandBTD", multiple = FALSE)
                                              )
                             )
            ,
            conditionalPanel(condition="input.tabs_stani3Estimate=='Tables'",
                             selectInput("stani3estimate_tabletype", "Tabletype:", ui.stani3estimate.tabletype,
                                         selected = "Data", multiple = FALSE)
                             )
            ,
            conditionalPanel(condition="input.tabs_stani3Estimate=='DataTables'",
                             ## checkboxInput("stani3estimate_sou_all_print", "Show Estimation sources", FALSE)
                             selectInput("stani3estimate_datatabletype", "DataTabletype:", ui.stani3estimate.datatabletype,
                                         selected = "Sources", multiple = FALSE)
                             )
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
                                 )
                )
            ,
            ## Industries
            wellPanel(
                htmlOutput("uiSe_ind"),
                conditionalPanel(condition="input.tabs_stani3Estimate=='DataTables'",
                                 checkboxInput("stani3estimate_STANi3.INDLABEL", "ISIC3 Industry label", FALSE)
                                 )
                ,
                conditionalPanel(condition="input.tabs_stani3Estimate=='Tables'",
                                 checkboxInput("stani3estimate_missing", "Show missing industries", FALSE)
                                 )
                ,
                conditionalPanel(condition="input.stani3estimate_var2==''",
                                 selectInput("stani3estimate_ind2", "Denominator or substractor industry:", ui.stani3Estimate.ind2)
                                 )
                )
            ,
            conditionalPanel(condition="input.stani3estimate_var2!='' | input.stani3estimate_ind2!=''",
                             htmlOutput("uiSe_diff")
                             )
            ,
            htmlOutput("uiSe_plot"),
            wellPanel(
                sliderInput("stani3estimate_time",
                            "Time Range:",
                            value = c(1994,2012),
                            min = 1980,
                            max = 2012,
                            step = 1,
                            format="#")
                ),
            wellPanel(
                ## conditionalPanel(condition="input.tabs_stani3Estimate!=2",
                checkboxInput("stani3estimate_extend", "Extend series", FALSE)
                ## )
                ,
                conditionalPanel(condition="input.stani3estimate_extend==true",
                                 actionButton("stani3estimate_sou_ext_reloadButton", "reload"),
                                 htmlOutput("uiSe_estExtend"),
                                 actionButton("stani3estimate_sou_ext_saveButton", "save"),
                                 helpText('\n"save" overwrites current setup of sources for dimension members selected in export section.')
                                 )
                ),
            wellPanel(
                ## conditionalPanel(condition="input.tabs_stani3Estimate!=2",
                checkboxInput("stani3estimate_detail", "Apply detailed industry share", TRUE),
                conditionalPanel(condition="input.stani3estimate_detail==true",
                                 actionButton("stani3estimate_sou_det_reloadButton", "reload"),
                                 htmlOutput("uiSe_estDetail"),
                                 actionButton("stani3estimate_sou_det_saveButton", "save"),
                                 helpText('\n"save" overwrites current setup of sources for dimension members selected in export section.'),
                                 ## ),
                                 wellPanel(
                                     htmlOutput("uiSe_indParent"),
                                     htmlOutput("uiSe_indPeers")
                                     )
                                 )
                )
            ,
            wellPanel(
                ## conditionalPanel(condition="input.tabs_stani3Estimate!=2",
                checkboxInput("stani3estimate_souall", "Select all sources", TRUE),
                conditionalPanel(condition="input.stani3estimate_souall==false",
                                 ## selectInput("sou", "Source:", as.list(ui.stani3Estimate.sou), selected = c("STANandBTD", "STANandBTDi4"), multiple = TRUE)
                                 selectInput("stani3estimate_sou", "Source:", as.list(ui.stani3Estimate.sou), selected = c("STANandBTD", "STANandBTDi4"), multiple = TRUE)
                                 ))
            ## )
            ,
            ## conditionalPanel(condition="input.tabs_stani3Estimate==2",
            ## helpText('Select the dimensions for exporting',
            ##          'to csv and to pdf.',
            ##          'Time Range is taken into account,',
            ##          'as well as the source selection.',
            ##          '"All Industries" are shrunk',
            ##          'to fit one page.'),
            ## helpText('Select the dimensions for exporting',
            ##          'to csv.',
            ##          'Time Range is taken into account,',
            ##          'as well as the source selection.'),
            wellPanel(
                h5("Export Estimates"),
                ## checkboxInput("stani3estimate_allcou", "Select all countries", FALSE),
                selectInput("stani3estimate_exportcou", "Countries:", union(ui.stani3Estimate.cou.agg, ui.stani3Estimate.cou),
                            selected = c(""), multiple = TRUE),
                ## checkboxInput("stani3estimate_allvar", "Select all variables", FALSE),
                selectInput("stani3estimate_morevar", "Variables:", ui.stani3Estimate.var, selected = c("PROD", "VALU"), multiple = TRUE),
                checkboxInput("stani3estimate_allind", "Select all industries", FALSE),
                downloadButton('download_stani3Estimate', 'Download XLS') # from radiant.R: paste0('download_', fun_label)
            ## downloadButton('downloadChart', 'Download PDF')
                ),
            helpAndReport("STAN ISIC3 Estimate","stani3Estimate",inclMD("tools/help/stani3Estimate.md"))
            ) # list(...

    } else
    {
        h3("Please log in")
    }

})

output$stani3Estimate <- renderUI({
    ## for input-output
    statTabPanel(menu_name = "STAN", # menu_name: for side bar - coincide with navbarMenu
                 fun_name = "STAN ISIC3 Estimate",   # fun_name
                 rfun_label = ".stani3Estimate", # rfun_label
                 fun_label = "stani3Estimate" # fun_label
                 ## ,rChart_lib = input$stani3estimate_rchartlib
                 ,fun_tabs = c("Tables", "Plots", "DataTables")
                 )
})


## ######################
## Input test
## ######################

## input <- list(
##     nav_radiant="STAN ISIC3 Estimate",
##     ## stani3estimate_allcou=FALSE,
##     stani3estimate_exportcou=c("COU","COUENO"),
##     ## stani3estimate_allvar=FALSE,
##     stani3estimate_morevar=c("VALU","PROD"),
##     stani3estimate_allind=TRUE,
##     tabs_stani3Estimate=1,
##     stani3estimate_cou="AUT", # VNM
##     stani3estimate_detail=FALSE,
##     stani3estimate_diff=FALSE,
##     stani3estimate_extend=TRUE,
##     stani3estimate_horiz=FALSE,
##     stani3estimate_ind="CTOTAL", # C15T16
##     stani3estimate_ind2="",
##     stani3estimate_ind.peers.select="C15T16",
##     stani3estimate_ind.parent.select=NULL,
##     stani3estimate_limit.yrange.max=FALSE,
##     stani3estimate_limit.yrange.min=FALSE,
##     stani3estimate_missing=FALSE,
##     stani3estimate_singlesou="UNSDSNA2013",
##     stani3estimate_sou=c("INDSTAT32", "UNSDSNA2013"),
##     stani3estimate_souall=FALSE,
##     stani3estimate_sou_det_main="UNSDSNA2013",
##     stani3estimate_sou_det_sec="INDSTAT32",
##     stani3estimate_sou_det_sec2="",
##     stani3estimate_sou_det_sec3="",
##     stani3estimate_sou_ext_main="STANandBTD",
##     stani3estimate_sou_ext_sec="STANandBTDi4",
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
        ## stani3estimate_allcou = input$stani3estimate_allcou,
        stani3estimate_exportcou = input$stani3estimate_exportcou,
        ## stani3estimate_allvar = input$stani3estimate_allvar,
        stani3estimate_morevar = input$stani3estimate_morevar,
        stani3estimate_allind = input$stani3estimate_allind,
        stani3estimate_cou = input$stani3estimate_cou,
        stani3estimate_detail = input$stani3estimate_detail,
        stani3estimate_diff = input$stani3estimate_diff,
        stani3estimate_extend = input$stani3estimate_extend,
        stani3estimate_horiz = input$stani3estimate_horiz,
        stani3estimate_ind = input$stani3estimate_ind,
        stani3estimate_ind2 = input$stani3estimate_ind2,
        stani3estimate_ind.peers.select = input$stani3estimate_ind.peers.select,
        stani3estimate_ind.parent.select = input$stani3estimate_ind.parent.select,
        stani3estimate_limit.yrange.max = input$stani3estimate_limit.yrange.max,
        stani3estimate_limit.yrange.min = input$stani3estimate_limit.yrange.min,
        stani3estimate_missing = input$stani3estimate_missing,
        stani3estimate_plottype = input$stani3estimate_plottype,
        stani3estimate_singlesou = input$stani3estimate_singlesou,
        stani3estimate_sou = input$stani3estimate_sou,
        stani3estimate_souall = input$stani3estimate_souall,
        ## stani3estimate_sou_all_print = input$stani3estimate_sou_all_print,
        stani3estimate_datatabletype = input$stani3estimate_datatabletype,
        stani3estimate_sou_det_main = input$stani3estimate_sou_det_main,
        stani3estimate_sou_det_sec = input$stani3estimate_sou_det_sec,
        stani3estimate_sou_ext_main = input$stani3estimate_sou_ext_main,
        stani3estimate_sou_ext_sec = input$stani3estimate_sou_ext_sec,
        stani3estimate_sou_ext_reloadButton = input$stani3estimate_sou_ext_reloadButton,
        stani3estimate_sou_ext_saveButton = input$stani3estimate_sou_ext_saveButton,
        stani3estimate_STANi3.INDA6 = input$stani3estimate_STANi3.INDA6,
        stani3estimate_STANi3.INDA18 = input$stani3estimate_STANi3.INDA18,
        stani3estimate_STANi3.INDA34 = input$stani3estimate_STANi3.INDA34,
        stani3estimate_STANi3.INDA46 = input$stani3estimate_STANi3.INDA46,
        stani3estimate_STANi3.INDA60 = input$stani3estimate_STANi3.INDA60,
        stani3estimate_STANi3.INDLABEL = input$stani3estimate_STANi3.INDLABEL,
        stani3estimate_tabletype = input$stani3estimate_tabletype,
        stani3estimate_time = input$stani3estimate_time,
        stani3estimate_var = input$stani3estimate_var,
        stani3estimate_var2 = input$stani3estimate_var2,
        stani3estimate_yrange = input$stani3estimate_yrange,
        stani3Estimate_viz_plot_height = input$stani3Estimate_viz_plot_height,
        stani3Estimate_viz_plot_width = input$stani3Estimate_viz_plot_width
        )

})


observe({
    if (is.null(input$stani3estimate_sou_ext_saveButton) || input$stani3estimate_sou_ext_saveButton == 0) return()
    ## isolate({
    ## if (input$stani3estimate_allcou==TRUE) {
    ##     cou <- seq(along = ui.stani3Estimate.cou)
    ## } else {
    ## cou <- match(input$stani3estimate_cou, ui.stani3Estimate.cou)
    cou <- match(union(input$stani3estimate_cou, input$stani3estimate_exportcou), ui.stani3Estimate.cou)
    ## }
    ## if (input$stani3estimate_allvar==TRUE) {
    ##     var <- seq(along = ui.stani3Estimate.var)
    ## } else {
    ## var <- match(input$stani3estimate_var, ui.stani3Estimate.var)
    var <- match(union(input$stani3estimate_var, input$stani3estimate_morevar), ui.stani3Estimate.var)
    ## }
    if (input$stani3estimate_allind==TRUE) {
        ind <- seq(along = ui.stani3Estimate.ind)
    } else {
        ind <- match(input$stani3estimate_ind, ui.stani3Estimate.ind)
    }
    load("data/data_init/stani3Estimate.rda")
    ui.stani3Estimate.est.array[cou,var,ind,"EXT","MAIN"] <- input$stani3estimate_sou_ext_main
    ui.stani3Estimate.est.array[cou,var,ind,"EXT","SEC"] <- toString(input$stani3estimate_sou_ext_sec)
    save(ui.stani3Estimate.est.array, file = "data/data_init/stani3Estimate.rda")
    ## })
})

observe({
    if (is.null(input$stani3estimate_sou_det_saveButton) || input$stani3estimate_sou_det_saveButton == 0) return()
    ## isolate({
    ## if (input$stani3estimate_allcou==TRUE) {
    ##     cou <- seq(along = ui.stani3Estimate.cou)
    ## } else {
    ## cou <- match(input$stani3estimate_cou, ui.stani3Estimate.cou)
    cou <- match(union(input$stani3estimate_cou, input$stani3estimate_exportcou), ui.stani3Estimate.cou)
    ## }
    ## if (input$stani3estimate_allvar==TRUE) {
    ##     var <- seq(along = ui.stani3Estimate.var)
    ## } else {
    ## var <- match(input$stani3estimate_var, ui.stani3Estimate.var)
    var <- match(union(input$stani3estimate_var, input$stani3estimate_morevar), ui.stani3Estimate.var)
    ## }
    if (input$stani3estimate_allind==TRUE) {
        ind <- seq(along = ui.stani3Estimate.ind)
    } else {
        ind <- match(input$stani3estimate_ind, ui.stani3Estimate.ind)
    }
    load("data/data_init/stani3Estimate.rda")
    ui.stani3Estimate.est.array[cou,var,ind,"DET","MAIN"] <- input$stani3estimate_sou_det_main
    ui.stani3Estimate.est.array[cou,var,ind,"DET","SEC"] <- toString(input$stani3estimate_sou_det_sec)
    save(ui.stani3Estimate.est.array, file = "data/data_init/stani3Estimate.rda")
    print("saved detail information")
    ## })
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
    ## stani3estimate_allcou = stani3estimate_allcou,
    stani3estimate_exportcou = stani3estimate_exportcou,
    ## stani3estimate_allvar = stani3estimate_allvar,
    stani3estimate_morevar = stani3estimate_morevar,
    stani3estimate_allind = stani3estimate_allind,
    stani3estimate_cou = stani3estimate_cou,
    stani3estimate_detail = stani3estimate_detail,
    stani3estimate_diff = stani3estimate_diff,
    stani3estimate_extend = stani3estimate_extend,
    stani3estimate_horiz = stani3estimate_horiz,
    stani3estimate_ind = stani3estimate_ind,
    stani3estimate_ind2 = stani3estimate_ind2,
    stani3estimate_ind.peers.select = stani3estimate_ind.peers.select,
    stani3estimate_ind.parent.select = stani3estimate_ind.parent.select,
    stani3estimate_limit.yrange.max = stani3estimate_limit.yrange.max,
    stani3estimate_limit.yrange.min = stani3estimate_limit.yrange.min,
    stani3estimate_missing = stani3estimate_missing,
    stani3estimate_plottype = stani3estimate_plottype,
    stani3estimate_singlesou = stani3estimate_singlesou,
    stani3estimate_sou = stani3estimate_sou,
    stani3estimate_souall = stani3estimate_souall,
    ## stani3estimate_sou_all_print = stani3estimate_sou_all_print,
    stani3estimate_datatabletype = stani3estimate_datatabletype,
    stani3estimate_sou_det_main = stani3estimate_sou_det_main,
    stani3estimate_sou_det_sec = stani3estimate_sou_det_sec,
    stani3estimate_sou_det_reloadButton = stani3estimate_sou_det_reloadButton,
    stani3estimate_sou_det_saveButton = stani3estimate_sou_det_saveButton,
    stani3estimate_sou_ext_main = stani3estimate_sou_ext_main,
    stani3estimate_sou_ext_sec = stani3estimate_sou_ext_sec,
    stani3estimate_sou_ext_reloadButton = stani3estimate_sou_ext_reloadButton,
    stani3estimate_sou_ext_saveButton = stani3estimate_sou_ext_saveButton,
    stani3estimate_STANi3.INDA6 = stani3estimate_STANi3.INDA6,
    stani3estimate_STANi3.INDA18 = stani3estimate_STANi3.INDA18,
    stani3estimate_STANi3.INDA34 = stani3estimate_STANi3.INDA34,
    stani3estimate_STANi3.INDA46 = stani3estimate_STANi3.INDA46,
    stani3estimate_STANi3.INDA60 = stani3estimate_STANi3.INDA60,
    stani3estimate_STANi3.INDLABEL = stani3estimate_STANi3.INDLABEL,
    stani3estimate_tabletype = stani3estimate_tabletype,
    stani3estimate_time = stani3estimate_time,
    stani3estimate_var = stani3estimate_var,
    stani3estimate_var2 = stani3estimate_var2,
    stani3estimate_yrange = stani3estimate_yrange,
    stani3Estimate_viz_plot_height = stani3Estimate_viz_plot_height,
    stani3Estimate_viz_plot_width = stani3Estimate_viz_plot_width
    )
{

    data.all <- stani3Estimate.data.all

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

    ## Create estimates: extend
    if (stani3estimate_extend==TRUE & length(namesou.ext) >= 2) {
        sources.ext <- data.frame(cou = stani3estimate_cou,
                                  var = stani3estimate_var,
                                  ind = stani3estimate_ind,
                                  est = "EXT",
                                  MAIN = namesou.ext[1],
                                  SEC = namesou.ext[2:length(namesou.ext)],
                                  stringsAsFactors = FALSE)
        data.patch.ext <- estimate(data = data.all, sources = sources.ext, period = nameyear)
    } else {
        data.patch.ext <- NULL
    }
    ## ## Create estimates: extend
    ## if (stani3estimate_extend==TRUE & length(namesou.ext) >= 2) {
    ##     data.ext.start <- data.all[data.all$cou==stani3estimate_cou &
    ##                                data.all$sou%in%namesou.ext &
    ##                                data.all$var==stani3estimate_var &
    ##                                data.all$ind==stani3estimate_ind &
    ##                                data.all$year%in%nameyear,]
    ##     PATCHEXT <- stan::extend(data = data.ext.start, namesou = namesou.ext)
    ##     newsou <- "PATCHEXT"
    ##     data.patch.ext <- data.frame(cou = stani3estimate_cou,
    ##                                  sou = newsou,
    ##                                  var = stani3estimate_var,
    ##                                  year = PATCHEXT$year,
    ##                                  ind = stani3estimate_ind,
    ##                                  value = PATCHEXT$value)
    ## } else {
    ##     data.patch.ext <- NULL
    ## }

    ## Create estimates: detail
    if (stani3estimate_detail==TRUE & length(namesou.det) >= 2) {
        sources.det <- data.frame(cou = stani3estimate_cou,
                                  var = stani3estimate_var,
                                  ind = stani3estimate_ind,
                                  est = "DET",
                                  MAIN = namesou.det[1],
                                  SEC = namesou.det[2:length(namesou.det)],
                                  stringsAsFactors = FALSE)
        data.patch.det <- estimate(data = data.all, sources = sources.det, period = nameyear)
    } else {
        data.patch.det <- NULL
    }
    ## ## Create estimates: detail
    ## if (stani3estimate_detail==TRUE & length(namesou.det) >= 2) {
    ##     newsou <- "PATCHDET"
    ##     data.det.start <- data.all[data.all$cou==stani3estimate_cou &
    ##                                data.all$sou%in%namesou.det &
    ##                                data.all$var==stani3estimate_var &
    ##                                data.all$ind%in%c(ind.parent[1], ind.peers) &
    ##                                data.all$year%in%nameyear,]
    ##     ## if parent industry in main source: calculate using this number
    ##     if (ind.parent[1]%in%unique(data.det.start$ind[data.det.start$sou==namesou.det[1]])) {
    ##         PATCHDET <- stan::detail(data = data.det.start,
    ##                                  namesou = namesou.det,
    ##                                  ind.parent = ind.parent[1],
    ##                                  ind.peers = ind.peers)
    ##     } else { # if parent industry not in main source, look for first parent
    ##         p <- 2
    ##         parent.in.namesou <- FALSE
    ##         while (p==2 | (p <= length(ind.parent) & parent.in.namesou==FALSE)) {
    ##             data.det.start.p <- data.all[data.all$cou==stani3estimate_cou &
    ##                                          data.all$sou%in%namesou.det &
    ##                                          data.all$var==stani3estimate_var &
    ##                                          data.all$ind%in%c(ind.parent[1:p]) &
    ##                                          data.all$year%in%nameyear,]
    ##             if (ind.parent[p]%in%unique(data.det.start.p$ind[data.det.start.p$sou==namesou.det[1]])) {
    ##                 PATCHDET.p <- detail(data = data.det.start.p,
    ##                                      namesou = namesou.det,
    ##                                      ind.parent = ind.parent[p],
    ##                                      ind.peers = ind.parent)
    ##                 data.patch.det.p <- data.frame(cou = stani3estimate_cou,
    ##                                                sou = newsou,
    ##                                                var = stani3estimate_var,
    ##                                                year = PATCHDET.p$year,
    ##                                                ind = PATCHDET.p$ind,
    ##                                                value = PATCHDET.p$value)
    ##             }
    ##             if (exists("data.patch.det.p")==TRUE) {
    ##                 parent.in.namesou <- ind.parent[p]%in%unique(data.patch.det.p$ind[data.patch.det.p$sou=="PATCHDET"])
    ##             } else {
    ##                 parent.in.namesou <- FALSE
    ##             }
    ##             p = p + 1
    ##         }
    ##         PATCHDET <- detail(data = rbind(data.det.start, data.patch.det.p),
    ##                            namesou = sub(namesou.det[1], newsou, namesou.det),
    ##                            ind.parent = ind.parent[1],
    ##                            ind.peers = ind.peers)
    ##     }
    ##     data.patch.det <- data.frame(cou = stani3estimate_cou,
    ##                                  sou = newsou,
    ##                                  var = stani3estimate_var,
    ##                                  year = PATCHDET$year,
    ##                                  ind = PATCHDET$ind,
    ##                                  value = PATCHDET$value)
    ## } else {
    ##     data.patch.det <- NULL
    ## }

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
    data.table.pivot$ind <- factor(data.table.pivot$ind, levels = ui.stani3Estimate.ind) # nameind
    data.table.pivot <- data.table.pivot[order(data.table.pivot$cou, data.table.pivot$sou, data.table.pivot$var, data.table.pivot$ind),]

    return(list(data.table = data.table,
                data.table2 = data.table2,
                data.table.pivot = data.table.pivot,
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
                ## stani3estimate_exportcou = stani3estimate_exportcou,
                stani3estimate_morevar = stani3estimate_morevar,
                stani3estimate_detail = stani3estimate_detail,
                stani3estimate_extend = stani3estimate_extend,
                stani3estimate_horiz = stani3estimate_horiz,
                stani3estimate_ind = stani3estimate_ind,
                stani3estimate_ind2 = stani3estimate_ind2,
                stani3estimate_STANi3.INDA6 = stani3estimate_STANi3.INDA6,
                stani3estimate_STANi3.INDA18 = stani3estimate_STANi3.INDA18,
                stani3estimate_STANi3.INDA34 = stani3estimate_STANi3.INDA34,
                stani3estimate_STANi3.INDA46 = stani3estimate_STANi3.INDA46,
                stani3estimate_STANi3.INDLABEL = stani3estimate_STANi3.INDLABEL,
                stani3estimate_limit.yrange.min = stani3estimate_limit.yrange.min,
                stani3estimate_limit.yrange.max = stani3estimate_limit.yrange.max,
                stani3estimate_plottype = stani3estimate_plottype,
                stani3estimate_singlesou = stani3estimate_singlesou,
                ## stani3estimate_sou_all_print = stani3estimate_sou_all_print,
                stani3estimate_datatabletype = stani3estimate_datatabletype,
                stani3estimate_tabletype = stani3estimate_tabletype,
                stani3estimate_var2 = stani3estimate_var2
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

    data.table.pivot <- result$data.table.pivot
    data.table2 <- result$data.table2
    stani3estimate_tabletype <- result$stani3estimate_tabletype

    if (stani3estimate_tabletype=="Data")
    {
        ## table output
        ## Tab: Table: Data pivot
        ## output$table_data <- renderTable({
        table <- data.table.pivot
        ## })
    } else if (stani3estimate_tabletype=="Calculation") {
        ##     ## Tab: Table: Calc (display merged data in table)
        ##     output$table_calc <- renderTable({
        ##         data.table2 <- data.table2()
        table <- data.table2
        ##     })
    }

    return(table)

}}

datatables_stani3Estimate <- function(result = .stani3Estimate()) {
    if (length(result) > 0) {

    ## print array with estimation sources
    ## stani3estimate_sou_all_print <- result$stani3estimate_sou_all_print
    ## if (stani3estimate_sou_all_print==TRUE) return(melt(ui.stani3Estimate.est.array))
        stani3estimate_datatabletype <- result$stani3estimate_datatabletype
        if (stani3estimate_datatabletype=="Sources") return(melt(ui.stani3Estimate.est.array))

    stani3estimate_ind <- result$stani3estimate_ind
    ## stani3estimate_STANi3.INDA6 <- result$stani3estimate_STANi3.INDA6
    ## stani3estimate_STANi3.INDA18 <- result$stani3estimate_STANi3.INDA18
    ## stani3estimate_STANi3.INDA34 <- result$stani3estimate_STANi3.INDA34
    ## stani3estimate_STANi3.INDA46 <- result$stani3estimate_STANi3.INDA46
    stani3estimate_STANi3.INDLABEL <- result$stani3estimate_STANi3.INDLABEL

    ## ## dataTableOutput
    ##       ## join industry lists
    ##     table.ind <- reactive({
    ##         if (input$tabs_stani3Estimate%in%c(5))
    ##         {
    table.ind <- data.frame(STANi3.INDA60All = STANi3.INDA60All)
    for (indlist in ui.stani3Estimate.ind.agg) {
        eval(parse(text = paste0('if ("', indlist, '"%in%stani3estimate_ind) table.ind <- merge(table.ind, data.frame(STANi3.INDA60All=STANi3.IND', indlist, ', STANi3.IND', indlist, '=STANi3.IND', indlist, '), all = TRUE)')))
    }

    ## if (stani3estimate_STANi3.INDA6==TRUE) table.ind <- merge(table.ind, data.frame(STANi3.INDA60All=STANi3.INDA6, STANi3.INDA6=STANi3.INDA6), all = TRUE)
    ## if (stani3estimate_STANi3.INDA18==TRUE) table.ind <- merge(table.ind, data.frame(STANi3.INDA60All=STANi3.INDA18, STANi3.INDA18=STANi3.INDA18), all = TRUE)
    ## if (stani3estimate_STANi3.INDA34==TRUE) table.ind <- merge(table.ind, data.frame(STANi3.INDA60All=STANi3.INDA34, STANi3.INDA34=STANi3.INDA34), all = TRUE)
    ## ## if (input$stani3estimate_STANi3.INDA60==TRUE) table.ind <- merge(table.ind, data.frame(STANi3.INDA60All=STANi3.INDA60, STANi3.INDA60=STANi3.INDA60), all = TRUE)
    ## if (stani3estimate_STANi3.INDA46==TRUE) table.ind <- merge(table.ind, data.frame(STANi3.INDA60All=STANi3.INDA60All, STANi3.INDA46=STANi3.INDA60All), all = TRUE)
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
            op <- par(oma=c(0,0,1,0))      # Room for the title and legend
            ## create empty canvas with final dimensions - add different series using loop over selected sources
            plot(nameyear, rep(NA, length(nameyear)),
                 ylim=c(ymin, ymax),
                 xlab='', ylab='')
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
            op <- par(usr = c(0,1,0,1),       # Reset the coordinates (above plot)
                      xpd = NA)     # Allow plotting outside the plot region
            legend(0, 1.1,          # legend position
                   legend = VIS.SOU$label[VIS.SOU$sou%in%namesou], # legend item labels
                   cex = 0.7,               # legend label font size
                   col = VIS.SOU$col[VIS.SOU$sou%in%namesou], # legend item fill colour
                   lty = VIS.SOU$lty[VIS.SOU$sou%in%namesou],
                   pch = VIS.SOU$pch[VIS.SOU$sou%in%namesou],
                   box.col = NA,            # invisible box around legend
                   horiz = TRUE)            # legend orientation
        }
    }

}}

polycharts_stani3Estimate <- function(result = .stani3Estimate()) {
    if (length(result) > 0) {

}}
## polycharts_stani3Estimate(result = isolate(.stani3Estimate()))

highcharts_stani3Estimate <- function(result = .stani3Estimate()) {
    if (length(result) > 0) {

}}
## highcharts_stani3Estimate(result = isolate(.stani3Estimate()))

nvd3charts_stani3Estimate <- function(result = .stani3Estimate()) {
    if (length(result) > 0) {

}}
## nvd3charts_stani3Estimate(result = isolate(.stani3Estimate()))

morrischarts_stani3Estimate <- function(result = .stani3Estimate()) {
    if (length(result) > 0) {

}}
## morrischarts_stani3Estimate(result = isolate(.stani3Estimate()))

maps_stani3Estimate <- function(result = .stani3Estimate()){
    if (length(result) > 0) {

}}

download_stani3Estimate <- function(result = .stani3Estimate(), zipfile = fname) {
    if (length(result) > 0) {

        exportcou <- result$exportcou
        namevar <- union(result$namevar, result$stani3estimate_morevar) # namevar <- "VALU"
        ## nameind
        nameyear <- result$nameyear
        ##
        stani3estimate_extend <- result$stani3estimate_extend
        stani3estimate_detail <- result$stani3estimate_detail


        load("data/data_init/stani3Estimate.rda")

        ## ## testing
        ## exportcou <- c("IDN")
        ## exportcou <- c("BRN", "IDN", "KHM", "MYS", "PHL", "SGP", "THA", "VNM")
        ## namevar <- c("VALU", "PROD")
        ## nameyear <- c(1994:2012)
        ## stani3estimate_extend <- FALSE
        ## stani3estimate_detail <- TRUE

        ## ## namecou <- unique(STAN.COU[["OECD"]], STAN.COU[["KPC"]])
        ## namecou <- STAN.COU[["KPC"]]

        ## nameind <- c("CTOTAL", "C15T37", "C80T93", "C99") # "C65T99"
        ## nameind <- c(nameind, "C65T99")
        ## nameind <- unique(c(nameind, as.character(STANi3.INDA6), as.character(STANi3.INDA18), as.character(STANi3.INDA34)))
        ## nameind <- factor(nameind, levels = STANi3.INDA60All)
        ## nameind <- as.character(nameind[order(nameind)])
        ##
        ## ui.stani3Estimate.ind <- STANi3.INDA60All
        nameind <- as.character(ui.stani3Estimate.ind)
        ## "C65T99"%in%nameind
        ## "C65T99"%in%ui.stani3Estimate.ind
        ## "C65T99"%in%STANi3.INDA60All

        nameest <- NULL
        if (stani3estimate_extend==TRUE) nameest <- c(nameest, "EXT")
        if (stani3estimate_detail==TRUE) nameest <- c(nameest, "DET")

        est.array <- ui.stani3Estimate.est.array[exportcou, namevar, nameind, nameest,,drop = FALSE]
        ##
        est.array.m <- melt(est.array, id.vars = c("cou", "var", "ind", "est"), variable.name = "sou")
        est.array.m$ind <- factor(est.array.m$ind, levels = STANi3.INDA60All)
        est.array.d <- dcast(est.array.m, cou + var + ind + est ~ sou, value.var = "value")
        ## subset data to array selection
        data.all <- stani3Estimate.data.all
        ## "C65T99"%in%data.all$ind

        x <- match(unique(est.array.m$ind), names(STANi3.HIERARCHYINV))
        x <- x[!is.na(x)]
        ind.parent <- unique(unlist(STANi3.HIERARCHYINV[x]))

        ## print(as.character(unique(est.array.m$cou)))

        data.all <- data.all[data.all$cou%in%as.character(unique(est.array.m$cou)) &
                             data.all$var%in%unique(est.array.m$var) &
                             data.all$ind%in%union(unique(est.array.m$ind), ind.parent) &
                             data.all$year%in%nameyear &
                             data.all$sou%in%unique(unlist(strsplit(as.character(est.array.m$value), split = ", "))),]

        ## print(unique(data.all$cou))

        res.ext <- NULL
        if (stani3estimate_extend==TRUE) {
            data.ext <- data.all
            print(est.array.d[est.array.d$est=="EXT",])
            res.ext <- estimate(data=data.ext,
                                sources=est.array.d[est.array.d$est=="EXT",],
                                period=nameyear)
            ## add back rows from main source
            sou.main.ext <- melt(ui.stani3Estimate.est.array[exportcou, namevar, nameind, "EXT", "MAIN"])
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
            res.det <- estimate(data=data.det,
                                sources=est.array.d[est.array.d$est=="DET",],
                                period=nameyear)
            res.det <- rbind(res.det, res.ext)
        }
        data.est <- rbind(res.det, res.ext)
        ## res.det <- rbind(res.det, res.ext)
        ## data.est <- NULL
        ## data.est <- rbind(data.est, res.det)

        data.est <- data.est[!duplicated(data.est[,!colnames(data.est)%in%c("sou", "value")]),]
        data.est$ind <- factor(data.est$ind, levels = nameind)
        data.est <- data.est[order(data.est$ind),]

        data <- data.est
        data <- dcast(data, cou + var + year ~ ind, value.var = "value")
        nameagg <- "CTOTAL"
        while(length(nameagg) > 0) {
            parts.all <- NULL
            for (agg in nameagg) {
                parts <- as.character(STANi3.HIERARCHY[[agg]])
                if (length(parts) > 0) {
                    ## cat(paste0(agg, ': ', toString(parts), '\n'))
                    temp <- data[, colnames(data) %in% parts]
                    ## if ("C99"%in%parts & !"C99"%in%colnames(data)) temp[,"C99"] <- 0
                    ## if ("C95"%in%parts & !"C95"%in%colnames(data)) temp[,"C95"] <- 0
                    ##
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
                    }
                    parts.all <- c(parts.all, parts)
                }
            }
            nameagg <- parts.all
        }

        data.adj <- melt(data, id.vars = c("cou", "var", "year"), variable.name = "ind")
        ## data.out <- data.adj
        ## data <- data.out
        data <- data.adj

        ## h(data)
        ## View(dcast(data, cou + var + ind ~ year, value.var = "value"))
        ## View(dcast(data.est, cou + var + ind ~ year, value.var = "value"))
        ## content = function(file){

        tempdir = tempdir()
        unlink(paste0(tempdir, list.files(tempdir)))
        file.remove(file.path(tempdir, list.files(tempdir)))

        ## print(nameind)
        ## cou <- namecou # temporary
        for (cou in unique(data$cou)) {
            ##
            ## cat(paste0(cou, '\n'))
            data.cou <- data[data$cou==cou,]
            ##
            ## path.out <- file.path("//ASAP5", "STI", "Data", "STAN", "STAN07", "PUB", "WORK", "XLS_VBAoutput")
            ## if (!file.exists(file.path(path.out, paste0('STAN_', cou)))) {
            ##     dir.create(file.path(path.out, paste0('STAN_', cou)))
            ## }
            ## file = file.path(path.out, paste0('STAN_', cou), paste0('STAN_', cou, '.xls'))
            file <- file.path(tempdir, paste0('STAN_', cou, '.xls'))

            ## wb <- loadWorkbook(filename = file , create = TRUE)
            wb <- loadWorkbook(filename = file , create = TRUE)
            ##
            for (var in unique(data.cou$var)) {
                data.var <- dcast(data.cou[data.cou$var==var,], ind ~ year, value.var = "value")
                ## add missing industries
                data.var <- merge(data.var, data.frame(ind = nameind), all = TRUE)
                data.var$ind <- factor(data.var$ind, levels = nameind)
                data.var <- data.var[order(data.var$ind),]
                ##
                names(data.var)[2:length(names(data.var))] <- paste0('_',names(data.var)[2:length(names(data.var))])
                createSheet(object = wb, name = as.character(var))
                setMissingValue(wb, value = "...")
                writeWorksheet(wb, data = data.var, sheet = as.character(var), startRow=1, startCol=1, header=TRUE)
                createFreezePane(wb, sheet = as.character(var), colSplit = 2, rowSplit = 2, leftColumn = 2, topRow = 2)
            }
            saveWorkbook(wb)
            ## }
        }

        zip(zipfile = zipfile, files = tempdir, extras = "-j")

}}


    ## ## subset data to download selection
    ## data.out <- reactive({
    ##     if (stani3estimate_allcou==TRUE) namecou <- sort(unique(data.all$cou)) else namecou <- stani3estimate_cou
    ##     if (stani3estimate_allvar==TRUE) namevar <- sort(unique(data.all$var)) else namevar <- c(stani3estimate_var, stani3estimate_var2)
    ##     if (stani3estimate_allind==TRUE) nameind <- sort(unique(data.all$ind)) else nameind <- union(nameind(), stani3estimate_ind2)
    ##     data.all[data.all$cou%in%namecou &
    ##              data.all$var%in%namevar &
    ##              data.all$ind%in%nameind & # nameind
    ##              data.all$year%in%nameyear() & # c(stani3estimate_time[1]:stani3estimate_time[2]) &
    ##              data.all$sou%in%namesou(), ] # namesou() reactive function
    ## })
    ##
    ## output$downloadData <- downloadHandler(
    ##     filename = function() {
    ##         if (stani3estimate_allcou==TRUE) namecou <- "ALLCOU" else namecou <- stani3estimate_cou
    ##         if (stani3estimate_allvar==TRUE) namevar <- "ALLVAR" else namevar <- c(stani3estimate_var, stani3estimate_var2)
    ##         if (stani3estimate_allind==TRUE) nameind <- "ALLIND" else nameind <- toString(union(nameind(), stani3estimate_ind2))
    ##         if (tabs_stani3Estimate==4) nameind <- sub("STANi3.", "", stani3estimate_indlist)
    ##
    ##         paste0(namecou, '_', namevar, '_', nameind, '.csv')
    ##     },
    ##     content = function(file) {
    ##         data.out <- data.out()
    ##         data.table.pivot <- data.table.pivot()
    ##         if (tabs_stani3Estimate==4)
    ##         {
    ##             data.out <- data.table.pivot
    ##         }
    ##         write.csv(data.out, file)
    ##     }
    ##     )
    ##
    ## output$downloadChart <- downloadHandler(
    ##     filename = function() {
    ##         if (stani3estimate_allcou==TRUE) namecou <- "ALLCOU" else namecou <- stani3estimate_cou
    ##         if (stani3estimate_allvar==TRUE) namevar <- "ALLVAR" else namevar <- c(stani3estimate_var, stani3estimate_var2)
    ##         if (stani3estimate_allind==TRUE) nameind <- "ALLIND" else nameind <- toString(union(nameind(), stani3estimate_ind2))
    ##         paste0(namecou, '_', namevar, '_', nameind, '.pdf')
    ##     },
    ##     content = function(file) {
    ##         ## ##########################
    ##         ## namecou <- c('AUT','BEL')
    ##         ## namevar <- c('VALU','PROD')
    ##         ## nameind <- c('CTOTAL','C01T05')
    ##         ## nameyear <- c(1995:2009)
    ##         ## data <- data.all[data.all$cou%in%namecou & data.all$var%in%namevar & data.all$ind%in%nameind,]
    ##         ## file <- tempfile(fileext=".pdf")
    ##         ##
    ##         ## cou <- namecou[1]
    ##         ## var <- namevar[1]
    ##         ## ind <- nameind[1]
    ##         ##
    ##         ## data.ind
    ##         ## ###########################
    ##         data <- data.out()
    ##
    ##         namevar <- unique(data$var)
    ##         namecou <- unique(data$cou)
    ##         nameind <- unique(data$ind)
    ##         nameyear <- nameyear()
    ##
    ##         data <- data[data$year%in%nameyear,]
    ##
    ##         pdf(file = file, width=16, height=10)
    ##         for (var in namevar)
    ##         {
    ##             data.var <- data[data$var==var,]
    ##             for (cou in namecou)
    ##             {
    ##                 data.cou <- data.var[data.var$cou==cou,]
    ##                 par(oma=c(0,0,2,0))
    ##                 if (stani3estimate_allind==TRUE) par(mfrow=c(11,10), mar=c(0.1,0.1,0,0), xaxt="n", yaxt="n")
    ##                 for (ind in nameind)
    ##                 {
    ##                     data.ind <- data.cou[data.cou$ind==ind,]
    ##                     if (nrow(data.ind) > 0)
    ##                     {
    ##                         ymin <- min(data.ind$value)
    ##                         ymax <- max(data.ind$value)
    ##                         plot(nameyear, rep(NA, length(nameyear)), ylim=c(ymin, ymax), xlab='', ylab='', main=paste('\n',ind))
    ##                         if (stani3estimate_STAN==TRUE)
    ##                         {
    ##                             lines(intersect(data.ind[data.ind$sou=="STANandBTD",]$year, nameyear),
    ##                                   data.ind$value[data.ind$year%in%intersect(data.ind$year, nameyear) & data.ind$sou=="STANandBTD"],
    ##                                   col='darkred', type='l', lty=2)
    ##                         }
    ##                         if (stani3estimate_OECDSUT112013==TRUE)
    ##                         {
    ##                             lines(intersect(data.ind[data.ind$sou=="OECDSUT112013",]$year, nameyear),
    ##                                   data.ind$value[data.ind$year%in%intersect(data.ind$year, nameyear) & data.ind$sou=="OECDSUT112013"],
    ##                                   col='darkblue', type='l')
    ##                         }
    ##                         if (stani3estimate_UNSDSNA2013==TRUE)
    ##                         {
    ##                             lines(intersect(data.ind[data.ind$sou=="UNSDSNA2013",]$year, nameyear),
    ##                                   data.ind$value[data.ind$year%in%intersect(data.ind$year, nameyear) & data.ind$sou=="UNSDSNA2013"],
    ##                                   col='darkorange', type='l', lty=3)
    ##                         }
    ##                         if (stani3estimate_UNDATA203CON==TRUE)
    ##                         {
    ##                             lines(intersect(data.ind[data.ind$sou=="UNDATA203CON",]$year, nameyear),
    ##                                   data.ind$value[data.ind$year%in%intersect(data.ind$year, nameyear) & data.ind$sou=="UNDATA203CON"],
    ##                                   col='darkorange', type='l', lty=3)
    ##                         }
    ##                         if (stani3estimate_UNDATA203100==TRUE)
    ##                         {
    ##                             lines(intersect(data.ind[data.ind$sou=="UNDATA203100",]$year, nameyear),
    ##                                   data.ind$value[data.ind$year%in%intersect(data.ind$year, nameyear) & data.ind$sou=="UNDATA203100"],
    ##                                   col='darkorange', type='l', lty=3)
    ##                         }
    ##                         ## if (stani3estimate_UNDATA203150==TRUE)
    ##                         ## {
    ##                         ##     lines(intersect(data.ind[data.ind$sou=="UNDATA203150",]$year, nameyear),
    ##                         ##           data.ind$value[data.ind$year%in%intersect(data.ind$year, nameyear) & data.ind$sou=="UNDATA203150"],
    ##                         ##           col='darkorange', type='l', lty=3)
    ##                         ## }
    ##                         if (stani3estimate_UNDATA203200==TRUE)
    ##                         {
    ##                             lines(intersect(data.ind[data.ind$sou=="UNDATA203200",]$year, nameyear),
    ##                                   data.ind$value[data.ind$year%in%intersect(data.ind$year, nameyear) & data.ind$sou=="UNDATA203200"],
    ##                                   col='darkorange', type='l', lty=3)
    ##                         }
    ##                         if (stani3estimate_UNDATA203300==TRUE)
    ##                         {
    ##                             lines(intersect(data.ind[data.ind$sou=="UNDATA203300",]$year, nameyear),
    ##                                   data.ind$value[data.ind$year%in%intersect(data.ind$year, nameyear) & data.ind$sou=="UNDATA203300"],
    ##                                   col='darkorange', type='l', lty=3)
    ##                         }
    ##                         if (stani3estimate_UNDATA203400==TRUE)
    ##                         {
    ##                             lines(intersect(data.ind[data.ind$sou=="UNDATA203400",]$year, nameyear),
    ##                                   data.ind$value[data.ind$year%in%intersect(data.ind$year, nameyear) & data.ind$sou=="UNDATA203400"],
    ##                                   col='darkorange', type='l', lty=3)
    ##                         }
    ##                         if (stani3estimate_UNDATA203500==TRUE)
    ##                         {
    ##                             lines(intersect(data.ind[data.ind$sou=="UNDATA203500",]$year, nameyear),
    ##                                   data.ind$value[data.ind$year%in%intersect(data.ind$year, nameyear) & data.ind$sou=="UNDATA203500"],
    ##                                   col='darkorange', type='l', lty=3)
    ##                         }
    ##                         if (stani3estimate_WIOT042012==TRUE)
    ##                         {
    ##                             lines(intersect(data.ind[data.ind$sou=="WIOT042012",]$year, nameyear),
    ##                                   data.ind$value[data.ind$year%in%intersect(data.ind$year, nameyear) & data.ind$sou=="WIOT042012"],
    ##                                   col='darkgreen', type='l', lty=4)
    ##                         }
    ##                         if (stani3estimate_WIOT112013==TRUE)
    ##                         {
    ##                             lines(intersect(data.ind[data.ind$sou=="WIOT112013",]$year, nameyear),
    ##                                   data.ind$value[data.ind$year%in%intersect(data.ind$year, nameyear) & data.ind$sou=="WIOT112013"],
    ##                                   col='darkgreen', type='l', lty=4)
    ##                         }
    ##                         if (stani3estimate_INDSTAT32==TRUE)
    ##                         {
    ##                             lines(intersect(data.ind[data.ind$sou=="INDSTAT32",]$year, nameyear),
    ##                                   data.ind$value[data.ind$year%in%intersect(data.ind$year, nameyear) & data.ind$sou=="INDSTAT32"],
    ##                                   col='magenta', type='l', lty=5)
    ##                         }
    ##                         if (stani3estimate_NSONAPATCH==TRUE)
    ##                         {
    ##                             lines(intersect(data.ind[data.ind$sou=="NSONAPATCH",]$year, nameyear),
    ##                                   data.ind$value[data.ind$year%in%intersect(data.ind$year, nameyear) & data.ind$sou=="NSONAPATCH"],
    ##                                   col='black', type='l', lty=5)
    ##                         }
    ##                         if (stani3estimate_EUNAIOR1==TRUE)
    ##                         {
    ##                             lines(intersect(data.ind[data.ind$sou=="EUNAIOR1",]$year, nameyear),
    ##                                   data.ind$value[data.ind$year%in%intersect(data.ind$year, nameyear) & data.ind$sou=="EUNAIOR1"],
    ##                                   col='black', type='l', lty=5)
    ##                         }
    ##                         if (stani3estimate_ICIO052013==TRUE)
    ##                         {
    ##                             lines(intersect(data.ind[data.ind$sou=="ICIO052013",]$year, nameyear),
    ##                                   data.ind$value[data.ind$year%in%intersect(data.ind$year, nameyear) & data.ind$sou=="ICIO052013"],
    ##                                   col='black', type='p', pch=19)
    ##                         }
    ##                     }
    ##                 }
    ##                 title(main=paste(cou, var),outer=T)
    ##             }
    ##         }
    ##         dev.off()
    ##     }
    ##     )

