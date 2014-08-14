#######################################
## Shiny interface for data functions
#######################################

data(stanDim)
dat <- isolate(values[["LFSi4"]])

ui.lfsShare.label.ocu <- rbind.data.frame(
    ## ISCO: LFSEU, LFSILO
    c("OC1", "Managers"),
    c("OC2", "Professionals"),
    c("OC3", "Technicians and associate professionals"),
    c("OC4", "Clerical support workers"),
    c("OC5", "Service and sales workers"),
    c("OC6", "Skilled agricultural, forestry and fishery workers"),
    c("OC7", "Craft and related trades workers"),
    c("OC8", "Plant and machine operators, and assemblers"),
    c("OC9", "Elementary occupations"),
    c("OC0", "Armed forces occupations"),
    ## AUS
    c("ANZSCO1", "Managers"),
    c("ANZSCO2", "Professionals"),
    c("ANZSCO3", "Technicians and Trades Workers"),
    c("ANZSCO4", "Community and Personal Service Workers"),
    c("ANZSCO5", "Clerical and Administrative Workers"),
    c("ANZSCO6", "Sales Workers"),
    c("ANZSCO7", "Machinery Operators and Drivers"),
    c("ANZSCO8", "Labourers"),
    ## CAN
    c("NOCSCANA", "Management Occupations"),
    c("NOCSCANB", "Business, Finance and Administrative Occupations"),
    c("NOCSCANC", "Natural and Applied Sciences and Related Occupations"),
    c("NOCSCAND", "Health Occupations"),
    c("NOCSCANE", "Occupations in Social Science, Education, Government Service and Religion"),
    c("NOCSCANF", "Occupations in Art, Culture, Recreation and Sport"),
    c("NOCSCANG", "Sales and Service Occupations"),
    c("NOCSCANH", "Trades, Transport and Equipment Operators and Related Occupations"),
    c("NOCSCANI", "Occupations Unique to Primary Industry"),
    c("NOCSCANJ", "Occupations Unique to Processing, Manufacturing and Utilities"),
    ## USA
    c("SOCUSA01", "Management, business, and financial occupations"),
    c("SOCUSA02", "Professional and related occupations"),
    c("SOCUSA03", "Service occupations"),
    c("SOCUSA04", "Sales and related occupations"),
    c("SOCUSA05", "Office and administrative support occupations"),
    c("SOCUSA06", "Farming, fishing, and forestry occupations"),
    c("SOCUSA07", "Construction and extraction occupations"),
    c("SOCUSA08", "Installation, maintenance, and repair occupations"),
    c("SOCUSA09", "Production occupations"),
    c("SOCUSA10", "Transportation and material moving occupations")
    ## ,c("SOCUSA11", "Armed Forces")
    )
names(ui.lfsShare.label.ocu) <- c("ocu", "label.ocu")
## ## tables for tools/help/lfsShare.md
## names(ui.lfsShare.label.ocu) <- c("Code", "Label")
## require(knitr)

## class.ocu <- rbind.data.frame(
##     c("EULFS, ILOSTAT", "OC"),
##     c("Australia", "ANZSCO"),
##     c("Canada", "NOCS"),
##     c("United States", "SOC")
##     )

## for (class in class.ocu[,2])
## {
##     table <- kable(ui.lfsShare.label.ocu[substr(ui.lfsShare.label.ocu[,1], 1, nchar(class))==class,], format = "markdown", row.names = FALSE, output = FALSE)
##     cat(paste0('\n', as.character(class.ocu[match(class, class.ocu[,2]), 1]), '\n\n'))
##     cat(table, sep = '\n')
## }


## require(dplyr)
## ## table.init <- data.frame(STANi4.INDALL = STANi4.INDALL)
## ## ## All lists
## ## table.code <- table.init
## ## table.code <- merge(table.code, data.frame(STANi4.INDALL=STANi4.INDA10, STANi4.INDA10=STANi4.INDA10), all = TRUE)
## ## table.code <- merge(table.code, data.frame(STANi4.INDALL=STANi4.INDA21, STANi4.INDA21=STANi4.INDA21), all = TRUE)
## ## table.code <- merge(table.code, data.frame(STANi4.INDALL=STANi4.INDA38, STANi4.INDA38=STANi4.INDA38), all = TRUE)
## ## table.code <- merge(table.code, data.frame(STANi4.INDALL=STANi4.INDA64, STANi4.INDA64=STANi4.INDA64), all = TRUE)
## ## table.code <- merge(table.code, data.frame(STANi4.INDALL=STANi4.INDA88, STANi4.INDA88=STANi4.INDA88), all = TRUE)names(table.code) <- sub("STANi4.IND", "", names(table.code))
## ## ##
## ## for (colname in names(table.code)[2:length(names(table.code))]) {
## ##     eval(parse(text = paste0('table.code <- mutate(table.code, ', colname, ' = ifelse(is.na(table.code[[colname]]), "", as.character(table.code[[colname]])))')))
## ## kable(table.code, format = "markdown", output = TRUE, row.names = FALSE)
## ## }
## ## All labels
## table.init <- data.frame(STANi4.INDA88 = STANi4.INDA88)
## table.label <- table.init
## table.label <- merge(table.label, data.frame(STANi4.INDA88=STANi4.INDLABEL$ind, STANi4.INDLABEL=STANi4.INDLABEL$label), all = FALSE)
## table.label$STANi4.INDA88 <- factor(table.label$STANi4.INDA88, levels = STANi4.INDA88)
## table.label <- table.label[order(table.label$STANi4.INDA88),]
## names(table.label) <- sub("STANi4.IND", "", names(table.label))
## names(table.label) <- sub("A88", "Code", names(table.label))
## names(table.label) <- sub("LABEL", "Label", names(table.label))
## ## cut long labels
## cut <- 100
## table.label <- mutate(table.label, LABEL = ifelse(nchar(as.character(LABEL)) > cut, paste0(substr(LABEL, 1, cut), '...'), as.character(LABEL)))
## kable(table.label, format = "markdown", output = TRUE, row.names = FALSE)


ui.lfsShare.xaxis <- c("sou", "cou", "var", "ind", "year")

ui.lfsShare.sou <- c("LFSNSO", "LFSEU", "LFSILO")

ui.lfsShare.cou.agg <- c("OECD")

ui.lfsShare.var <- list("EMPN: Total engaged" = "EMPN",
                        "EMPE: Employees" = "EMPE",
                        "SELF: Self-employed, Family worker" = "SELF")

ui.lfsShare.ind.agg <- c("A10", "A21", "A38", "A64", "A88")
ui.lfsShare.ind <- STANi4.INDALL

output$uiLs_namesou <- renderUI({
    if (input$lfsShare_xaxis=="sou") {
        selectInput("lfsShare_namesou", "Source:", ui.lfsShare.sou, selected = ui.lfsShare.sou, multiple = TRUE)
    } else {
        selectInput("lfsShare_namesou", "Source:", ui.lfsShare.sou, selected = ui.lfsShare.sou[1], multiple = FALSE)
    }
})

output$uiLs_namecou <- renderUI({

    ui.lfsShare.cou <- sort(as.character(unique(dat$cou[dat$sou%in%input$lfsShare_namesou])))

    if (input$lfsShare_xaxis=="cou") {
        selectInput("lfsShare_namecou", "Country:", c(ui.lfsShare.cou, ui.lfsShare.cou.agg), selected = "OECD", multiple = TRUE)
    } else {
        selectInput("lfsShare_namecou", "Country:", ui.lfsShare.cou, selected = ui.lfsShare.cou[1], multiple = FALSE)
    }
})

output$uiLs_namevar <- renderUI({

    ## ui.lfsShare.var <- sort(as.character(unique(dat$var[dat$sou%in%input$lfsShare_namesou &
    ##                                                     dat$cou%in%input$lfsShare_namecou])))

    if (input$lfsShare_xaxis=="var") {
        selectInput("lfsShare_namevar", "Variable:", ui.lfsShare.var, selected = ui.lfsShare.var, multiple = TRUE)
    } else {
        selectInput("lfsShare_namevar", "Variable:", ui.lfsShare.var, selected = "EMPN", multiple = FALSE)
    }
})

output$uiLs_nameind <- renderUI({

    ## ui.lfsShare.ind <- sort(unique(dat$ind[dat$sou%in%input$lfsShare_namesou &
    ##                                   dat$cou%in%input$lfsShare_namecou &
    ##                                   dat$var%in%input$lfsShare_namevar]))
    ## ui.lfsShare.ind <- union(ui.lfsShare.ind.agg, ui.lfsShare.ind)

    if (input$lfsShare_xaxis=="ind") {
        selectInput("lfsShare_nameind", "Industries:", c(ui.lfsShare.ind.agg, STANi4.INDALL), selected = "A10", multiple = TRUE)
    } else {
        selectInput("lfsShare_nameind", "Industries:", ui.lfsShare.ind, selected = "DTOTAL", multiple = FALSE)
    }
})

output$uiLs_nameyear <- renderUI({

    namecou <- input$lfsShare_namecou
    if ("OECD"%in%namecou) namecou <- union(namecou[!namecou%in%"OECD"], STAN.COU)

    nameind <- input$lfsShare_nameind
    for (indlist in ui.lfsShare.ind.agg) {
        eval(parse(text = paste0('if ("', indlist, '"%in%nameind) nameind <- union(nameind[!nameind%in%"', indlist, '"], STANi4.IND', indlist, ')')))
    }

    ui.lfsShare.year <- unique(dat$year[dat$sou%in%input$lfsShare_namesou &
                                 dat$cou%in%namecou &
                                 dat$var%in%input$lfsShare_namevar &
                                 dat$ind%in%nameind])
    ui.lfsShare.year <- rev(sort(ui.lfsShare.year))

    if (input$lfsShare_xaxis=="year") {
        selectInput("lfsShare_nameyear", "Year:", ui.lfsShare.year, selected = head(ui.lfsShare.year, 3), multiple = TRUE)
    } else {
        selectInput("lfsShare_nameyear", "Year:", ui.lfsShare.year, selected = ui.lfsShare.year[1], multiple = FALSE)
    }
    ## if (input$lfsShare_xaxis=="year") {
    ##     selectInput("lfsShare_nameyear", "Year:", as.list(c(2012:1970)), selected = 2011, multiple = TRUE)
    ## } else {
    ##     selectInput("lfsShare_nameyear", "Year:", as.list(c(2012:1970)), selected = 2011, multiple = FALSE)
    ## }
})

## ########################
## lfsShare ui script
## ########################

output$ui_lfsShare <- renderUI({

    ## doLogin()
    ## if (loginData$LoggedIn) {

        list(
            conditionalPanel(condition = "input.tabs_lfsShare=='NVD3Charts'",
                             wellPanel(
                                 checkboxInput("lfsShare_viz_plot_controls", "Plot options", FALSE),
                                 conditionalPanel(condition = "input.lfsShare_viz_plot_controls==true",
                                                  ## htmlOutput("ui_plot_options"),
                                                  sliderInput(inputId = "lfsShare_viz_plot_height", label = "Height:", min = 400, max = 1000, value = 600, step = 50),
                                                  sliderInput(inputId = "lfsShare_viz_plot_width", label = "Width:", min = 400, max = 1200, value = 800, step = 50),
                                                  checkboxInput("lfsShare_xrotate", "Rotate x-axis labels", FALSE)

                                                  )
                                 )
                             ),
            wellPanel(
                checkboxInput("lfsShare_calcshare", "Calculate share in total", TRUE),
                numericInput("lfsShare_rounddec", "Round to number of decimals:", 2),
                checkboxInput("lfsShare_labelocu", "Show occupation labels", FALSE)
                ),
            selectInput("lfsShare_xaxis", "x-Axis variable:", ui.lfsShare.xaxis, selected = "ind", multiple = FALSE),
            uiOutput("uiLs_namesou"),
            uiOutput("uiLs_namecou"),
            uiOutput("uiLs_namevar"),
            uiOutput("uiLs_nameind"),
            uiOutput("uiLs_nameyear"),
            ##
            ## actionButton("lfsShare_recalcButton", "Recalculate with selection"),
            ##
            ## wellPanel(
            ##     htmlOutput("select_occ_high_UI")
            ##     ),
            ## ),
            helpAndReport("LFS Share","lfsShare",inclMD("tools/help/lfsShare.md"))
            ) # list(...

    ## } else
    ## {
    ##     h3("Please log in")
    ## }

})
output$lfsShare <- renderUI({
    ## for input-output
    statTabPanel(menu_name = "SKILL", # menu_name: for side bar - coincide with navbarMenu
                 fun_name = "LFS Share",   # fun_name
                 rfun_label = ".lfsShare", # rfun_label
                 fun_label = "lfsShare" # fun_label
                 ## ,rChart_lib = input$stanindic_rchartlib
                 ,fun_tabs = c("Tables", "NVD3Charts")
                 )
})


## ######################
## Input test
## ######################

## require(reshape2)
## require(rCharts)
## input <- list(
##     lfsShare_xaxis="cou",
##     ## lfsShare_namesou=c("LFSNSO", "LFSEU", "LFSILO"),
##     lfsShare_namesou=c("LFSNSO"),
##     lfsShare_namecou=c("OECD"),
##     lfsShare_namevar="EMPN",
##     lfsShare_nameind=c("DTOTAL"),
##     lfsShare_nameyear=c(2011),
##     lfsShare_rounddec=2,
##     lfsShare_labelocu=FALSE,
##     lfsShare_recalcButton=0,
##     lfsShare_calcshare=TRUE,
##     lfsShare_xrotate=FALSE,
##     lfsShare_viz_plot_height=600,
##     lfsShare_viz_plot_width=800
##     )
## lfsShare_xaxis <- input$lfsShare_xaxis
## lfsShare_namesou <- input$lfsShare_namesou
## lfsShare_namecou <- input$lfsShare_namecou
## lfsShare_namevar <- input$lfsShare_namevar
## lfsShare_nameind <- input$lfsShare_nameind
## lfsShare_nameyear <- input$lfsShare_nameyear
## lfsShare_rounddec <- input$lfsShare_rounddec
## lfsShare_labelocu <- input$lfsShare_labelocu
## lfsShare_recalcButton <- input$lfsShare_recalcButton
## lfsShare_calcshare <- input$lfsShare_calcshare
## lfsShare_xrotate <- input$lfsShare_xrotate
## lfsShare_viz_plot_height <- input$lfsShare_viz_plot_height
## lfsShare_viz_plot_width <- input$lfsShare_viz_plot_width

.lfsShare <- reactive({
    ## reactive that calls the function for main analysis
    ## . used to indicate this is an 'internal' function
    ##
    ## if (length(input$stanindic_dimS)==0) return ()
    ##
    lfsShare(
        lfsShare_xaxis = input$lfsShare_xaxis,
        lfsShare_namesou = input$lfsShare_namesou,
        lfsShare_namecou = input$lfsShare_namecou,
        lfsShare_namevar = input$lfsShare_namevar,
        lfsShare_nameind = input$lfsShare_nameind,
        lfsShare_nameyear = input$lfsShare_nameyear,
        lfsShare_rounddec = input$lfsShare_rounddec,
        lfsShare_labelocu = input$lfsShare_labelocu,
        lfsShare_recalcButton = input$lfsShare_recalcButton,
        lfsShare_calcshare = input$lfsShare_calcshare,
        lfsShare_xrotate = input$lfsShare_xrotate,
        lfsShare_viz_plot_height = input$lfsShare_viz_plot_height,
        lfsShare_viz_plot_width = input$lfsShare_viz_plot_width
        )
})
## isolate(.lfsShare())
##
## observe({
##   if(is.null(input$lfsShareReport) || input$lfsShareReport==0) return()
##   isolate({
##     inp <- list(
##       input$datasets,
##
##       input$stanindic_indic,
##       ui.lfsShare.nameyear[as.numeric(input$stanindic_time)],
##       input$stanindic_demand,
##       ui.lfsShare.namesec.agg[as.numeric(input$stanindic_indX)],
##       ## indS = ui.lfsShare.namesec.agg[as.numeric(input$indS)],
##       names(ui.lfsShare.namereg.agg)[as.numeric(input$stanindic_couS)],
##       names(ui.lfsShare.namereg.agg)[as.numeric(input$stanindic_couX)],
##       names(ui.lfsShare.namereg.agg)[as.numeric(input$stanindic_couD)]
##       )
##
##     updateReport(inp,"lfsShare")
##   })
## })

lfsShare <- function(
    lfsShare_xaxis = lfsShare_xaxis,
    lfsShare_namesou = lfsShare_namesou,
    lfsShare_namecou = lfsShare_namecou,
    lfsShare_namevar = lfsShare_namevar,
    lfsShare_nameind = lfsShare_nameind,
    lfsShare_nameyear = lfsShare_nameyear,
    lfsShare_rounddec = lfsShare_rounddec,
    lfsShare_labelocu = lfsShare_labelocu,
    lfsShare_recalcButton = lfsShare_recalcButton,
    lfsShare_calcshare = lfsShare_calcshare,
    lfsShare_xrotate = lfsShare_xrotate,
    lfsShare_viz_plot_height = lfsShare_viz_plot_height,
    lfsShare_viz_plot_width = lfsShare_viz_plot_width
    )
{

    namecou <- lfsShare_namecou
  ## if(is.null(lfsShare_recalcButton) || lfsShare_recalcButton==0) {
  ##       isolate({ namecou <- lfsShare_namecou })
  ##   } else {
  ##       isolate({ namecou <- lfsShare_namecou })
  ##   } ## namecou
    if ("OECD"%in%namecou) namecou <- union(namecou[!namecou%in%"OECD"], STAN.COU)
    ## print(namecou)


    namesou <- lfsShare_namesou
    ## if ("LFSNSO"%in%namesou) {
    ##     namesou <- namesou[!namesou=="LFSNSO"]
    ##     ## cou <- namecou[1]
    ##     for (cou in namecou) {
    ##         namesou <- c(namesou, paste0("LFS", cou))
    ##     }
    ## }

    namevar <- lfsShare_namevar

    nameind <- lfsShare_nameind
    for (indlist in ui.lfsShare.ind.agg) {
        eval(parse(text = paste0('if ("', indlist, '"%in%nameind) nameind <- union(nameind[!nameind%in%"', indlist, '"], STANi4.IND', indlist, ')')))
    }

  nameyear <- lfsShare_nameyear

    dat <- values[["LFSi4"]]
    ## dat <- isolate(values[["LFSi4"]])

    data.calc.indic <- dat[dat$sou%in%namesou &
                           dat$cou%in%namecou &
                           dat$var%in%namevar &
                           dat$ind%in%nameind &
                           dat$year%in%nameyear,]

    if (lfsShare_calcshare==TRUE)
    {
        data.calc.indic.total <- aggregate(data.calc.indic$value, by = list(data.calc.indic[, lfsShare_xaxis]), FUN = "sum")
        names(data.calc.indic.total) <- c(lfsShare_xaxis, "value")
        data.calc.indic.merge <- merge(data.calc.indic, data.calc.indic.total, by = lfsShare_xaxis)
        attach(data.calc.indic.merge)
        data.calc.indic.merge$value <- value.x / value.y
        detach(data.calc.indic.merge)
        data.calc.indic <- subset(data.calc.indic.merge, select = names(data.calc.indic))
    }

    if (lfsShare_labelocu==TRUE)
    {
        data.calc.indic <- merge(data.calc.indic, ui.lfsShare.label.ocu)
        data.calc.indic <- subset(data.calc.indic, select = names(data.calc.indic)[names(data.calc.indic)!="ocu"])
        names(data.calc.indic) <- sub("label.ocu", "ocu", names(data.calc.indic))
    }

  ## conv.skillData <- list()
  ## conv.skillData$USA$ocu <- cbind.data.frame(ocu = as.character(sort(unique(DATA.LFSUSA$ocu))))
  ## write.csv(conv.skillData$USA$ocu, file = file.path(dbpath, "GitHub", "skillData", "inst", "LFSUSA.csv"), row.names = FALSE)
  ## conv.skillData

    return(list(data.calc.indic = data.calc.indic,
                lfsShare_xaxis = lfsShare_xaxis,
                lfsShare_namesou = lfsShare_namesou,
                lfsShare_namecou = lfsShare_namecou,
                lfsShare_namevar = lfsShare_namevar,
                lfsShare_nameind = lfsShare_nameind,
                lfsShare_nameyear = lfsShare_nameyear,
                lfsShare_rounddec = lfsShare_rounddec,
                lfsShare_xrotate = lfsShare_xrotate,
                lfsShare_viz_plot_height = lfsShare_viz_plot_height,
                lfsShare_viz_plot_width = lfsShare_viz_plot_width
                )
           )
}

summary_lfsShare <- function(result = .lfsShare())
{ if (length(result) > 0) {
}}

tables_lfsShare <- function(result = .lfsShare())
{ if (length(result) > 0) {
    ##
    data.calc.indic <- result$data.calc.indic
    lfsShare_xaxis <- result$lfsShare_xaxis
    lfsShare_rounddec <- result$lfsShare_rounddec

    ##
    dim.row <- c(setdiff(ui.lfsShare.xaxis, lfsShare_xaxis), "ocu")
    dim.row.string <- gsub(", ", " + ", toString(dim.row))
    eval(parse(text = paste0('data.table <- dcast(data.calc.indic, ', dim.row.string, ' ~ ', lfsShare_xaxis, ', value.var = "value")')))
    ##
    for (col in setdiff(names(data.table), dim.row))
    {
        data.table[, col] <- sprintf(paste0('%1.', lfsShare_rounddec, 'f'), data.table[, col])
    }
    data.table
    ##
}}

plots_lfsShare <- function(result = .lfsShare())
{ if (length(result) > 0) {
}}
polycharts_lfsShare <- function(result = .lfsShare())
{ if (length(result) > 0) {
}}
## polycharts_lfsShare(result = isolate(.lfsShare()))
highcharts_lfsShare <- function(result = .lfsShare())
{ if (length(result) > 0) {
}}
## highcharts_lfsShare(result = isolate(.lfsShare()))

nvd3charts_lfsShare <- function(result = .lfsShare())
{ if (length(result) > 0) {

    data.calc.indic <- result$data.calc.indic
    lfsShare_xaxis <- result$lfsShare_xaxis
    lfsShare_rounddec <- result$lfsShare_rounddec
    lfsShare_xrotate <- result$lfsShare_xrotate

    lfsShare_viz_plot_height <- result$lfsShare_viz_plot_height
    lfsShare_viz_plot_width <- result$lfsShare_viz_plot_width

    ## ########################################
    ## add zeroes to have a balanced panel -
    ## include in application instead of data
    ## ########################################

    ## namedim <- names(DATA.LFSEU)[!names(DATA.LFSEU)%in%c("value")]
    ## dim.all <- eval(parse(text = paste0('data.frame(', namedim[1], ' = unique(DATA.LFSEU$', namedim[1], '))')))
    ## for (dim in namedim[2:length(namedim)])
    ## {
    ##     dim.all <- eval(parse(text = paste0('merge(dim.all, data.frame(', dim, ' = unique(DATA.LFSEU$', dim, ')), all = TRUE)')))
    ## }
    ## dim.all$value <- 0
    ## DATA.LFSEU <- rbind(DATA.LFSEU, dim.all)
    ## DATA.LFSEU <- DATA.LFSEU[!duplicated(DATA.LFSEU[,!colnames(DATA.LFSEU)%in%c("value")]),]

    data.plot <- data.calc.indic[,!colnames(data.calc.indic)%in%setdiff(ui.lfsShare.xaxis, lfsShare_xaxis)]

    data.plot.expand <- merge(data.frame(ocu = unique(data.plot$ocu)),
                              data.frame(dim = unique(data.plot[, lfsShare_xaxis])),
                              all = TRUE)
    names(data.plot.expand) <- sub("dim", lfsShare_xaxis, names(data.plot.expand))
    data.plot.expand$value <- 0
    data.plot <- rbind(data.plot, data.plot.expand)
    data.plot <- data.plot[!duplicated(data.plot[,!colnames(data.plot)=="value"]),]
    data.plot <- data.plot[order(data.plot[, lfsShare_xaxis],
                                             data.plot$ocu),]
    xticks <- unique(data.plot[, lfsShare_xaxis])
    if (lfsShare_xrotate==TRUE) xrotate = -90 else xrotate = 0
    ##
    ## eval(parse(text = paste0('n1 <- nPlot(', "value", ' ~ ', lfsShare_xaxis, ', group = "ocu", data = data.plot, type = "multiBarChart", height = ', lfsShare_viz_plot_height, ')')))
    eval(parse(text = paste0('n1 <- nPlot(', "value", ' ~ ', lfsShare_xaxis, ', group = "ocu", data = data.plot, type = "multiBarChart")')))
    n1$chart(reduceXTicks = FALSE)
    n1$chart(stacked = TRUE)
    ## n1$xAxis(ticks = levels(data.plot[, lfsShare_xaxis])) # , tickFormat = "#!function(x) { return (x).toFixed(0) }!#")
    n1$xAxis(ticks = xticks, rotateLabels = xrotate) # , tickFormat = "#!function(x) { return (x).toFixed(0) }!#")
    eval(parse(text = paste0('n1$yAxis(tickFormat = "#!function(x) { return (x).toFixed(', as.numeric(lfsShare_rounddec),') }!#")')))
    ## n1$set(height = lfsShare_viz_plot_height)
    ## n1$params$facet=lfsShare_facet
    ## n1$templates$script = system.file("libraries/nvd3/layouts/nvd3FacetPlot.html", package = "rCharts")
    n1$addParams(height = lfsShare_viz_plot_height,
                 width = lfsShare_viz_plot_width,
                 dom = "nvd3charts_lfsShare") # capital "Indic"
    ## n1

    return(n1)

}}
## nvd3charts_lfsShare(result = isolate(.lfsShare()))

morrischarts_lfsShare <- function(result = .lfsShare())
{ if (length(result) > 0) {

}}
## morrischarts_lfsShare(result = isolate(.lfsShare()))

maps_lfsShare <- function(result = .lfsShare())
{ if (length(result) > 0) {

}}
