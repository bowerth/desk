data(stanDim)

dat <- isolate(values[["STANNAi4"]])
##
ui.stanIndic.xrates <- dat$DATA.XRATES
names(ui.stanIndic.xrates) <- sub("var", "cur", names(ui.stanIndic.xrates))

ui.stanIndic.namevar.btd <- unique(dat$DATA.BTD$var)
ui.stanIndic.namevar.stan <- unique(dat$DATA.STAN$var)
ui.stanIndic.namevar.anberd <- as.factor(unique(dat$DATA.ANBERD$var))

ui.stanIndic.group <- list(
  "cou: by Country" = "cou",
  "ind: by Industry" = "ind"
  )

ui.stanIndic.indic <- list(
  "VSHT: Value added share relative to total economy" = "VSHT",
  ## "custom: Custom formula" = "custom",
  "VSHM: Value added shares relative to manufacturing" = "VSHM",
  "ESHT: Employment shares in total economy" = "ESHT",
  "ESHM: Employment shares in total manufacturing" = "ESHM",
  "LBNT: Labour compensation per workforce in total economy" = "LBNT",
  "LBNM: Labour compensation per workforce in manufacturing" = "LBNM",
  "LBET: Labour compensation per employee in total economy" = "LBET",
  "LBEM: Labour compensation per employee in manufacturing" = "LBEM",
  "LBVA: Labour share of value added in total economy" = "LBVA",
  "IPYE: Labour productivity index" = "IPYE",
  "IPYH: Labour productivity index" = "IPYH",
  "IULC: Unit labor cost index" = "IULC",
  "LULC: Unit labor cost level" = "LULC",
  "AVHW: Average hours worked" = "AVHW",
  "VAPR: Value added share of production" = "VAPR",
  "INPR: Intermediate consumption share of production" = "INPR",
  "INVV: Investment intensity based on value added" = "INVV",
  "INVT: Investment shares relative to total economy" = "INVT",
  "INVM: Investment shares relative to total manufacturing" = "INVM",
  "RDST: Distribution of R&D expenditures across all activities" = "RDST",
  "RDSM: Distribution of R&D expenditures across manufacturing activities" = "RDSM",
  "RDIV: R&D intensity using value added" = "RDIV",
  "RDIP: R&D intensity using production" = "RDIP",
  "CMTB: Contribution to manufacturing trade balance" = "CMTB",
  "EXIM: Export import ratio" = "EXIM",
  "TBAL: Trade balance - in US dollars: exchange rate as SP.EXCH" = "TBAL",
  "XSHT: Composition of total exports of goods" = "XSHT",
  "XSHM: Composition of manufacturing exports of goods" = "XSHM",
  "MSHT: Composition of total imports of goods" = "MSHT",
  "MSHM: Composition of manufacturing imports of goods" = "MSHM",
  "XSHP: Export share of production" = "XSHP",
  "MPEN: Import penetration" = "MPEN"
  )

ui.stanIndic.formula.indic.init <- rbind.data.frame(
  c("VSHT", "VALU / VALU_DTOTAL"),
  c("VSHM", "VALU / VALU_D10T33"),
  c("ESHT", "EMPN / EMPN_DTOTAL"),
  c("ESHM", "EMPN / EMPN_D10T33"),
  c("LBNT", "LABR / LABR_DTOTAL"),
  c("LBNM", "LABR / LABR_D10T33"),
  c("LBET", "LABR / EMPE_DTOTAL"),
  c("LBEM", "LABR / EMPE_D10T33"),
  c("LBVA", "LABR / VALU"),
  c("IPYE", "VALK / EMPN / (VALK_2005 / EMPN_2005)"),
  c("IPYH", "VALK / HRSN / (VALK_2005 / HRSN_2005)"),
  c("IULC", "EMPN / EMPE * LABR / VALK / (EMPN_2005 / EMPE_2005 * LABR_2005 / VALK_2005)"),
  c("LULC", "EMPN / EMPE * LABR / VALK"),
  c("AVHW", "HRSN / EMPN"),
  c("VAPR", "VALU / PROD"),
  c("INPR", "INTI / PROD"),
  c("INVV", "GFCF / VALU"),
  c("INVT", "GFCF / GFCF_DTOTAL"),
  c("INVM", "GFCF / GFCF_D10T33"),
  c("RDST", "RDNC / RDNC_DTOTAL"),
  c("RDSM", "RDNC / RDNC_D10T33"),
  c("RDIV", "RDNC / VALU"),
  c("RDIP", "RDNC / PROD"),
  c("CMTB", "((EXPO - IMPO) - (EXPO_D10T33 - IMPO_D10T33) * (EXPO + IMPO) / (EXPO_D10T33 + IMPO_D10T33)) / (EXPO_D10T33 + IMPO_D10T33)"),
  c("EXIM", "EXPO / IMPO"),
  c("TBAL", "EXPO - IMPO"),
  c("XSHT", "EXPO / EXPO_DTOTAL"),
  c("XSHM", "EXPO / EXPO_D10T33"),
  c("MSHT", "IMPO / IMPO_DTOTAL"),
  c("MSHM", "IMPO / IMPO_D10T33"),
  c("XSHP", "EXPO / PROD"),
  c("MPEN", "IMPO / (PROD - EXPO + IMPO)"))
names(ui.stanIndic.formula.indic.init) <- c("indic", "formula")

## function for table output and download to avoid repetition
pivottable_stanIndic <- function(data.calc,
                                 data.set,
                                 stanindic_showData,
                                 stanindic_showIndic,
                                 stanindic_pivotRow
                                 )
    {

        names(data.calc) <- sub("indic", "var", names(data.calc))
        data.calc <- subset(data.calc, select = names(data.set))

        data.table <- NULL
        if (stanindic_showData==TRUE) data.table <- rbind(data.table, data.set)
        if (stanindic_showIndic==TRUE) data.table <- rbind(data.table, data.calc)

        pivot.row <- stanindic_pivotRow
        string.pivot.row <- gsub(",", " +", toString(pivot.row))
        pivot.col <- setdiff(names(data.table), c(pivot.row, "value"))
        string.pivot.col <- gsub(",", " +", toString(pivot.col))

        if (length(pivot.col) > 0) {
            eval(parse(text = paste0('data.table <- dcast(data.table, ', string.pivot.row, ' ~ ', string.pivot.col, ', value.var = "value")')))
            eval(parse(text = paste0('data.table <- data.table[order(', toString(paste0('data.table$', pivot.row)), '),]')))
        }

        return(data.table)
    }

## ########################
## stanIndic ui script
## ########################

## ui.stanIndic.rchartlib <- c("highcharts", "polycharts", "nvd3", "morris")
## ui.stanIndic.charttype <- c("Bar", "Scatter", "Bubble", "Line")

output$uiSi_charttype <- renderUI({
    if (input$tabs_stanIndic=="PolyCharts") {
            ui.stanIndic.charttype <- c("Bar") # "Scatter"
            ui.stanIndic.charttype.selected <- "Bar"
        }
    if (input$tabs_stanIndic=="HighCharts") {
            ui.stanIndic.charttype <- c("Bubble") # "Bar", "Scatter"
            ui.stanIndic.charttype.selected <- "Bubble"
        }
    if (input$tabs_stanIndic=="NVD3Charts") {
            ui.stanIndic.charttype <- c("Bar") # "Scatter"
            ui.stanIndic.charttype.selected <- "Bar"
        }
    if (input$tabs_stanIndic=="MorrisCharts") {
            ui.stanIndic.charttype <- c("Bar", "Line", "Area")
            ui.stanIndic.charttype.selected <- "Line"
        }

    ## selectInput("stanindic_charttype", "Select chart type:", state_init_list("stanindic_charttype", "Bar", ui.stanIndic.charttype))
    selectInput("stanindic_charttype", "Select chart type:",
                choices = ui.stanIndic.charttype,
                selected = ui.stanIndic.charttype.selected)

})


output$uiSi_nameyear <- renderUI({
    if (input$tabs_stanIndic%in%c("PolyCharts", "HighCharts", "NVD3Charts", "MorrisCharts")) {
        if (input$stanindic_charttype%in%c("Bar", "Scatter", "Bubble")) {
            selectInput("stanindic_nameyear", "Year:", as.list(c(2012:1970)), selected = 2011, multiple = FALSE)
        } else {
            sliderInput("stanindic_nameyear",
                        "Years:",
                        value = c(2000,2012),
                        min = 1970,
                        max = 2012,
                        step = 1,
                        format="#")
        }
    } else {
        sliderInput("stanindic_nameyear",
                    "Years:",
                    value = c(2000,2012),
                    min = 1970,
                    max = 2012,
                    step = 1,
                    format="#")
    }
})


output$ui_stanIndic <- renderUI({

  ## doLogin()
  ## if (loginData$LoggedIn) {

    list(
        conditionalPanel(condition="input.tabs_stanIndic=='PolyCharts' | input.tabs_stanIndic=='HighCharts' | input.tabs_stanIndic=='NVD3Charts' | input.tabs_stanIndic=='MorrisCharts'",
                         uiOutput("uiSi_charttype")
                         ),
        wellPanel(
            helpText("Click '?' button below for indicator calculation formula"),
            selectInput("stanindic_indic", "Indicator:", ui.stanIndic.indic),
            conditionalPanel(condition="input.tabs_stanIndic=='PolyCharts' | input.stanindic_charttype=='Scatter' | input.stanindic_charttype=='Bubble'",
                             conditionalPanel(condition="input.stanindic_charttype=='Scatter' | input.stanindic_charttype=='Bubble'",
                                              selectInput("stanindic_indic2", "Indicator 2 (y-axis):", ui.stanIndic.indic, selected = "ESHT"),
                                              conditionalPanel(condition="input.stanindic_charttype=='Bubble'",
                                                               selectInput("stanindic_indic3", "Indicator 3 (size):", ui.stanIndic.indic, selected = "LBNT")
                                                               )
                                              ),
                             selectInput("stanindic_group", "Group by:", ui.stanIndic.group, selected = "cou")
                             )
            )
        ,
        ## conditionalPanel(condition="input.tabs_stanIndic=='Tables'",
        conditionalPanel(condition="input.tabs_stanIndic=='DataTables'",
                           checkboxInput("stanindic_showIndic", "Show indicator values", TRUE),
                           checkboxInput("stanindic_showData", "Show source data values", FALSE),
                           selectInput("stanindic_pivotRow", "ID variables and order of sorting",
                                       as.list(c("var", "cou", "ind", "year")),
                                       selected = c("var", "cou", "ind"), multiple = TRUE)
                           ),
          actionButton("stanindic_recalcButton", "Recalculate with selection"),
          selectInput("stanindic_namecou", "Country:", as.list(STAN.COU[["OECD"]]), selected = sample(STAN.COU[["OECD"]], 15), multiple = TRUE),
          selectInput("stanindic_nameind", "Industries:", c(list("A10"), as.list(STANi4.INDALL)), selected = "D10T33", multiple = TRUE),
          uiOutput("uiSi_nameyear"),
          selectInput("stanindic_cur", "Currency:",
                      list(
                          "Exchange Rate" = "EXCH",
                          "Purchasing Power Parities" = "PPPS", # test
                          "National Currency Units" = "NCU"
                          )
                      ),
          numericInput("stanindic_rounddec", "Round to number of decimals:", 4),
          ## conditionalPanel(condition="input.tabs_stanIndic=='Tables' | input.tabs_stanIndic=='download'",
          conditionalPanel(condition="input.tabs_stanIndic=='DataTables' | input.tabs_stanIndic=='download'",
                           helpText("Download comma-separated values:"),
                           downloadButton('download_stanIndic', 'Download CSV')
                           ),
          helpAndReport("STAN Indicators", "stanIndic", inclMD("tools/help/stanIndic.md"))
          ) # list(...

  ## } else
  ##   {
  ##     h3("Please log in")
  ##   }

})

stanIndic_widthSize <- reactive({
    ifelse(is.null(input$stanIndic_stanIndic_viz_plot_width), return(values$plotWidth), return(input$stanIndic_stanIndic_viz_plot_width))
})
stanIndic_heightSize <- reactive({
    ifelse(is.null(input$stanIndic_viz_plot_height), return(values$plotHeight), return(input$stanIndic_viz_plot_height))
})

output$stanIndic <- renderUI({
  statTabPanel(menu_name = "STAN",
               fun_name = "STAN Indicators",
               rfun_label = ".stanIndic",
               fun_label = "stanIndic",
               widthFun = "stanIndic_widthSize",
               heightFun = "stanIndic_heightSize",
               fun_tabs = c("DataTables", "PolyCharts", "HighCharts", "NVD3Charts") # "Tables"
               )
})


## ######################
## Input test
## ######################

## input <- list(
##     stanindic_indic = "VSHT",
##     stanindic_indic2 = "ESHT",
##     stanindic_indic3 = "LBNT",
##     stanindic_group = "cou",
##     stanindic_showIndic = TRUE,
##     stanindic_showData = FALSE,
##     stanindic_pivotRow = c("var", "cou", "ind"),
##     ## stanindic_pivotDataDownload = stanindic_pivotDataDownload
##     stanindic_recalcButton = 0,
##     stanindic_namecou = c("AUT", "DEU", "FRA", "CHL", "JPN"),
##     stanindic_nameind = c("D01T03", "D10T33"),
##     ## stanindic_nameyear = c(1970, 2012),
##     stanindic_nameyear = 2006,
##     stanindic_charttype = "Scatter",
##     stanindic_cur = "EXCH",
##     stanindic_rounddec = 4,
##     stanindic_downloadData = 0,
##     tabs_stanIndic = "PolyCharts",
##     stanIndic_viz_plot_height = 650,
##     stanIndic_viz_plot_width = 650
## )
## stanindic_indic = input$stanindic_indic
## stanindic_indic2 = input$stanindic_indic2
## stanindic_indic3 = input$stanindic_indic3
## stanindic_group = input$stanindic_group
## stanindic_showIndic = input$stanindic_showIndic
## stanindic_showData = input$stanindic_showData
## stanindic_pivotRow = input$stanindic_pivotRow
## ## stanindic_pivotDataDownload = input$stanindic_pivotDataDownload
## stanindic_recalcButton = input$stanindic_recalcButton
## stanindic_namecou = input$stanindic_namecou
## stanindic_nameind = input$stanindic_nameind
## stanindic_nameyear = input$stanindic_nameyear
## ## stanindic_year = input$stanindic_year
## stanindic_charttype = input$stanindic_charttype
## stanindic_cur = input$stanindic_cur
## stanindic_rounddec = input$stanindic_rounddec
## stanindic_downloadData = input$stanindic_downloadData
## stanindic_tabs_stanIndic = input$tabs_stanIndic
## stanIndic_viz_plot_height = input$stanIndic_viz_plot_height
## stanIndic_viz_plot_width = input$stanIndic_viz_plot_width


.stanIndic <- reactive({
    stanIndic(
        stanindic_indic = input$stanindic_indic,
        stanindic_indic2 = input$stanindic_indic2,
        stanindic_indic3 = input$stanindic_indic3,
        stanindic_group = input$stanindic_group,
        stanindic_showIndic = input$stanindic_showIndic,
        stanindic_showData = input$stanindic_showData,
        stanindic_pivotRow = input$stanindic_pivotRow,
        stanindic_recalcButton = input$stanindic_recalcButton,
        stanindic_namecou = input$stanindic_namecou,
        stanindic_nameind = input$stanindic_nameind,
        stanindic_nameyear = input$stanindic_nameyear,
        stanindic_cur = input$stanindic_cur,
        stanindic_rounddec = input$stanindic_rounddec,
        stanindic_downloadData = input$stanindic_downloadData,
        stanindic_charttype = input$stanindic_charttype,
        stanindic_tabs_stanIndic = input$tabs_stanIndic,
        stanIndic_viz_plot_height = input$stanIndic_viz_plot_height,
        stanIndic_viz_plot_width = input$stanIndic_viz_plot_width
        )
})
##
## observe({
##   if(is.null(input$stanIndicReport) || input$stanIndicReport == 0) return()
##   isolate({
##     inp <- list(
##       input$datasets,
##
##       input$stanindic_indic,
##       ui.stanIndic.year[as.numeric(input$stanindic_time)],
##       input$stanindic_demand,
##       ui.stanIndic.namesec.agg[as.numeric(input$stanindic_indX)],
##       ## indS = ui.stanIndic.namesec.agg[as.numeric(input$indS)],
##       names(ui.stanIndic.namereg.agg)[as.numeric(input$stanindic_couS)],
##       names(ui.stanIndic.namereg.agg)[as.numeric(input$stanindic_couX)],
##       names(ui.stanIndic.namereg.agg)[as.numeric(input$stanindic_couD)]
##       )
##
##     updateReport(inp,"stanIndic")
##   })
## })

stanIndic <- function(
    stanindic_indic = stanindic_indic,
    stanindic_indic2 = stanindic_indic2,
    stanindic_indic3 = stanindic_indic3,
    stanindic_group = stanindic_group,
    stanindic_showIndic = stanindic_showIndic,
    stanindic_showData = stanindic_showData,
    stanindic_pivotRow = stanindic_pivotRow,
    stanindic_recalcButton = stanindic_recalcButton,
    stanindic_namecou = stanindic_namecou,
    stanindic_nameind = stanindic_nameind,
    stanindic_nameyear = stanindic_nameyear,
    stanindic_cur = stanindic_cur,
    stanindic_rounddec = stanindic_rounddec,
    stanindic_downloadData = stanindic_downloadData,
    stanindic_charttype = stanindic_charttype,
    stanIndic_viz_plot_height = stanIndic_viz_plot_height,
    stanIndic_viz_plot_width = stanIndic_viz_plot_width,
    stanindic_tabs_stanIndic = stanindic_tabs_stanIndic
    )
{

  formula.indic <- ui.stanIndic.formula.indic.init
  ## namecou
  if(is.null(stanindic_recalcButton) || stanindic_recalcButton == 0) {
        isolate({
            namecou <- stanindic_namecou
        })
    } else {
        isolate({
            namecou <- stanindic_namecou
        })
    }
  ## nameindic
  nameindic <- stanindic_indic
  if (stanindic_tabs_stanIndic%in%c("PolyCharts", "HighCharts", "NVD3Charts")) {
      if (stanindic_charttype%in%c("Scatter", "Bubble")) nameindic <- union(nameindic, stanindic_indic2)
      if (stanindic_charttype%in%c("Bubble")) nameindic <- union(nameindic, stanindic_indic3)
    }
  ## namedim
  namedim <- NULL
  for (indic in nameindic) {
      dim.indic <- gsub(pattern = "[^a-zA-Z0-9]", replacement = " ", formula.indic$formula[formula.indic$indic==indic])
      dim.indic <- gsub(pattern = "[ ]+", replacement = " ", dim.indic)
      dim.indic <- unlist(strsplit(dim.indic, split = " "))
      namedim <- union(namedim, dim.indic)
    }
  ## string.formula
  string.formula <- NULL
  for (indic in nameindic) {
      formula <- formula.indic$formula[formula.indic$indic==indic]
      string.formula <- toString(paste(string.formula, formula))
    }
  ## nameind
  if(is.null(stanindic_recalcButton) || stanindic_recalcButton == 0) {
        isolate({
            nameind <- stanindic_nameind
        })
    } else {
        isolate({
            nameind <- stanindic_nameind
            if ("A10"%in%nameind) nameind <- union(nameind[!nameind%in%"A10"], STANi4.INDA10)
        })
    }
  ## namevar
  namevar <- namedim[namedim%in%STAN.VARALL]
  ## nameyear
  if (length(stanindic_nameyear) > 1) {
      nameyear <- c(stanindic_nameyear[1]:stanindic_nameyear[2])
  } else nameyear <- stanindic_nameyear
  ## denom.year
  denom.year <- suppressWarnings(as.numeric(namedim)[!is.na(as.numeric(namedim[nchar(namedim)==4]))])
  ## denom.ind
  denom.ind <- namedim[namedim%in%STANi4.INDALL]
  ## ######
  ## Data
  ## ######
  dat <- values[["STANNAi4"]]
  data.set <- NULL
  for (dataset in c("stan", "btd", "anberd")) {
    eval(parse(text = paste0(
                 'if (length(intersect(namevar, ui.stanIndic.namevar.', dataset, ')) > 0) {',
                 'data.temp <- dat$DATA.', toupper(dataset), '[dat$DATA.', toupper(dataset), '$cou%in%namecou & ',
                   'dat$DATA.', toupper(dataset), '$var%in%namevar & ',
                   'dat$DATA.', toupper(dataset), '$ind%in%union(nameind, denom.ind) & ',
                   'dat$DATA.', toupper(dataset), '$year%in%union(nameyear, denom.year),];',
                 'data.set <- rbind(data.set, data.temp)',
                 '}'
                 )))
  }
  ## apply exchange USD rates
  if (stanindic_cur!="NCU") {
      data.set.mon <- data.set[data.set$var%in%STAN.VARMON,]
      data.set.mon <- merge(data.set.mon, ui.stanIndic.xrates[ui.stanIndic.xrates$cur==stanindic_cur,], by = c("cou", "year"))
      data.set.mon$value <- data.set.mon$value.x / data.set.mon$value.y
      data.set.mon <- subset(data.set.mon, select = c("cou", "var", "ind", "year", "value"))
      data.set <- rbind(data.set[!data.set$var%in%STAN.VARMON,], data.set.mon)
    }
  ## denom
  denom <- list(denom.ind, denom.year)
  names(denom) <- c("ind", "year")
  ## data.denom.all
  data.denom.all <- NULL
  for (dim in names(denom)) {
      if (length(denom[[dim]]) > 0)
          {
              denom.member <- denom[[dim]]
              eval(parse(text = paste0('nom.member <- name', dim)))
              data.denom <- data.set[data.set[,dim]%in%denom.member,]
              data.denom$var <- paste0(data.denom$var, "_", denom.member)
              data.denom <- merge(data.denom, nom.member, all = TRUE)
              data.denom[,dim] <- data.denom$y
              data.denom <- data.denom[,!colnames(data.denom)=="y"]
              if (length(denom.member) > 0) data.denom.all <- rbind(data.denom.all, data.denom)
          }
  }
  if (length(data.denom.all) > 0) data.set <- rbind(data.set, data.denom.all)
  ## data.calc
  data.calc <- dcast(data.set, cou + ind + year ~ var, value.var = "value")
  attach(data.calc)
  for (indic in nameindic) {
      eval(parse(text = paste0('data.calc$', indic, ' <- ', formula.indic$formula[formula.indic$indic==indic])))
  }
  detach(data.calc)
  data.calc <- data.calc[,!sapply(strsplit(colnames(data.calc), "_"), "[[", 1)%in%namevar] # remove variables from data frame
  data.calc$ind <- factor(data.calc$ind, levels = STANi4.INDALL)
  data.calc <- melt(data.calc, id.vars = c("cou", "ind", "year"), variable.name = "indic", na.rm = TRUE)
  data.calc <- transform(data.calc, value = round(value, stanindic_rounddec))
  ## sort values for plotting
  data.calc.indic <- dcast(data.calc, cou + ind + year ~ indic, value.var = "value")
  ## sort data according to largest industry in selection
  ind.top <- data.calc.indic$ind[order(-data.calc.indic[,stanindic_indic])][1]
  data.calc.indic.ind.top <- data.calc.indic[data.calc.indic$ind==ind.top,]
  data.calc.indic$cou <- factor(data.calc.indic$cou, levels = unique(data.calc.indic.ind.top$cou[order(-data.calc.indic.ind.top[,stanindic_indic])])) # added: 'unique' if more than one year
  data.calc.indic <- data.calc.indic[order(data.calc.indic$cou),]

  return(list(namecou = namecou,
              nameindic = nameindic,
              namedim = namedim,
              string.formula = string.formula,
              nameind = nameind,
              namevar = namevar,
              nameyear = nameyear,
              denom.year = denom.year,
              denom.ind = denom.ind,
              data.set = data.set,
              data.calc = data.calc,
              data.calc.indic = data.calc.indic,
              stanindic_charttype = stanindic_charttype,
              stanindic_indic = stanindic_indic,
              stanindic_indic2 = stanindic_indic2,
              stanindic_indic3 = stanindic_indic3,
              stanindic_rounddec = stanindic_rounddec,
              stanindic_group = stanindic_group,
              stanindic_showData = stanindic_showData,
              stanindic_showIndic = stanindic_showIndic,
              stanindic_pivotRow = stanindic_pivotRow,
              stanIndic_viz_plot_height = stanIndic_viz_plot_height,
              stanIndic_viz_plot_width = stanIndic_viz_plot_width
              )
         )
}

summary_stanIndic <- function(result = .stanIndic())
{ if (length(result) > 0) {

  string.formula <-  result$string.formula
  string.formula

}}

datatables_stanIndic <- function(result = .stanIndic())
{ if (length(result) > 0) {

  data.table <- pivottable_stanIndic(result$data.calc,
                                     result$data.set,
                                     result$stanindic_showData,
                                     result$stanindic_showIndic,
                                     result$stanindic_pivotRow
                                     )

  return(data.table)

}}

polycharts_stanIndic <- function(result = .stanIndic())
{ if (length(result) > 0) {

    data.calc.indic <- result$data.calc.indic
    string.formula <-  result$string.formula
    namecou <- result$namecou
    stanindic_charttype <- result$stanindic_charttype
    stanindic_indic <- result$stanindic_indic
    stanindic_indic2 <- result$stanindic_indic2
    stanindic_indic3 <- result$stanindic_indic3
    stanindic_rounddec <- result$stanindic_rounddec
    stanindic_group <- result$stanindic_group
    stanindic_group <- result$stanindic_group
    stanIndic_viz_plot_height <- result$stanIndic_viz_plot_height
    stanIndic_viz_plot_width <- result$stanIndic_viz_plot_width

  if (stanindic_charttype == "Bar") {

      xValue <- stanindic_group
      facet <- setdiff(c("cou", "ind"), xValue) # complementary

      p1 <- rPlot(x = list(var = xValue, sort = stanindic_indic),
                  y = stanindic_indic,
                  facet = facet,
                  color = xValue,
                  data = data.calc.indic,
                  type = 'bar')
      p1$guides(x = list(title = "", ticks = levels(data.calc.indic[[xValue]])))
      ## print(p1)

  } else if (stanindic_charttype == "Scatter") {
      color <- setdiff(c("ind", "cou"), stanindic_group)
      p1 <- rPlot(VSHT ~ ESHT, data = data.calc.indic, color = color, type = "point")
      p1$facet(var = stanindic_group, type = "wrap", cols = 2)
  }

  p1$addParams(height = 500,
               ## height = stanIndic_viz_plot_height,
               ## width = stanIndic_viz_plot_width,
               ## dom = "polycharts_stanIndic", # capial "Indic", see radiant.R
               title = string.formula)
  return(p1)

}}

highcharts_stanIndic <- function(result = .stanIndic())
{ if (length(result) > 0) {

  data.calc.indic <- result$data.calc.indic
  string.formula <-  result$string.formula
  namecou <- result$namecou
  stanindic_charttype <- result$stanindic_charttype
  stanindic_indic <- result$stanindic_indic
  stanindic_indic2 <- result$stanindic_indic2
  stanindic_indic3 <- result$stanindic_indic3
  stanindic_rounddec <- result$stanindic_rounddec
  stanindic_group <- result$stanindic_group
  stanIndic_viz_plot_height <- result$stanIndic_viz_plot_height
  stanIndic_viz_plot_width <- result$stanIndic_viz_plot_width

  if (stanindic_charttype == "Bar") {
      eval(parse(text = paste0('h1 <- hPlot(', stanindic_indic, ' ~ cou, group = "ind", data = data.calc.indic, type = "column", title = string.formula)')))
      h1$xAxis(categories = levels(data.calc.indic$cou), title = list(text = ""))
  } else if (stanindic_charttype == "Scatter") {
      eval(parse(text = paste0('h1 <- hPlot(', stanindic_indic2, ' ~ ', stanindic_indic, ', group = "', stanindic_group,'", data = data.calc.indic, type = "scatter")')))
  } else if (stanindic_charttype == "Bubble") {
      eval(parse(text = paste0('h1 <- hPlot(', stanindic_indic2, ' ~ ', stanindic_indic, ', group = "', stanindic_group,'", size = "', stanindic_indic3, '", data = data.calc.indic, type = "bubble")')))
  }
  h1$addParams(height = 500,
               ## height = stanIndic_viz_plot_height, # doesn't update
               ## width = stanIndic_viz_plot_width, # doesn't update
               dom = "highcharts_stanIndic") # capital "Indic"
  return(h1)

}}

nvd3charts_stanIndic <- function(result = .stanIndic())
{ if (length(result) > 0) {

  data.calc.indic <- result$data.calc.indic
  string.formula <-  result$string.formula
  namecou <- result$namecou
  stanindic_charttype <- result$stanindic_charttype
  stanindic_indic <- result$stanindic_indic
  stanindic_indic2 <- result$stanindic_indic2
  stanindic_indic3 <- result$stanindic_indic3
  stanindic_rounddec <- result$stanindic_rounddec
  stanindic_group <- result$stanindic_group
  stanIndic_viz_plot_height <- result$stanIndic_viz_plot_height
  stanIndic_viz_plot_width <- result$stanIndic_viz_plot_width

  if (stanindic_charttype == "Bar") {
      eval(parse(text = paste0('n1 <- nPlot(', stanindic_indic, ' ~ cou, group = "ind", data = data.calc.indic, type = "multiBarChart")')))
      n1$chart(reduceXTicks = FALSE)
      n1$xAxis(ticks = levels(data.calc.indic$cou))
      eval(parse(text = paste0('n1$yAxis(tickFormat = "#!function(x) { return (x).toFixed(', as.numeric(stanindic_rounddec),') }!#")')))

  } else if (stanindic_charttype == "Scatter") {
      eval(parse(text = paste0('n1 <- nPlot(', stanindic_indic2, ' ~ ', stanindic_indic, ', group = "cou", data = data.calc.indic, type = "scatterChart")')))
      n1$xAxis(axisLabel = stanindic_indic)
      n1$yAxis(axisLabel = stanindic_indic2)
  }
  n1$addParams(height = 500,
               ## height = stanIndic_viz_plot_height,
               ## width = stanIndic_viz_plot_width, # doesn't update
               dom = "nvd3charts_stanIndic") # capital "Indic"
  return(n1)

}}

morrischarts_stanIndic <- function(result = .stanIndic())
{ if (length(result) > 0) {

  data.calc.indic <- result$data.calc.indic
  string.formula <-  result$string.formula
  namecou <- result$namecou
  stanindic_charttype <- result$stanindic_charttype
  stanindic_indic <- result$stanindic_indic
  stanindic_indic2 <- result$stanindic_indic2
  stanindic_indic3 <- result$stanindic_indic3
  stanindic_rounddec <- result$stanindic_rounddec
  stanindic_group <- result$stanindic_group


  if (stanindic_charttype == "Bar")
  {
      eval(parse(text = paste0('m1 <- mPlot(', stanindic_indic, ' ~ cou, group = "ind", data = data.calc.indic, type = "Bar")'))) # , labels = unique(data.calc.indic$ind))')))
  } else if (stanindic_charttype%in%c("Line", "Area"))
  {
      eval(parse(text = paste0('data.calc.morris <- transform(data.calc.indic, year = as.character(year), ', stanindic_indic, ' = round(', stanindic_indic, ', stanindic_rounddec))')))
      data.calc.morris.d <- dcast(data.calc.morris, year ~ cou + ind, value.var = stanindic_indic)
      m1 <- mPlot(x = "year", y = names(data.calc.morris.d)[names(data.calc.morris.d)!="year"], type = "Line", data = data.calc.morris.d)
      m1$set(pointSize = 0, lineWidth = 1
             ## ,xLabelFormat = "#! function (x) { return x.toString(); } !#"
             )
      if (stanindic_charttype=="Area")
      {
          m1$set(type = 'Area')
      }
  }
  m1$addParams(height = 500,
               dom = "morrischarts_stanIndic")
  return(m1)

}}

## maps_stanIndic <- function(result = .stanIndic())
## { if (length(result) > 0) {

##   return()

## }}

download_stanIndic <- function(result = .stanIndic(), zipfile = fname)
{ if (length(result) > 0) {

    data.table <- pivottable_stanIndic(result$data.calc,
                                       result$data.set,
                                       result$stanindic_showData,
                                       result$stanindic_showIndic,
                                       result$stanindic_pivotRow
                                       )

    nameindic <- result$nameindic

    tempdir = tempdir()
    unlink(paste0(tempdir, list.files(tempdir)))
    file.remove(file.path(tempdir, list.files(tempdir)))

    file <- file.path(tempdir, paste0('stanIndic_', nameindic, '.csv'))

    write.csv(data.table, file, row.names = FALSE, na = "")

    zip(zipfile = zipfile, files = tempdir, extras = "-j")

}}

