data(stanDim)

## testing
## require(devtools)
## require(ggplot2)
## load_all(file.path(dbpath, "GitHub", "ggthemes"))
## devtools::document(file.path(dbpath, "GitHub", "ggthemes"))
## devtools::install(file.path(dbpath, "GitHub", "ggthemes"))

## ########

dat <- isolate(values[["STANNAi4"]])
##
ui.stanIndic.namevar.btd <- unique(dat$DATA.BTDi4$var)
ui.stanIndic.namevar.stan <- unique(dat$DATA.STANi4$var)
ui.stanIndic.namevar.anberd <- as.factor(unique(dat$DATA.ANBERDi4$var))

dat <- isolate(values[["STANNAi0"]])

ui.stanIndic.xrates <- dat$DATA.XRATES
## ui.stanIndic.xrates <- dat$DATACONVCUR[["UNSDMA"]]
names(ui.stanIndic.xrates) <- sub("var", "cur", names(ui.stanIndic.xrates))



ui.stanIndic.col <- isolate(values$colors)
## ############################################
## load themes and colors from ggthemes package
## ############################################
## add theme_oecdpac, theme_oecdeco
## require(ggthemes)
funname_all <- ls(as.environment("package:ggthemes"))
funname_theme <- funname_all[substr(funname_all, 1, nchar("theme_"))=="theme_"]
ui.stanIndic.ggtheme <- sub("theme_", "", funname_theme)
funname_scale <- funname_all[substr(funname_all, 1, nchar("scale_color_"))=="scale_color_"]
ui.stanIndic.ggcolor <- sub("scale_color_", "", funname_scale)

## file <- tempfile(fileext = ".txt")
## filecon <- file(file)
## theme_all_str <- NULL
## for (theme in funname_theme) {
##     themename <- paste0('theme_', theme)
##     ## themename <- "theme_economist"
##     if (exists(themename)) {
##         theme_all_str <- paste(theme_all_str, themename, sep = '\n')

##         themestring <- eval(parse(text = themename))

##         theme_all_str <- paste(theme_all_str, themestring, sep = '\n')
##         ## print(themename)
##         ## print(eval(parse(text = themename)))
##     }
## }


ui.stanIndic.group <- list(
  "cou: by Country" = "cou",
  "ind: by Industry" = "ind"
  )

ui.stanIndic.indic <- list(
    ## "Select indicator" = "",
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
ui.stanIndic.indic <- unname(unlist(ui.stanIndic.indic))


ui.stanIndic.formula.indic.init <- rbind.data.frame(
    c("VSHT", "PC", "VALU / VALU_DTOTAL"),
    c("VSHM", "PC", "VALU / VALU_D10T33"),
    c("ESHT", "PC", "EMPN / EMPN_DTOTAL"),
    c("ESHM", "PC", "EMPN / EMPN_D10T33"),
    c("LBNT", "PC", "LABR / LABR_DTOTAL"),
    c("LBNM", "PC", "LABR / LABR_D10T33"),
    c("LBET", "PC", "LABR / EMPE_DTOTAL"),
    c("LBEM", "PC", "LABR / EMPE_D10T33"),
    c("LBVA", "RATIO", "LABR / VALU"),
    c("IPYE", "IDX", "VALK / EMPN / (VALK_2005 / EMPN_2005)"),
    c("IPYH", "IDX", "VALK / HRSN / (VALK_2005 / HRSN_2005)"),
    c("IULC", "IDX", "EMPN / EMPE * LABR / VALK / (EMPN_2005 / EMPE_2005 * LABR_2005 / VALK_2005)"),
    c("LULC", "RATIO", "EMPN / EMPE * LABR / VALK"),
    c("AVHW", "RATIO", "HRSN / EMPN"),
    c("VAPR", "RATIO", "VALU / PROD"),
    c("INPR", "RATIO", "INTI / PROD"),
    c("INVV", "RATIO", "GFCF / VALU"),
    c("INVT", "PC", "GFCF / GFCF_DTOTAL"),
    c("INVM", "PC", "GFCF / GFCF_D10T33"),
    c("RDST", "PC", "RDNC / RDNC_DTOTAL"),
    c("RDSM", "PC", "RDNC / RDNC_D10T33"),
    c("RDIV", "RATIO", "RDNC / VALU"),
    c("RDIP", "RATIO", "RDNC / PROD"),
    ## IITR missing
    c("CMTB", "PC", "((EXPO - IMPO) - (EXPO_D10T33 - IMPO_D10T33) * (EXPO + IMPO) / (EXPO_D10T33 + IMPO_D10T33)) / (EXPO_D10T33 + IMPO_D10T33)"),
    c("EXIM", "RATIO", "EXPO / IMPO"),
    c("TBAL", "USD", "EXPO - IMPO"),
    c("XSHT", "PC", "EXPO / EXPO_DTOTAL"),
    c("XSHM", "PC", "EXPO / EXPO_D10T33"),
    c("MSHT", "PC", "IMPO / IMPO_DTOTAL"),
    c("MSHM", "PC", "IMPO / IMPO_D10T33"),
    c("XSHP", "RATIO", "EXPO / PROD"),
    c("MPEN", "RATIO", "IMPO / (PROD - EXPO + IMPO)"))
names(ui.stanIndic.formula.indic.init) <- c("indic", "type", "formula")

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
## ui.stanIndic.plottype <- c("Bar", "Scatter", "Bubble", "Line")

output$uiSi_plottype <- renderUI({
    if (input$tabs_stanIndic=="Plots") {
            ui.stanIndic.plottype <- c("MultiBar", "Scatter", "Line") # "Scatter"
            ui.stanIndic.plottype.selected <- "Scatter"
        }
    if (input$tabs_stanIndic=="PolyCharts") {
            ui.stanIndic.plottype <- c("Bar") # "Scatter"
            ui.stanIndic.plottype.selected <- "Bar"
        }
    if (input$tabs_stanIndic=="HighCharts") {
            ui.stanIndic.plottype <- c("Bubble") # "Bar", "Scatter"
            ui.stanIndic.plottype.selected <- "Bubble"
        }
    if (input$tabs_stanIndic=="NVD3Charts") {
            ui.stanIndic.plottype <- c("Bar") # "Scatter"
            ui.stanIndic.plottype.selected <- "Bar"
        }
    if (input$tabs_stanIndic=="MorrisCharts") {
            ui.stanIndic.plottype <- c("Bar", "Line", "Area")
            ui.stanIndic.plottype.selected <- "Line"
        }
    else
        ui.stanIndic.plottype <- ui.stanIndic.plottype.selected <- "Scatter"

    ## selectInput("stanindic_plottype", "Select chart type:", state_init_list("stanindic_plottype", "Bar", ui.stanIndic.plottype))
    selectInput("stanindic_plottype", "Select chart type:",
                choices = ui.stanIndic.plottype,
                selected = ui.stanIndic.plottype.selected)

})


output$uiSi_nameyear <- renderUI({
    if (input$tabs_stanIndic%in%c("Plots", "PolyCharts", "HighCharts", "NVD3Charts", "MorrisCharts")) {
        if (input$stanindic_plottype%in%c("Bar", "MultiBar", "Scatter", "Bubble")) {
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

output$uiSi_indic <- renderUI({

    if (input$stanindic_plottype%in%c("MultiBar")) {
        list(
            selectInput("stanindic_indic", "Select bar indicator(s):", choices = ui.stanIndic.indic, selected = "VSHT", multiple = TRUE),
            selectInput("stanindic_indic2", "Select point indicator(s):", choices = ui.stanIndic.indic, selected = "ESHT", multiple = TRUE)
            )
    } else if (input$stanindic_plottype%in%c("Scatter")) {
        list(
            selectInput("stanindic_indic", "Select x-axis indicator:", choices = ui.stanIndic.indic, selected = "VSHT", multiple = FALSE),
            selectInput("stanindic_indic2", "Select y-axis indicator:", choices = ui.stanIndic.indic, selected = "ESHT", multiple = FALSE)
            )
    } else if (input$stanindic_plottype%in%c("Line")) {
        selectInput("stanindic_indic", "Select indicator:", choices = ui.stanIndic.indic, selected = "VSHT", multiple = FALSE)
    }

})

output$ui_stanIndic <- renderUI({

  ## doLogin()
  ## if (loginData$LoggedIn) {

    list(
        conditionalPanel(condition = "input.tabs_stanIndic!='DataTables'",
                         wellPanel(
                             checkboxInput("stanIndic_viz_plot_controls", "Plot options", FALSE),
                             conditionalPanel(condition = "input.stanIndic_viz_plot_controls==true",
                                              sliderInput(inputId = "stanIndic_viz_plot_height", label = "Height:", min = 400, max = 1000,
                                                          value = 400, step = 50)
                                              ,
                                              sliderInput(inputId = "stanIndic_viz_plot_width", label = "Width:", min = 400, max = 1200,
                                                          value = 800, step = 50)
                                              ## ,
                                              ## checkboxInput("stanindic_xrotate", "Rotate x-axis labels", FALSE)
                                              ,
                                              conditionalPanel(condition = "input.tabs_stanIndic=='Plots'",
                                                               selectInput("stanindic_ggtheme", "Select plot theme", ui.stanIndic.ggtheme, selected = "oecdcpi")
                                                               ,
                                                               selectInput("stanindic_ggcolor", "Select color scheme", ui.stanIndic.ggcolor, selected = "oecdcpi")
                                                               )
                                              )
                             )
                         ),

        ## conditionalPanel(condition="input.tabs_stanIndic=='Plots' | input.tabs_stanIndic=='PolyCharts' | input.tabs_stanIndic=='HighCharts' | input.tabs_stanIndic=='NVD3Charts' | input.tabs_stanIndic=='MorrisCharts'",
                         uiOutput("uiSi_plottype")
                         ## )
       ,
        wellPanel(
            helpText("Click '?' button below for indicator calculation formula"),
            uiOutput("uiSi_indic"),
            ## selectInput("stanindic_indic", "Select bar indicator(s):", choices = ui.stanIndic.indic, selected = "VSHT", multiple = TRUE),
            ## selectInput("stanindic_indic2", "Select point indicator(s):", choices = ui.stanIndic.indic, selected = "LBNT", multiple = TRUE),
                             selectInput("stanindic_group", "Group by:", ui.stanIndic.group,
                                         ## selected = "cou"
                                         selected = "ind"
                                         )
                             ,
                             conditionalPanel(condition="input.tabs_stanIndic=='Plots'",
                                              checkboxInput("stanindic_facethoriz", "Facet horizontally", FALSE),
                                              checkboxInput("stanindic_stack", "Stacked values", FALSE)
                                              )
                             )
            ## )
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
          selectInput("stanindic_namecou", "Country:", as.list(STAN.COU[["OECD"]]), selected = sample(STAN.COU[["OECD"]], 10), multiple = TRUE),
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
    ifelse(is.null(input$stanIndic_viz_plot_width), return(values$plotWidth), return(input$stanIndic_viz_plot_width))
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
               ## fun_tabs = c("DataTables", "Plots", "HighCharts", "NVD3Charts", "PolyCharts") # "Tables"
               fun_tabs = c("DataTables", "Plots") # , "PolyCharts")
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
##     stanindic_plottype = "Scatter",
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
## stanindic_plottype = input$stanindic_plottype
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
        stanindic_plottype = input$stanindic_plottype,
        stanindic_tabs_stanIndic = input$tabs_stanIndic,
        stanindic_facethoriz = input$stanindic_facethoriz,
        stanindic_stack = input$stanindic_stack,
        stanindic_ggtheme = input$stanindic_ggtheme,
        stanindic_ggcolor = input$stanindic_ggcolor,
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
    stanindic_plottype = stanindic_plottype,
    stanindic_tabs_stanIndic = stanindic_tabs_stanIndic,
    stanindic_facethoriz = stanindic_facethoriz,
    stanindic_stack = stanindic_stack,
    stanindic_ggtheme = stanindic_ggtheme,
    stanindic_ggcolor = stanindic_ggcolor,
    stanIndic_viz_plot_height = stanIndic_viz_plot_height,
    stanIndic_viz_plot_width = stanIndic_viz_plot_width
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
  ## if (stanindic_tabs_stanIndic%in%c("Plots", "PolyCharts", "HighCharts", "NVD3Charts")) {
      ## if (stanindic_plottype%in%c("MultiBar", "Scatter", "Bubble")) nameindic <- union(nameindic, stanindic_indic2)
  nameindic <- union(nameindic, stanindic_indic2)
  ##     if (stanindic_plottype%in%c("Bubble")) nameindic <- union(nameindic, stanindic_indic3)
  ##   }
  ## namedim
  namedim <- NULL
  for (indic in nameindic) {
      dim.indic <- gsub(pattern = "[^a-zA-Z0-9]", replacement = " ", formula.indic$formula[formula.indic$indic==indic])
      dim.indic <- gsub(pattern = "[ ]+", replacement = " ", dim.indic)
      dim.indic <- unlist(strsplit(dim.indic, split = " "))
      namedim <- union(namedim, dim.indic)
    }
  ## string.formula
  ## string.formula <- NULL
  ## for (indic in nameindic) {
  ##     formula <- formula.indic$formula[formula.indic$indic==indic]
  ##     string.formula <- toString(paste(string.formula, formula))
  ##   }

  string.formula <- toString(paste0(nameindic, ' = ', formula.indic$formula[formula.indic$indic%in%nameindic], collapse = '; '))

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
  ## ###############
  ## Data for output
  ## ###############
  ## in flat format for pivot table
  ## remove variables from data frame
  data.calc <- data.calc[,!sapply(strsplit(colnames(data.calc), "_"), "[[", 1)%in%namevar]
  ## remove industries from data frame
  data.calc <- data.calc[data.calc$ind%in%nameind,]
  ## remove years from data frame
  data.calc <- data.calc[data.calc$year%in%nameyear,]

  data.calc$ind <- factor(data.calc$ind, levels = STANi4.INDALL)
  data.calc <- melt(data.calc, id.vars = c("cou", "ind", "year"), variable.name = "indic", na.rm = TRUE)
  data.calc <- transform(data.calc, value = round(value, stanindic_rounddec))

  ## sort values for plotting
  ## ## sort countries in data according to highest industry value in stanindic_indic
  data.calc.order <- data.calc

  ind.top <- data.calc.order$ind[order(-data.calc.order[,"value"])][1] # data.calc.order$indic==stanindic_indic,
  data.calc.order.ind.top <- data.calc.order[data.calc.order$ind==ind.top,]
  data.calc.order$cou <- factor(data.calc.order$cou, levels = unique(data.calc.order.ind.top$cou[order(-data.calc.order.ind.top[,"value"])])) # data.calc.order$indic==stanindic_indic,

  data.calc <- data.calc.order

  ## in pivoted format for d3 charts
  data.calc.indic <- dcast(data.calc.order, cou + ind + year ~ indic, value.var = "value")
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
              stanindic_plottype = stanindic_plottype,
              stanindic_indic = stanindic_indic,
              stanindic_indic2 = stanindic_indic2,
              stanindic_indic3 = stanindic_indic3,
              stanindic_rounddec = stanindic_rounddec,
              stanindic_group = stanindic_group,
              stanindic_showData = stanindic_showData,
              stanindic_showIndic = stanindic_showIndic,
              stanindic_pivotRow = stanindic_pivotRow,
              stanindic_facethoriz = stanindic_facethoriz,
              stanindic_stack = stanindic_stack,
              stanindic_ggtheme = stanindic_ggtheme,
              stanindic_ggcolor = stanindic_ggcolor,
              stanIndic_viz_plot_height = stanIndic_viz_plot_height,
              stanIndic_viz_plot_width = stanIndic_viz_plot_width
              )
         )
}

summary_stanIndic <- function(result = .stanIndic())
{ if (length(result) > 0) {

  data.calc.indic <- result$data.calc.indic
  string.formula <-  result$string.formula

  list.print <- list(
    Formula = string.formula
    ## Formula = toString(string.formula)
    ,
    Data = data.calc.indic)

  return(list.print)

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

highcharts_stanIndic <- function(result = .stanIndic())
{ if (length(result) > 0) {

  data.calc.indic <- result$data.calc.indic
  string.formula <-  result$string.formula
  namecou <- result$namecou
  stanindic_plottype <- result$stanindic_plottype
  stanindic_indic <- result$stanindic_indic
  stanindic_indic2 <- result$stanindic_indic2
  stanindic_indic3 <- result$stanindic_indic3
  stanindic_rounddec <- result$stanindic_rounddec
  stanindic_group <- result$stanindic_group
  stanIndic_viz_plot_height <- result$stanIndic_viz_plot_height
  stanIndic_viz_plot_width <- result$stanIndic_viz_plot_width

  if (stanindic_plottype == "Bar") {
      eval(parse(text = paste0('h1 <- hPlot(', stanindic_indic, ' ~ cou, group = "ind", data = data.calc.indic, type = "column", title = string.formula)')))
      h1$xAxis(categories = levels(data.calc.indic$cou), title = list(text = ""))
  } else if (stanindic_plottype == "Scatter") {
      eval(parse(text = paste0('h1 <- hPlot(', stanindic_indic2, ' ~ ', stanindic_indic, ', group = "', stanindic_group,'", data = data.calc.indic, type = "scatter")')))
  } else if (stanindic_plottype == "Bubble") {
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
  stanindic_plottype <- result$stanindic_plottype
  stanindic_indic <- result$stanindic_indic
  stanindic_indic2 <- result$stanindic_indic2
  stanindic_indic3 <- result$stanindic_indic3
  stanindic_rounddec <- result$stanindic_rounddec
  stanindic_group <- result$stanindic_group
  stanIndic_viz_plot_height <- result$stanIndic_viz_plot_height
  stanIndic_viz_plot_width <- result$stanIndic_viz_plot_width

  if (stanindic_plottype == "Bar") {
      eval(parse(text = paste0('n1 <- nPlot(', stanindic_indic, ' ~ cou, group = "ind", data = data.calc.indic, type = "multiBarChart")')))
      n1$chart(reduceXTicks = FALSE)
      n1$xAxis(ticks = levels(data.calc.indic$cou))
      eval(parse(text = paste0('n1$yAxis(tickFormat = "#!function(x) { return (x).toFixed(', as.numeric(stanindic_rounddec),') }!#")')))

  } else if (stanindic_plottype == "Scatter") {
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

polycharts_stanIndic <- function(result = .stanIndic())
{ if (length(result) > 0) {

    data.calc.indic <- result$data.calc.indic
    string.formula <-  result$string.formula
    namecou <- result$namecou
    stanindic_plottype <- result$stanindic_plottype
    stanindic_indic <- result$stanindic_indic
    stanindic_indic2 <- result$stanindic_indic2
    stanindic_indic3 <- result$stanindic_indic3
    stanindic_rounddec <- result$stanindic_rounddec
    stanindic_group <- result$stanindic_group
    stanIndic_viz_plot_height <- result$stanIndic_viz_plot_height
    stanIndic_viz_plot_width <- result$stanIndic_viz_plot_width

  if (stanindic_plottype == "Bar") {

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

  } else if (stanindic_plottype == "Scatter") {
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

morrischarts_stanIndic <- function(result = .stanIndic())
{ if (length(result) > 0) {

  data.calc.indic <- result$data.calc.indic
  string.formula <-  result$string.formula
  namecou <- result$namecou
  stanindic_plottype <- result$stanindic_plottype
  stanindic_indic <- result$stanindic_indic
  stanindic_indic2 <- result$stanindic_indic2
  stanindic_indic3 <- result$stanindic_indic3
  stanindic_rounddec <- result$stanindic_rounddec
  stanindic_group <- result$stanindic_group


  if (stanindic_plottype == "Bar")
  {
      eval(parse(text = paste0('m1 <- mPlot(', stanindic_indic, ' ~ cou, group = "ind", data = data.calc.indic, type = "Bar")'))) # , labels = unique(data.calc.indic$ind))')))
  } else if (stanindic_plottype%in%c("Line", "Area"))
  {
      eval(parse(text = paste0('data.calc.morris <- transform(data.calc.indic, year = as.character(year), ', stanindic_indic, ' = round(', stanindic_indic, ', stanindic_rounddec))')))
      data.calc.morris.d <- dcast(data.calc.morris, year ~ cou + ind, value.var = stanindic_indic)
      m1 <- mPlot(x = "year", y = names(data.calc.morris.d)[names(data.calc.morris.d)!="year"], type = "Line", data = data.calc.morris.d)
      m1$set(pointSize = 0, lineWidth = 1
             ## ,xLabelFormat = "#! function (x) { return x.toString(); } !#"
             )
      if (stanindic_plottype=="Area")
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


    nameindic <- gsub(", ", "_", toString(result$nameindic))

    tempdir = tempdir()
    unlink(paste0(tempdir, list.files(tempdir)))
    file.remove(file.path(tempdir, list.files(tempdir)))

    file <- file.path(tempdir, paste0('stanIndic_', nameindic, '.csv'))

    ## print(data.table)
    ## print(file)

    write.csv(data.table, file, row.names = FALSE, na = "")

    zip(zipfile = zipfile, files = tempdir, extras = "-j")

}}

plots_stanIndic <- function(result = .stanIndic())
{ if (length(result) > 0) {

    ## result
    data.calc.indic <- result$data.calc.indic
    data.calc <- result$data.calc
    ## data.calc <- read.csv(file.path("C:\\", "Users", "werth_b", "Downloads", "data_calc.csv"))
    ## data.scatter <- read.csv(file.path("C:\\", "Users", "werth_b", "Downloads", "data_calc2.csv"))
    ##
    nameyear <- result$nameyear
    nameind <- result$nameind
    nameindic <- result$nameindic
    ##
    stanindic_plottype <- result$stanindic_plottype
    stanindic_indic <- result$stanindic_indic
    stanindic_indic2 <- result$stanindic_indic2
    stanindic_indic3 <- result$stanindic_indic3
    stanindic_group <- result$stanindic_group
    ##
    stanindic_facethoriz <- result$stanindic_facethoriz
    stanindic_stack <- result$stanindic_stack
    ##
    stanindic_ggtheme <- result$stanindic_ggtheme
    stanindic_ggcolor <- result$stanindic_ggcolor

    ## result transforms
    facet <- stanindic_group
    xValue <- setdiff(c("cou", "ind"), facet) # complementary
    facet.formula <- ifelse(stanindic_facethoriz==TRUE, paste('. ~', facet), paste(facet, '~ .'))
    ##
    theme_select <- eval(parse(text = paste0('theme_', stanindic_ggtheme)))
    scale_fill_select <- eval(parse(text = paste0('scale_fill_', stanindic_ggcolor)))
    scale_color_select <- eval(parse(text = paste0('scale_color_', stanindic_ggcolor)))
    scale_shape_select <- eval(parse(text = paste0('scale_shape_', stanindic_ggcolor)))

    if (stanindic_plottype=="MultiBar") {
        p <- ggplot(data = data.calc)
        x <- xValue
        y <- "value"
        fill <- shape <- "factor(indic)"
        ##
        p <- p + geom_bar(data = subset(data.calc, indic%in%stanindic_indic),
                          aes_string(x = x, y = y, fill = fill),
                          color = "black",
                          position = ifelse(stanindic_stack, "stack", "dodge"),
                          width = 0.4,
                          stat = "identity"
                          )
        p <- p + geom_point(data = subset(data.calc, indic%in%stanindic_indic2),
                            aes_string(x = x, y = y, shape = shape),
                            color = "black",
                            size = 4
                            )
        p <- p +
            xlab(NULL) +
                ylab(NULL)

    } else if (stanindic_plottype=="Scatter") {
        data.scatter <- data.calc
        data.scatter <- dcast(data.scatter, cou + ind + year ~ indic, value.var = "value")
        ##
        p <- ggplot(data = data.scatter)
        x <- stanindic_indic
        y <- stanindic_indic2
        ##
        p <- p + geom_point(aes_string(x = x, y = y, shape = "factor(0)"),
                            color = "black",
                            size = 4
                            )
        p <- p + geom_smooth(aes_string(x = x, y = y), method="lm", se = F)
        p <- p + geom_text(aes_string(x = x, y = y, label = "cou"),
                           size = 4,
                           vjust = .55,
                           hjust = -.25)

    } else if (stanindic_plottype=="Line") {
        data.line <- subset(data.calc, indic == stanindic_indic)
        p <- ggplot(data = data.line)
        ##
        x <- "year"
        y <- "value"
        ##
        p <- p + geom_line(aes_string(x = x, y = y, group = xValue,
                                      linetype = paste0('factor(', xValue, ')'),
                                      color = paste0('factor(', xValue, ')')
                                      ))

        row <- sapply(unique(data.line$cou), function(x) sample(which(data.line$cou==x), 1))
        data.text <- data.line[row,]

        p <- p + geom_text(data = data.text,
                           aes_string(x = x, y = y, label = xValue),
                           size = 4,
                           vjust = -.5,
                           hjust = .5)
        p <- p +
            xlab(NULL) +
                ylab(NULL)
        }

    p <- p +
        facet_grid(facet.formula) +
            scale_fill_select() +
                scale_shape_select() +
                    scale_color_select() +
                        theme_select() +
                            ggtitle(paste0(toString(nameindic), ', ',
                                           toString(nameind), ', ',
                                           ifelse(length(nameyear)>1, paste0(min(nameyear),'-',max(nameyear)), nameyear)))
    return(p)
}}

## stanIndic_plots_multibar <- function(p,
##                                      x,
##                                      y,
##                                      stack,
##                                      fill,
##                                      data
##                                      ) {
##     position <- ifelse(stack, "stack", "dodge")
##     p <- p + geom_bar(data = data,
##                       aes_string(x = x, y = y, fill = fill),
##                       color = "black",
##                       position = position,
##                       width = 0.4,
##                       stat = "identity"
##                       )
##     return(p)
## }

## stanIndic_plots_point <- function(p,
##                                   x,
##                                   y,
##                                   shape,
##                                   data
##                                   ) {
##     p <- p + geom_point(data = data,
##                         aes_string(x = x, y = y, shape = shape),
##                         color = "black",
##                         size = 4
##                         )
##     return(p)
## }

## stanIndic_plots_line <- function(p,
##                                  x,
##                                  y,
##                                  linetype,
##                                  color,
##                                  data,
##                                  group
##                                  ) {
##   p <- p + geom_line(aes_string(x = x, y = y, group = group, linetype = linetype, color = color))

##   return(p)
## }


    ## require(grid)
    ## install.packages("extrafont")
    ## library(extrafont)
    ## font_import()
    ## loadfonts(device="win")

    ## padding.pre <- gsub(", ", "", toString(rep(" ", 4)))
    ## padding.post <- gsub(", ", "", toString(rep(" ", 20)))

    ## labels.color <- c("Unit labour cost")
    ## labels.color <- paste0(padding.pre, labels.color, padding.post)
    ## labels.fill <- c("Labour productivity", "Labour compensation per hour worked")
    ## labels.fill <- paste0(padding.pre, labels.fill, padding.post)

    ## ## sort data by point/color variable
    ## ## p <- ggplot(data = data.calc.indic.m, aes(x = cou)) +
    ## p <- ggplot(data = data.calc.indic.m, aes_string(x = xValue)) +
    ##     geom_bar(
    ##         aes(y = value, fill = factor(var)),
    ##         color = "black",
    ##         stat = "identity",
    ##         position = "dodge",
    ##         width = .6) +
    ##             geom_point(
    ##                 ## aes(y = LULC, colour = factor(0)),
    ##                 aes_string(y = stanIndic, colour = factor(0)),
    ##                 size = 3,
    ##                 shape = 18) +
    ##                     scale_fill_manual(
    ##                         guide_legend(title = ""),
    ##                         values = c("#4F81BD", "#FFFFFF"),
    ##                         labels = labels.fill) +
    ##                             scale_color_manual(
    ##                                 guide_legend(title = ""),
    ##                                 values = c("#000000"),
    ##                                 labels = labels.color) +
    ##                                     scale_y_continuous(
    ##                                         breaks=number_ticks(10),
    ##                                         limits = c(-4, 14)) +
    ##                                             geom_hline(yintercept = 0) +
    ##                                                 ggtitle("2001-2012") +
    ##                                                     theme_oecdcpi()



