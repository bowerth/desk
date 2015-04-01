
#' #######
#' CHECK
#' #######
#' Can not match keys in x and y to automatically determine appropriate `by` parameter. Please set `by` value explicitly.
#'
#' exgrdva
#' rei
#' fddvash
#'
#' #######




## require(brew)
## require(knitr)
## require(markdown)
## require(xlsx)
## require(ggplot2)
## require(reshape2)
## library(RColorBrewer)
## library(gridExtra)

## path.data <- file.path("//ASAP5", "STI", "Progs", "STAN", "I-O", "2013sut-io", "icio", "indic2013-05-17")
## filename <- "indic_oecdwto_cubeAC"
## icioIndic_data <- NULL
## for (yr in c(1995, 2009)) {
##     data.yr <- read.csv(file.path(path.data, paste0(filename, yr, '.csv')), header=F, sep="|")
##     names(data.yr) <- c("indic", "year", "cou", "par", "ind", "value")
##     data.yr$year <- as.numeric(data.yr$year)
##     data.yr$value <- as.numeric(data.yr$value)
##     data.yr <- data.yr[data.yr$cou%in%namereg$cou & data.yr$par%in%union("Total", namereg$cou),]
##     icioIndic_data <- rbind(icioIndic_data, data.yr)
## }
## icioIndic_data_bsci <- sqlQuery(connection, "SELECT Cou, Ind, Indic, SUM(SHARE) AS SHARE, SrcIndgrp1, srcreg4, Year FROM TIVAPUB_CUBEB_EXPINDwtSH WHERE Ind = '30T33' AND Indic = 'EXGR_VA_BSCI' AND Year IN (1995, 2009) GROUP BY Cou, Ind, Indic, SrcIndgrp1, srcreg4, Year")
## names(icioIndic_data_bsci) <- tolower(names(icioIndic_data_bsci))
## ## unique(icioIndic_data_bsci$srcreg4)
## ## Europe, North America, East and S.E. Asia Other regions, South America
## ## save(list = c("icioIndic_data", "icioIndic_data_bsci"), file = file.path("data", "data_init", "icioIndic.rda"))
load(file.path("data", "data_init", "icioIndic.rda"))
icioIndic_data <- data.table(icioIndic_data)

icioIndic_nameind <- read.csv(file.path("data", "data_init", "icioIndic_nameind.csv"))
icioIndic_namereg <- read.csv(file.path("data", "data_init", "icioIndic_namereg.csv"))
## icioIndic_nameindic <- read.csv(file.path("data", "data_init", "icioIndic_nameindic.csv"))

## oecd, eu28, americas, eastasia, asean, asiapacific, othereu28, restofworld
icioIndic_reg <- list()
icioIndic_reg[["oecd"]] <- as.character(icioIndic_namereg$cou[icioIndic_namereg$inoecd==1])
icioIndic_reg.label <- rbind.data.frame(c("EU28", "eu28"),
                                        c("Americas", "americas"),
                                        c("East Asia", "eastasia"),
                                        c("ASEAN", "asean"),
                                        c("Ot.ASIA-PACIFIC", "asiapacific"),
                                        c("OtherEUR", "othereu28"),
                                        c("Restof the World", "restoftheworld"))
names(icioIndic_reg.label) <- c("label", "reg")
for (i in seq(along = icioIndic_reg.label$label)) {
    reg <- as.character(icioIndic_reg.label$reg[i])
    icioIndic_reg[[reg]] <- as.character(icioIndic_namereg$cou[icioIndic_namereg$region==as.character(icioIndic_reg.label$label[i])])
}

## XLConnect
icioIndic_cntext <- XLConnect::readWorksheetFromFile(file = file.path("data", "data_init", "icioIndic_custom_text.xlsx"), sheet=1)

## modify scientific notation
options(scipen=8)
axis.text.size=12
legend.text.size=14

ui.icioIndic.cou <- STAN.COU[["ICIO"]]

ui.icioIndic.indic <- list(exgrdvaex = "EXGRDVA_EX",
                           exgrdva = "EXGRDVA",
                           exgrfvash = c("EXGR_FVASH", "EXGR_DDCSH", "EXGR_IDCSH", "EXGR_RIMSH"),
                           rei = "REI",
                           exgrvabsci = "EXGR_VA_BSCI",
                           fddvash = c("FDFVASH", "FDDVASH", "EXGRSH", "IMGRSH"),
                           tsvafd = c("TSVAFD", "TSGR"),
                           servvagr = c("EXGR", "EXGR_DDC_SV", "EXGR_IDC_SV", "EXGR_RIM_SV", "EXGR_FVA_SV"),
                           servvagrind = c("EXGR", "EXGR_DDC_SV", "EXGR_IDC_SV", "EXGR_RIM_SV", "EXGR_FVA_SV")
                           )

ui.icioIndic.chart <- names(ui.icioIndic.indic)

ui.icioIndic.year <- c(1995, 2009)


output$ui_icioIndic <- renderUI({

  ## doLogin()
  ## if (loginData$LoggedIn) {

    list(

      conditionalPanel(condition = "input.tabs_icioIndic == 'Plots'",
                       wellPanel(
                         checkboxInput("icioIndic_viz_plot_controls", "Plot options", FALSE),
                         conditionalPanel(condition = "input.icioIndic_viz_plot_controls == true",
                                          ## htmlOutput("ui_plot_options"),
                                          sliderInput(inputId = "icioIndic_viz_plot_width", label = "Width:", min = 900, max = 2400, value = 900, step = 50),
                                          conditionalPanel(condition = "input.tabs_icioIndic == 'Plots'",
                                                           sliderInput(inputId = "icioIndic_viz_plot_height", label = "Height:", min = 400, max = 1200, value = 600, step = 50)
                                                           )
                                          )
                         )
                       ),
        selectInput("icioindic_cou", "Country:", ui.icioIndic.cou, selected = "AUT", multiple = FALSE, selectize = TRUE),
        selectInput("icioindic_chart", "Chart:", ui.icioIndic.chart, selected = "chart1", multiple = FALSE, selectize = TRUE),
        selectInput("icioindic_year", "Year:", ui.icioIndic.year, selected = 2009, multiple = FALSE, selectize = TRUE),
        selectInput("icioindic_refyear", "Reference Year:", ui.icioIndic.year, selected = 1995, multiple = FALSE, selectize = TRUE),
        sliderInput("icioindic_span", "Span:", min = 0.2, max = 1, value = 0.5, step = 0.1),
        ## uiOutput("icioIndic_ggvis_ui"),
        helpAndReport("TiVA Indicators", "icioIndic", inclMD(file.path("tools", "help", "icioIndic.md")))
        ) # list(...

})


icioIndic_widthSize <- reactive({
    ifelse(is.null(input$icioIndic_viz_plot_width), return(values$plotWidth), return(input$icioIndic_viz_plot_width))
})
icioIndic_heightSize <- reactive({
    ifelse(is.null(input$icioIndic_viz_plot_height), return(values$plotHeight), return(input$icioIndic_viz_plot_height))
})

output$icioIndic <- renderUI({
  ## for input-output
  statTabPanel(menu_name = "ICIO", # menu_name: for side bar - coincide with navbarMenu
               fun_name = "TiVA Indicators",           # fun_name
               rfun_label = ".icioIndic",         # rfun_label
               fun_label = "icioIndic"           # fun_label
               ,fun_tabs = c("Plots")
               ## ,fun_tabs = c("Plots", "HTML") # , "NVD3Charts") #, "ggVis") # , "Tables", "Maps")
               ,widthFun = "icioIndic_widthSize"
               ,heightFun = "icioIndic_heightSize"
               )
})

.icioIndic <- reactive({

    icioIndic(
        icioindic_cou = input$icioindic_cou,
        icioindic_chart = input$icioindic_chart,
        icioindic_year = input$icioindic_year,
        icioindic_refyear = input$icioindic_refyear,
        icioindic_span = input$icioindic_span
        )
})


icioIndic <- function(
    icioindic_cou = icioindic_cou,
    icioindic_chart = icioindic_chart,
    icioindic_year = icioindic_year,
    icioindic_refyear = icioindic_refyear,
    icioindic_span = icioindic_span
    ) {

    nameindic <- ui.icioIndic.indic[[icioindic_chart]]

    country <- as.character(icioIndic_namereg$country[match(icioindic_cou, icioIndic_namereg$cou)])
    natnlty <- as.character(icioIndic_namereg$coupron[match(icioindic_cou, icioIndic_namereg$cou)])
    customtext <- subset(icioIndic_cntext, cou==icioindic_cou)

    if (icioindic_chart %in% c("exgrdvaex")) {

      data <- subset(icioIndic_data, indic%in%nameindic & ind=="Total")
      fun_exgrdvaex <- function(data,
                                cou = icioindic_cou) {
        data$cou <- factor(data$cou, levels = data$cou[order(data$value)])
        data$col <- 1
        data$col[data$cou==cou] <- 0
        return(data)
      }
      data.year <- fun_exgrdvaex(data = subset(data, year==icioindic_year))
      data.refyear <- fun_exgrdvaex(data = subset(data, year==icioindic_refyear))
      data <- rbind(data.year, data.refyear)

      .perc <- round(data.year$value[data.year$cou==icioindic_cou])
      avrg <- round(mean(data.year$value[data.year$cou%in%icioIndic_reg[["oecd"]]]))
      if (.perc >= (avrg + 5))
        .rel <- "above"
      else if (.perc < (avrg - 5))
        .rel <- "below"
      else
        .rel <- "around"
      blurb <- paste0(country, '\'s domestic value-added content of its exports is, at ', .perc, '%, ', .rel, ' the OECD average in ', icioindic_year, '.')
      data.summary <- list(title = paste('Domestic value added content of gross exports,', icioindic_year),
                           avrg = avrg)

    }
    if (icioindic_chart %in% c("exgrdva")) {

      data <- subset(icioIndic_data, indic%in%nameindic & cou==icioindic_cou)
      fun_exgrdva <- function(data,
                              nameind = icioIndic_nameind) {
        data$share <- data$value / data$value[data$ind=='Total'] * 100
        data <- merge(data, nameind)
        industry_wrap <- NULL
        for(i in seq(along=nameind$industry)) industry_wrap[i] <- sub('_', '\n', nameind$industry[i])
        data$industry <- factor(data$industry, levels=nameind$industry, labels=industry_wrap)
        data$col <- 1
        data <- data[!data$ind%in%c("Total", "40T41", "45"),] # drop on request
      }
      data.year <- fun_exgrdva(data = subset(data, year==icioindic_year))
      data.refyear <- fun_exgrdva(data = subset(data, year==icioindic_refyear))
      data <- rbind(data.year, data.refyear)

      blurb <- ""
      data.summary <- list(title = paste('Domestic Value added in gross exports, industry shares,', icioindic_year))

    }
    if (icioindic_chart %in% c("exgrfvash")) {

      data <- subset(icioIndic_data, indic%in%nameindic & cou==icioindic_cou)
      fun_exgrfvash <- function(data,
                                cou = icioindic_cou,
                                nameind = icioIndic_nameind) {
        data21 <- data[data$indic=='EXGR_FVASH',]
        data22 <- data[data$indic%in%c('EXGR_DDCSH','EXGR_IDCSH','EXGR_RIMSH'),]
        data2 <- NULL
        for (inds in unique(data$ind)) {
          data2tmp <- cbind.data.frame(cou, inds, sum(data21$value[data21$ind==inds]), sum(data22$value[data22$ind==inds]))
          names(data2tmp) <- c('cou','ind','EXGR_F','EXGR_D')
          data2 <- rbind(data2, data2tmp)
        }
        data2m <- melt(data2, id.vars=c('cou','ind'))
        data2m <- merge(data2m, nameind)
        industry_wrap <- NULL
        for(i in seq(along=nameind$industry)) industry_wrap[i] <- sub('_', '\n', nameind$industry[i])
        data2m$industry <- factor(data2m$industry, levels=nameind$industry, labels=industry_wrap)
        data2m$variable <- factor(data2m$variable, levels=rev(levels(data2m$variable)))
        data2m$col <- 1
        data2m$col[data2m$variable=='EXGR_D'] <- 2
        data2m$col[data2m$ind=='Total'] <- 3
        data2m$col[data2m$variable=='EXGR_D' & data2m$ind=='Total'] <- 4
        data2m <- data2m[!data2m$ind%in%c('40T41','45'),] # drop on request
        data2m <- subset(data2m, variable=="EXGR_F")
        data2m$year <- unique(data$year)
        return(data2m)
      }
      data.year <- fun_exgrfvash(data = subset(data, year==icioindic_year))
      data.refyear <- fun_exgrfvash(data = subset(data, year==icioindic_refyear))
      data <- rbind(data.year, data.refyear)

      data.year.extr <- subset(data.year, ind!="Total")
      data.year.extr <- data.year.extr[order(data.year.extr$variable, -data.year.extr$value),]
      data.year.extr <- data.year.extr[1:4, ]
      .perc <- round(data.year.extr$value)
      .ind <- icioIndic_nameind$indlabel[match(data.year.extr$ind, icioIndic_nameind$ind)]
      blurb <- paste0('The foreign content of its exports is highest in the ', .ind[1], ' industry, where about ', .perc[1], '% of the value of exports consists of foreign value added. The ', .ind[2], ', ', .ind[3], ' and ', .ind[4], ' industries also have relatively high shares of foreign content (', .perc[4], '-', .perc[2], '%).')
      data.summary <- list(title = paste('Foreign value-added content of gross exports, by industry,', icioindic_year))

    }
    if (icioindic_chart %in% "rei") {

      data <- subset(icioIndic_data, indic%in%nameindic & cou==icioindic_cou & par=="Total")
      fun_rei <- function(data,
                           nameind = icioIndic_nameind) {
        data <- merge(data, nameind)
        industry_wrap <- NULL
        for(i in seq(along=nameind$industry)) industry_wrap[i] <- sub('_', '\n', nameind$industry[i])
        data$industry <- factor(data$industry, levels=nameind$industry, labels=industry_wrap)
        data$col <- 2
        data$col[data$ind=='Total'] <- 1
        data <- data[!data$ind=='45',] # removed upon request
      }
      data.year <- fun_rei(data = subset(data, year==icioindic_year))
      data.refyear <- fun_rei(data = subset(data, year==icioindic_refyear))
      data <- rbind(data.year, data.refyear)

      .perctotal <- round(data.year$value[data.year$ind=="Total"])
      data.year.extr <- subset(data.year, ind!="Total")
      data.year.extr <- data.year.extr[order(-data.year.extr$value), ]
      data.year.extr <- data.year.extr[1:6, ]
      .perc <- round(data.year.extr$value)
      ## .ind <- NULL
      ## for(i in c(1:6)) .ind[i] <- as.character(nameind$indlabel[nameind$ind==data.year$ind[!data.year$ind=='Total'][i]])
      .ind <- icioIndic_nameind$indlabel[match(data.year.extr$ind, icioIndic_nameind$ind)]
      blurb <- paste0('The share of intermediate imports that are used in producing exports are highest in the following industries: ',
                      .ind[1], ' (', .perc[1], '%); ',
                      .ind[2], ' (', .perc[2], '%); ',
                      .ind[3], ' (', .perc[3], '%); ',
                      .ind[4], ' (', .perc[4], '%); ',
                      .ind[5], ' (', .perc[5], '%) and ',
                      .ind[6], ' (', .perc[6], '%), ',
                      'illustrating the integration of many ', natnlty, ' industries in global value chains. ',
                      .perctotal, '% of all intermediate imports are used to produce exports.')
      data.summary <- list(title = paste('Share of imported intermediate inputs that are exported, by import category,', icioindic_year))

    }
    if (icioindic_chart %in% "exgrvabsci") {

      data <- subset(icioIndic_data_bsci, cou==icioindic_cou & srcindgrp1!="Total")
      data <- merge(data, icioIndic_nameind, by.x='srcindgrp1', by.y="ind")
      industry_wrap <- NULL
      for(i in seq(along=icioIndic_nameind$industry)) industry_wrap[i] <- sub('_', '\n', icioIndic_nameind$industry[i])
      data$industry <- factor(data$industry, levels=icioIndic_nameind$industry[order(icioIndic_nameind$ind)], labels=industry_wrap)

      ## require(reshape2)
      data.year.extr <- subset(data, year==icioindic_year)
      .percind <- dcast(data = data.year.extr, formula = srcindgrp1 ~ year, value.var = 'share', fun.aggregate=sum)
      .percind <- melt(.percind, id.vars='srcindgrp1', variable.name='year')
      .percind <- merge(.percind, icioIndic_nameind[,colnames(icioIndic_nameind)%in%c('ind','indlabel')], by.x='srcindgrp1', by.y='ind')
      .percind <- .percind[order(-.percind$value),]

      .percreg <- dcast(data = data.year.extr, formula = srcreg4 ~ year, value.var = 'share', fun.aggregate=sum)
      .percreg <- melt(.percreg, id.vars='srcreg4', variable.name='year')
      .percreg$srcreg4 <- sub('East and S.E. Asia','East and South East Asia',.percreg$srcreg4)
      .percreg <- .percreg[order(-.percreg$value),]

      blurb <- paste0('In ', icioindic_year, ', ',
                      round(.percind$value[1]), '% of ', country, '\'s exports in value added terms originated in the ',
                      .percind$indlabel[1], ' sector. The industry is well integrated in global value chains, with significant inputs of intermediate goods and services from ',
                      .percreg$srcreg4[1], ' (', round(.percreg$value[1]), '% of the value added in gross exports).')

      data.summary <- list(title = paste('Origin of foreign value-added in gross exports, electrical equipment (ISIC Rev. 3 30T33), by region and product group,', icioindic_year))

    }
    if (icioindic_chart %in% "fddvash") {

      data <- subset(icioIndic_data, indic%in%nameindic & cou==icioindic_cou & par!="ROW" & ind=="Total" )

      fun_fddvash <- function(data,
                              namereg = icioIndic_namereg,
                              n = 10,
                              indicator = "FDDVASH") {
        ## data51 <- data[data$indic%in%c("FDFVASH", "FDDVASH"),]
        data1 <- subset(data, indic==indicator)
        data1 <- merge(data1, namereg, by.x='par')
        names(data1) <- sub("country", "partner", names(data1))
        ## Share of VA exports
        ## data12 <- data1[data1$indic==indic,]
        data1$partner <- factor(data1$partner, levels=data1$partner[order(data1$value)])
        data1 <- data1[order(-data1$value),]
        ## Gross export share by partner
        indicator2 <- ifelse(indicator=="FDDVASH", "EXGRSH", "IMGRSH")
        ## data53 <- data[data$par!="Total" & data$indic=='EXGRSH',]
        data2 <- subset(data, par!="Total" & indic==indicator2)
        data2 <- merge(data2, namereg, by.x='par')
        names(data2) <- sub('country','partner',names(data2))
        data2$partner <- factor(data2$partner, levels=data2$partner[order(data2$value)])
        data2 <- data2[order(-data2$value),]
        ##
        data <- rbind(data1[1:n,], data2[data2$par%in%data1$par[1:n],])
        data$par <- factor(data$par, levels=data$par[order(-data$value[data$indic==indicator])])
        data$indic <- factor(data$indic, levels=rev(levels(data$indic)))
        return(data)
      }
      data.export <- fun_fddvash(data = subset(data, year==icioindic_year), indicator = "FDDVASH")
      data.import <- fun_fddvash(data = subset(data, year==icioindic_year), indicator = "FDFVASH")
      data <- rbind(data.export, data.import)

      .pargr <- data.export$partner[data.export$indic=='EXGRSH']
      .parva <- data.export$partner[data.export$indic=='FDDVASH']
      .sharegr <- round(data.export$value[data.export$indic=='EXGRSH'])
      .shareva <- round(data.export$value[data.export$indic=='FDDVASH'])
      .comparepar <- NULL
      for (i in seq(along = .pargr)) {
        if (.pargr[i] == .parva[i]) .comparepar[i] = "remains" else .comparepar[i] = "becomes"
      }

      blurb <- paste0('Looking at gross flows ', .pargr[1], ' and ', .pargr[2], ' are ', country, '\'s major trading partners, consuming about ', .sharegr[1], '% and ', .sharegr[2], '% of its exports, respectively. In value-added terms, ', .parva[1], ' ', .comparepar[1], ' ', country, '\'s biggest partner, consuming about ', .shareva[1], '% of its exports and ', .parva[2], ' ', .comparepar[2], ' it\'s second biggest partner.')

      data.summary <- list(title = paste('Exports and imports, partner shares, in gross and value-added terms, % of total,', icioindic_year))

    }
    if (icioindic_chart %in% "tsvafd") {

      data <- subset(icioIndic_data, indic%in%nameindic & cou==icioindic_cou & !par%in%c("Total", "ROW") & ind=="Total")

      fun_tsvafd <- function(data,
                             n = 11) {
        data1 <- data[data$indic=="TSVAFD",]
        data2 <- data[data$indic=="TSGR",]
        data2 <- data2[order(-data2$value),]
        range <- c(1:round(n/2),(nrow(data2)-round((n-1)/2)):nrow(data2))
        data <- rbind(data2[range,], data1[data1$par%in%data2$par[range],])
        data$par <- factor(data$par, levels=data$par[order(-data$value[data$indic=="TSGR"])])
        return(data)
      }

      data <- fun_tsvafd(data = subset(data, year==icioindic_year))

      blurb <- ""
      data.summary <- list(title = paste('Bilateral trade balances, USD million,', icioindic_year))

    }
    if (icioindic_chart %in% "servvagr") {

      data <- subset(icioIndic_data, indic%in%nameindic & cou!="ROW" & par=="Total" & ind=="Total")

      fun_servvagr <- function(data,
                               cou = icioindic_cou,
                               namereg = icioIndic_namereg) {
        data1 <- dcast(data, year + cou + par + ind ~ indic, value.var = 'value')
        data1$total <- (data1$EXGR_DDC_SV + data1$EXGR_IDC_SV + data1$EXGR_RIM_SV + data1$EXGR_FVA_SV) / data1$EXGR * 100
        data1$foreign <- data1$EXGR_FVA_SV/ data1$EXGR * 100
        data1$domestic <- data1$total - data1$foreign
        data1m <- melt(data1[,colnames(data1)%in%c('year','cou','par','ind','foreign','domestic','total')], id.vars = c('year','cou','par','ind'))
        ## join country labels
        data1m <- merge(data1m, namereg)
        ## order country label factor levels by values ('total')
        ## data1m$country <- factor(data1m$country, levels=data1m$country[data1m$variable=='total'][order(data
        data1m$cou <- factor(data1m$cou, levels=data1m$cou[data1m$variable=='total'][order(data1m[data1m$variable=='total',colnames(data1m)%in%c('value')])])
        ## change order of stacking
        data1m$variable <- factor(data1m$variable, levels=rev(unique(levels(data1m$variable))))
        ## add colours based on country code
        data1m$col <- 1
        data1m$col[data1m$variable=='domestic'] <- 2
        data1m$col[data1m$cou==cou] <- 3
        data1m$col[data1m$cou==cou & data1m$variable=='domestic'] <- 4
        return(data1m)
      }
      data.year <- fun_servvagr(data = subset(data, year==icioindic_year))
      data.year.extr <- subset(data.year, variable%in%c("total"))
      data.year <- subset(data.year, variable%in%c("domestic", "foreign"))
      data.refyear <- fun_servvagr(data = subset(data, year==icioindic_refyear))
      data.refyear <- subset(data.refyear, variable=="total")
      data <- rbind(data.year, data.refyear)

      .perc <- round(sum(data.year.extr$value[data.year.extr$cou==icioindic_cou]))
      avrg <- round(mean(data.year.extr$value[data.year.extr$cou%in%icioIndic_reg[["oecd"]]]))
      if(.perc >= (avrg + 5)) .rel <- "above" else if(.perc < (avrg - 5)) .rel <- "below" else .rel <- "around"

      blurb <- paste0('In value added terms about ', .perc, '% of ', country, '\'s exports reflect services. This is ', .rel, ' the (unweighted) OECD average (about ', avrg, '%).')
      data.summary <- list(title = paste('Services content of gross exports,', icioindic_year),
                           avrg = avrg)

    }

    if (icioindic_chart %in% "servvagrind") {
      data <- subset(icioIndic_data, indic%in%nameindic & cou==icioindic_cou & par=="Total" & !ind%in%c("40T41", "45", "50T55", "60T64", "65T67", "70T74", "75T95", "Total"))
      fun_servvagrind <- function(data,
                                  nameind = icioIndic_nameind
                                  ) {
        data1 <- dcast(data, year + cou + par + ind ~ indic, value.var = 'value')
        data1$total <- (data1$EXGR_DDC_SV + data1$EXGR_IDC_SV + data1$EXGR_RIM_SV + data1$EXGR_FVA_SV) / data1$EXGR * 100
        data1$foreign <- data1$EXGR_FVA_SV/ data1$EXGR * 100
        data1$domestic <- data1$total - data1$foreign
        ## do not need total because ordering by ind (not by value)
        data1m <- melt(data1,  id.vars = c("year", "cou", "par", "ind"))
        data1m <- merge(data1m, nameind)
        industry_wrap <- NULL
        for(i in seq(along=nameind$industry)) industry_wrap[i] <- sub("_", "\n", nameind$industry[i])
        data1m$industry <- factor(data1m$industry, levels=nameind$industry[order(nameind$ind)], labels=industry_wrap)
        ## change order of stacking
        data1m$variable <- factor(data1m$variable, levels=rev(levels(data1m$variable)))
        return(data1m)
      }
      data.year <- fun_servvagrind(data = subset(data, year==icioindic_year))
      data.year <- subset(data.year, variable%in%c("foreign", "domestic"))
      data.refyear <- fun_servvagrind(data = subset(data, year==icioindic_refyear))
      data.refyear <- subset(data.refyear, variable=="total")
      data <- rbind(data.year, data.refyear)
      blurb <- ""
      data.summary <- list(title = paste('Services content of gross exports, by industry,', icioindic_year))
    }

    customtext <- ifelse(is.na(customtext[[icioindic_chart]]), "", customtext[[icioindic_chart]])
    data.summary <- c(data.summary,
                      list(blurb = blurb,
                           customtext = customtext)
                      )
    data.plot <- list(data.year = subset(data, year==icioindic_year),
                      data.refyear = subset(data, year==icioindic_refyear))

    return(list(icioindic_cou = icioindic_cou,
                icioindic_chart = icioindic_chart,
                icioindic_year = icioindic_year,
                icioindic_refyear = icioindic_refyear,
                icioindic_span = icioindic_span,
                data.plot = data.plot,
                data.summary = data.summary,
                nameindic = nameindic))
}

summary_icioIndic <- function(result = .icioIndic()) {
    if (length(result) > 0) {

        icioindic_cou <- result$icioindic_cou
        icioindic_year <- result$icioindic_year
        icioindic_refyear <- result$icioindic_refyear
        ##
        nameindic <- result$nameindic
        data.plot <- result$data.plot
        data.summary <- result$data.summary
        ##
        list.print <- NULL
        if (icioindic_year==icioindic_refyear)
            list.print <- c(list.print, cat("WARNING: Please select 'Reference Year' different from 'Year'\n\n"))
        list.print <- c(list.print, cat(paste0(gsub("(.{1,100})(\\s|$)", "\\1\n", data.summary[["blurb"]]), '\n')))
        list.print <- c(list.print, cat(paste0(gsub("(.{1,100})(\\s|$)", "\\1\n", data.summary[["customtext"]]), '\n')))
        list.print <- c(list.print, list(Indicator = nameindic,
                                         Data = data.plot
                                         ))
        return(list.print)
    }
}

plots_icioIndic <- function(result = .icioIndic()) {
    if (length(result) > 0) {
        icioindic_cou <- result$icioindic_cou
        icioindic_chart <- result$icioindic_chart
        data.plot <- result$data.plot
        data.summary <- result$data.summary
        if (icioindic_chart=="exgrdvaex") {
          p <- icioIndic_plot_exgrdvaex(data.plot,
                                        data.summary)
        }
        if (icioindic_chart=="exgrdva") {
          p <- icioIndic_plot_exgrdva(data.plot,
                                      data.summary)
        }
        if (icioindic_chart=="exgrfvash") {
          p <- icioIndic_plot_exgrfvash(data.plot,
                                        data.summary)
        }
        if (icioindic_chart=="rei") {
          p <- icioIndic_plot_rei(data.plot,
                                  data.summary)
        }
        if (icioindic_chart=="exgrvabsci") {
          p <- icioIndic_plot_exgrvabsci(data.plot,
                                         data.summary)
        }
        if (icioindic_chart=="fddvash") {
          p <- icioIndic_plot_fddvash(data.plot,
                                      data.summary)
        }
        if (icioindic_chart=="tsvafd") {
          p <- icioIndic_plot_tsvafd(data.plot,
                                     data.summary)
        }
        if (icioindic_chart=="servvagr") {
          p <- icioIndic_plot_servvagr(data.plot,
                                       data.summary)
        }
        if (icioindic_chart=="servvagrind") {
          p <- icioIndic_plot_servvagrind(data.plot,
                                          data.summary)
        }
        return(p)
    }
}

## ##################################### ##
## create dynamic charts for jekyll page ##
## ##################################### ##


## cat(paste0('## icioindic_chart = "', names(ui.icioIndic.indic), '",\n'))

## ## Test
## input <- list(
##     icioindic_cou = "AUT",
##     ## icioindic_chart = "exgrdvaex",
##     ## icioindic_chart = "exgrdva",
##     ## icioindic_chart = "exgrfvash",
##     ## icioindic_chart = "rei",
##     ## icioindic_chart = "exgrvabsci",
##     icioindic_chart = "fddvash",
##     ## icioindic_chart = "tsvafd",
##     ## icioindic_chart = "servvagr",
##     ## icioindic_chart = "servvagrind",
##     icioindic_year = 2009,
##     icioindic_refyear = 1995,
##     icioindic_span = 0.5)
## icioindic_cou = input$icioindic_cou
## icioindic_chart = input$icioindic_chart
## icioindic_year = input$icioindic_year
## icioindic_refyear = input$icioindic_refyear
## icioindic_span = input$icioindic_span


## .icioIndic <- reactive({
##     icioIndic(
##         icioindic_cou = input$icioindic_cou,
##         icioindic_chart = input$icioindic_chart,
##         icioindic_year = input$icioindic_year,
##         icioindic_refyear = input$icioindic_refyear,
##         icioindic_span = input$icioindic_span
##         )
## })

## plots_icioIndic(isolate(.icioIndic()))
## plots_icioIndic(isolate(.icioIndic()))

## ## require(rCharts)

## result <- isolate(.icioIndic())
## result <- isolate(icioIndic())
## data.plot <- result$data.plot
## ## data.summary <- result$data.summary


## cat(paste0('## p <- icioIndic_plot_', names(ui.icioIndic.indic), '(data.plot, data.summary)\n'))
## p <- icioIndic_plot_exgrdvaex(data.plot, data.summary)
##  p <- icioIndic_plot_exgrdva(data.plot, data.summary)
 ## p <- icioIndic_plot_exgrfvash(data.plot, data.summary)
 ## p <- icioIndic_plot_rei(data.plot, data.summary)
 ## p <- icioIndic_plot_exgrvabsci(data.plot, data.summary)
 ## p <- icioIndic_plot_fddvash(data.plot, data.summary)
 ## p <- icioIndic_plot_tsvafd(data.plot, data.summary)
 ## p <- icioIndic_plot_servvagr(data.plot, data.summary)
 ## p <- icioIndic_plot_servvagrind(data.plot, data.summary)
## p
## p

## nvd3charts_egxrdvaex <- function(data=data.plot) {
##     data <- rbind(data[["data.year"]], data[["data.refyear"]])
##     data <- data[order(data$cou),]
##     n1 <- nPlot(value ~ cou, group = "year", data = data, type = "multiBarChart")
##     ## n1$chart(stacked = TRUE)
##     n1$chart(reduceXTicks = FALSE)
##     n1$xAxis(rotateLabels = -45)
##     return(n1)
## }


## nvd3charts_servvagr <- function(data=data.plot) {
##     data <- data[["data.year"]]
##     data <- data[order(data$cou),]
##     n1 <- nPlot(value ~ cou, group = "variable", data = data, type = "multiBarChart")
##     n1$chart(stacked = TRUE)
##     n1$chart(reduceXTicks = FALSE)
##     n1$xAxis(rotateLabels = -45)
##     ## n1
##     return(n1)
## }

## outpath <- file.path(dbpath, "GitHub", "jekyll", "industry", "figures", "app_icioIndic")
## n1 <- nvd3charts_egxrdvaex(); htmlfile <- file.path(outpath, "exgrdvaex", "index.html")
## n1 <- nvd3charts_servvagr(); htmlfile <- file.path(outpath, "servvagr", "index.html")
## ## n1
## n1$save(htmlfile, cdn = FALSE)

## fileCon <- file(htmlfile)
## text.body <- readLines(fileCon)
## ## replace js libraries location
## lib <- system.file('libraries', package = "rCharts")
## text.body <- sub(lib, "/www/rCharts", text.body)
## ## replace css location
## text.body <- sub("./index_files", "/www/rCharts", text.body)
## ##
## writeLines(text = text.body, con = fileCon)
## close(fileCon)



## html_icioIndic <- function(result = .icioIndic()) {
##     if (length(result) > 0) {
##         icioindic_span = result$icioindic_span
##         ## icioindic_span <- 0.5
##         ##library(ggvis)
##         p <- print(mtcars %>%
##                        ggvis(~wt, ~mpg) %>%
##                            layer_points() %>%
##                                layer_smooths(span = icioindic_span) # %>%
##                   ,
##                    dynamic = FALSE, launch = FALSE)
##         return(cat(paste0(p[[1]], p[[2]])))
##     }
## }

icioIndic_plot_exgrdvaex <- function(data.plot,
                                     data.summary) {
    f1 <- qplot(cou,
                value,
                data = data.plot[["data.year"]],
                geom = "bar",
                stat = "identity",
                fill = as.factor(col),
                width=0.4,
                position = position_dodge(width=0.5)) +
                    scale_fill_brewer(guide = "none",
                                      palette = 'Set1') +
                                          geom_hline(yintercept = data.summary[["avrg"]], colour = "grey50", size = 0.5, linetype = 2) +
                                              geom_point(data = data.plot[["data.refyear"]], aes(x = as.factor(cou), y = value, colour  = as.factor(col)), size = 3, shape = 18) +
                                                  geom_point(data = data.plot[["data.refyear"]], aes(x = as.factor(cou), y = value), colour  = "white", size = 2.5, shape = 18) +
                                                      scale_colour_brewer(
                                                          guide = "none",
                                                          palette = 'Set1',
                                                          name="",
                                                          labels=c("", "1995")) +
                                                              theme(axis.title.x=element_blank(),
                                                                    axis.title.y=element_blank(),
                                                                    axis.text.x = element_text(angle=90, hjust=1, vjust=.4),
                                                                    axis.text.y = element_text(size=axis.text.size),
                                                                    axis.ticks=element_line(linetype=0),
                                                                    legend.position="top") +
                                                                      ggtitle(data.summary[["title"]])
    return(f1)
}

icioIndic_plot_exgrdva <- function(data.plot,
                                   data.summary) {
    f9 <- qplot(industry,
                share,
                data = data.plot[["data.year"]],
                geom = "bar",
                stat = "identity",
                fill = as.factor(-col),
                width = 0.6,
                position = "stack") +
                    scale_fill_manual(guide = "none",
                                      values = '#377EB8',
                                      name="",
                                      labels=c("", "1995")) +
                                          geom_point(data = data.plot[["data.refyear"]], aes(x = as.factor(industry), y = share, colour = as.factor(col)), size = 4, shape = 18) +
                                              geom_point(data = data.plot[["data.refyear"]], aes(x = as.factor(industry), y = share), colour = "white", size = 3.5, shape = 18) +
                                                  scale_colour_manual(
                                                      values=c("black"),
                                                      guide = "none",
                                                      name="",
                                                      labels=c("1995")) +
                                                          theme(axis.title.x=element_blank(),
                                                                axis.title.y=element_blank(),
                                                                axis.text.x = element_text(angle=90, hjust=1, vjust=.4, size=axis.text.size),
                                                                axis.text.y = element_text(size=axis.text.size),
                                                                axis.ticks=element_line(linetype=0),
                                                                legend.position="top") +
                                                                  ggtitle(data.summary[["title"]])
    return(f9)
}

icioIndic_plot_exgrfvash <- function(data.plot,
                                     data.summary) {
    f2 <- qplot(industry,
                value,
                data = data.plot[["data.year"]],
                geom = "bar",
                stat = "identity",
                fill = as.factor(-col),
                width = 0.4,
                order = variable,
                position = "stack") +
                    scale_fill_brewer(guide = "none",
                                      palette = 'Set1',
                                      name="",
                                      labels=c("", "1995")) +
                                          geom_point(data = data.plot[["data.refyear"]], aes(x = as.factor(industry), y = value, colour = as.factor(col)), size = 3, shape = 18) +
                                              geom_point(data = data.plot[["data.refyear"]], aes(x = as.factor(industry), y = value), colour = "white", size = 2.5, shape = 18) +
                                                  scale_colour_manual(
                                                      values=c("black", "black"), # "Total" second factor
                                                      guide = "none",
                                                      name="",
                                                      labels=c("1995")) +
                                                          theme(axis.title.x=element_blank(),
                                                                axis.title.y=element_blank(),
                                                                axis.text.x = element_text(angle=90, hjust=1, vjust=.4, size=axis.text.size),
                                                                axis.text.y = element_text(size=axis.text.size),
                                                                axis.ticks=element_line(linetype=0),
                                                                legend.position="top") +
                                                                  ggtitle(data.summary[["title"]])
    return(f2)
}

icioIndic_plot_rei <- function(data.plot,
                               data.summary) {
    f3 <- qplot(industry,
                value,
                data = data.plot[["data.year"]],
                geom = "bar",
                stat = "identity",
                fill = as.factor(col),
                width=0.4) +
                    scale_fill_brewer(guide = "none",
                                      palette = 'Set1') +
                                          geom_point(data = data.plot[["data.refyear"]], aes(x = industry, y = value, colour = as.factor(col)), size = 3, shape = 18) +
                                              geom_point(data = data.plot[["data.refyear"]], aes(x = industry, y = value), size = 2.5, shape = 18, colour = "white") +
                                                  scale_colour_manual(values = c("black", "black"),
                                                                      guide = "none",
                                                                      name = "",
                                                                      labels = c("1995")) +
                                                                          theme(axis.title.x = element_blank(),
                                                                                axis.title.y = element_blank(),
                                                                                axis.text.x = element_text(angle=90, hjust=1, vjust=.4, size=axis.text.size),
                                                                                axis.text.y = element_text(size=axis.text.size),
                                                                                axis.ticks = element_line(linetype=0), legend.position="top") +
                                                                                  ggtitle(data.summary[["title"]])
    return(f3)
}

icioIndic_plot_exgrvabsci <- function(data.plot,
                                      data.summary) {
        data.plot <- rbind(data.plot[["data.year"]], data.plot[["data.refyear"]])
        regions <- levels(data.plot$srcreg4)
        f4 <- qplot(data = data.plot,
                    x = as.factor(year),
                    y = share,
                    fill = srcreg4,
                    order = srcreg4,
                    position = "stack",
                    facets = . ~ industry,
                    geom = "bar",
                    stat = "identity", width=0.4) +
                        scale_fill_brewer(guide = guide_legend(keyheight=.2),
                                          palette = "Set1",
                                          name = ""
                                          ,
                                          labels = regions
                                          ) +
                                              theme(axis.title.x = element_blank(),
                                                    axis.title.y = element_blank(),
                                                    axis.text.x = element_text(angle=90, hjust=1, vjust=1.1, size=axis.text.size),
                                                    axis.text.y = element_text(size=axis.text.size),
                                                    axis.ticks=element_line(linetype=0),
                                                    strip.text.x = element_text(size = 8),
                                                    legend.position="top",
                                                    legend.text = element_text(size=legend.text.size)) +
                                                      ggtitle(data.summary[["title"]])
        return(f4)
    }

icioIndic_plot_fddvash <- function(data.plot,
                                   data.summary) {
  data.export <- subset(data.plot[["data.year"]], indic%in%c("FDDVASH", "EXGRSH"))
  data.import <- subset(data.plot[["data.year"]], indic%in%c("FDFVASH", "IMGRSH"))
  ylim <- max(max(data.export$value), max(data.import$value))
  graph1 <- qplot(par,
                  value,
                  data = data.export,
                  geom = "bar",
                  fill = indic,
                  stat = "identity",
                  position = "dodge",
                  width = .65) +
                    scale_fill_brewer(palette = "Set1",
                                      guide = guide_legend(keyheight=.2),
                                      name = "",
                                      labels = c("Share of VA exports", "Share of gross exports")) +
                                        theme(axis.title.x = element_blank(),
                                              axis.title.y = element_blank(),
                                              axis.text.x = element_text(angle=90, hjust=1, vjust=.4, size=axis.text.size),
                                              axis.text.y = element_text(size=axis.text.size),
                                              axis.ticks = element_line(linetype = 0),
                                              legend.position="top",
                                              legend.text = element_text(size=legend.text.size)
                                              ) +
                                                scale_y_continuous(limits = c(0, ylim))
  graph2 <- qplot(par,
                  value,
                  data = data.import,
                  geom = "bar",
                  fill = indic,
                  stat = "identity",
                  position = "dodge",
                  width = .65) +
                    scale_fill_brewer(palette = "Set1",
                                      guide = guide_legend(keyheight=.2),
                                      name = "",
                                      labels = c("Share of VA imports", "Share of gross imports")) +
                                        theme(axis.title.x = element_blank(),
                                              axis.title.y = element_blank(),
                                              axis.text.x = element_text(angle=90, hjust=1, vjust=.4, size=axis.text.size),
                                              axis.text.y = element_text(size=axis.text.size),
                                              axis.ticks = element_line(linetype = 0),
                                              legend.position="top",
                                              legend.text = element_text(size=legend.text.size)
                                              ) +
                                                scale_y_continuous(limits = c(0, ylim))
  return(grid.arrange(graph1, graph2, nrow=1, main = data.summary[["title"]]))
}

icioIndic_plot_tsvafd <- function(data.plot,
                                  data.summary) {
  f6 <- qplot(par,
              value,
              data = data.plot[["data.year"]],
              fill = indic,
              geom = "bar",
              position = "dodge",
              stat = "identity",
              width = 0.65) +
                scale_fill_brewer(palette = 'Paired',
                                  guide = guide_legend(keyheight=.2),
                                  name = "",
                                  labels = c("Gross Trade surplus/deficit (TSGR)", "Value Added surplus/deficit (TSVAFD)")) +
                                    theme(axis.title.x = element_blank(),
                                          axis.title.y = element_blank(),
                                          axis.text.x = element_text(size=axis.text.size),
                                          axis.text.y = element_text(size=axis.text.size),
                                          axis.ticks = element_line(linetype=0),
                                          legend.position = "top",
                                          legend.text = element_text(size=legend.text.size)
                                          ) +
                                            ggtitle(data.summary[["title"]])
  return(f6)
}

icioIndic_plot_servvagr <- function(data.plot,
                                    data.summary) {
    f7 <- qplot(cou,
                value,
                data = data.plot[["data.year"]],
                fill = as.factor(col),
                order = variable,
                position = "stack",
                width = 0.6,
                geom = "bar",
                stat = "identity") +
                  scale_fill_brewer(guide = guide_legend(keyheight=.2),
                                    palette = 'Paired',
                                    name = "",
                                    labels = c("Foreign value added contents of gross exports", "Domestic contents", "", "Total, Reference Year")) +
                                      geom_hline(yintercept = data.summary[["avrg"]], colour = "grey50", size = 0.5, linetype = 2) +
                                        geom_point(data = data.plot[["data.refyear"]], aes(x = as.factor(cou), y = value), size = 3, shape = 18) +
                                          geom_point(data = data.plot[["data.refyear"]], aes(x = as.factor(cou), y = value), size = 2.5, shape = 18, colour = "white") +
                                            theme(axis.title.x = element_blank(),
                                                  axis.title.y = element_blank(),
                                                  axis.text.x = element_text(angle=90, hjust=1, vjust=.4),
                                                  axis.text.y = element_text(size=axis.text.size),
                                                  axis.ticks = element_line(linetype=0),
                                                  legend.position = "top",
                                                  legend.text=element_text(size=legend.text.size)
                                                  ) +
                                                    ggtitle(data.summary[["title"]])
    return(f7)
  }

icioIndic_plot_servvagrind <- function(data.plot,
                                       data.summary) {
    f8 <- qplot(industry,
                value,
                data = data.plot[["data.year"]],
                fill = rev(variable),
                order = variable,
                width = 0.4,
                position = "stack",
                geom = "bar",
                stat = "identity") +
                  scale_fill_brewer(guide = guide_legend(keyheight=.2),
                                    palette = "Paired",
                                    name = ""
                                    ,
                                    labels = c("Foreign service contents", "Domestic service contents", "Total, Reference Year")) + #, as.character(namereg$country[match(cou, namereg$cou)]))
                                    ## ) +
                                      geom_point(data = data.plot[["data.refyear"]], aes(x = industry, y = value), size = 3, shape = 18) +
                                        geom_point(data = data.plot[["data.refyear"]], aes(x = industry, y = value), size = 2.5, shape = 18, colour = "white") +
                                          theme(axis.title.x = element_blank(),
                                                axis.title.y = element_blank(),
                                                axis.text.x = element_text(angle=90, hjust=1, vjust=.4, size=axis.text.size),
                                                axis.text.y = element_text(size=axis.text.size),
                                                axis.ticks = element_line(linetype=0),
                                                legend.position = "top",
                                                legend.text=element_text(size=legend.text.size)
                                                ) +
                                                  ggtitle(data.summary[["title"]])
    return(f8)
  }
