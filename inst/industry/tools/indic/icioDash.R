
## devtools::install(file.path(dbpath, "GitHub", "icioData"))
## require(shiny)
## require(icioData)
## dat <- new.env()
## data("ICIO6234APP", package = "icioData", envir = dat)
## ## dim(dat$DATA.ICIO6234GRTR)
## ls(dat)
## dim(dat$DATA.ICIO6234VB)
## path <- file.path(dbpath, "GitHub", "desk", "inst", "industry")
## setwd(path)


dat <- isolate(values[["ICIO6234APP"]])

## ls(dat)
ui.icioDash.year <- as.numeric(dimnames(dat$DATA.ICIO6234GRTR)[[1]]) # c(1995:2011)  # 17 years
ui.icioDash.cou <- dimnames(dat$DATA.ICIO6234GRTR)[[3]]

ui.icioDash.ind <- dimnames(dat$DATA.ICIO6234GRTR)[[2]]
X <- strsplit(ui.icioDash.ind, split = "_")
ui.icioDash.ind <- unique(sapply(X, "[[", 2))
## dim(grtr)
## DATA.ICIO6234GRTR <- grtr[, , c(1:dim(grtr)[3]-1)]  # remove "DISC" from dimension 3

ui.icioDash.nocou <- dim(dat$DATA.ICIO6234GRTR)[3] # previous: [2]
ui.icioDash.noind <- dim(dat$DATA.ICIO6234GRTR)[2] / ui.icioDash.nocou # previous: [1]
##
ui.icioDash.dim_conv <- list(row = c(ui.icioDash.nocou, ui.icioDash.noind),
                             col = c(ui.icioDash.nocou))


## ## ui.icioDash.dimnames=list(ui.icioDash.rownames, ui.icioDash.colnames)
## ui.icioDash.dimnames <- list(dimnames(dat$DATA.ICIO6234GRTR)[[2]],
##                              dimnames(dat$DATA.ICIO6234GRTR)[[3]])

## new file: I:\I-O\2015sut-io\0_LookupFiles\Reggrp_ones.csv
## metapath <- file.path(PATH.IO, "2015sut-io", "0_LookupFiles")
## file.copy(
## 	from = file.path(metapath, "countrygroups.csv"),
## 	to = file.path(dbpath, "GitHub", "desk", "inst", "industry", "data", "data_init", "ICIO6234_countrygroups.csv"))
## namereg.df <- read.csv(file.path(metapath, "countrygroups.csv"), header = TRUE)


ui.icioDash.namereg.df <- read.csv(file.path("data", "data_init", "ICIO6234_countrygroups.csv"), header = TRUE)
ui.icioDash.namereg.select <- c("EU28", "E_ASIA", "NAFTA", "ASEAN8")

showConverter_icioDash <- function(m,
                                   xLabels=NULL,
                                   yLabels=NULL) {
  z <- t(m)
  z <- z[, rev(seq_len(ncol(z)))]
  if (is.null(xLabels)) xLabels <- 1:dim(z)[1]
  if (is.null(yLabels)) yLabels <- 1:dim(z)[2]
  i <- image(x = c(1:length(xLabels)),
             y = c(1:length(yLabels)),
             z = z,
             col = c("white", "green"),
             axes = FALSE)
  axis(BOTTOM<-1, at=1:length(xLabels), labels=xLabels, las = HORIZONTAL <- 1, cex.axis=0.7, tick=FALSE)
  axis(TOP<-3, at=1:length(xLabels), labels=xLabels, las = HORIZONTAL <- 1, cex.axis=0.7, tick=FALSE)
  axis(LEFT<-2, at=1:length(yLabels), labels=rev(yLabels), las = VERTICAL <- 2, cex.axis=0.7, tick=FALSE)
  axis(RIGHT<-4, at=1:length(yLabels), labels=rev(yLabels), las = VERTICAL <- 2, cex.axis=0.7, tick=FALSE)
  ## ?axis
  return(i)
}

aggMatrix_icioDash <- function(data,
                      toRow,
                      toCol,
                      displayConv=FALSE) {
  sourceRow <- unname(unlist(toRow))
  nocou <- dim(data)[2]
  noind <- dim(data)[1] / nocou
  conv.pre <- array(0, dim = c(length(toRow), nocou * noind))
  for (i in seq(along = toRow)) {
    conv.pre[as.numeric(i), c(((sourceRow[i]-1)*noind+1):((sourceRow[i]-1)*noind+noind-1))] <- 1
  }
  conv.post <- array(0, dim = c(nocou, length(toCol)))
  for (j in seq(along = toCol)) {
    conv.post[toCol[[j]], j] <- 1
  }
  if (displayConv==TRUE) {
    op <- par(mfrow = c(2, 1))
    showConverter_icioDash(m = conv.pre, yLabels = names(toRow))
    showConverter_icioDash(conv.post, xLabels = names(toCol), yLabels = dimnames(data)[[2]])
  }
  data <- conv.pre %*% data %*% conv.post
  dimnames(data) <- list(names(toRow), names(toCol))
  return(data)
}

ui.icioDash.charttype <- c("GRTRnoDomestic",
                           "VAsource")

## io.domestic <- function(data=stop("'data' needs to be specified"),
##                         action = "keep") {
##   if (action=="keep") {
##         for (c in c(1:dim(data)[2])) data[((c-1)*ui.icioDash.noind+1):((c-1)*ui.icioDash.noind+ui.icioDash.noind), c] <- 0
##   } else if (action=="remove") {
##   }
## }


icioDash.data.matrix <- reactive({

  ## ## testing
  ## input <- list(iciodash_couXlist = c("USA", "AUT", "AUS", "CAN", "FRA", "DEU"),
  ##               iciodash_couXlist = c("USA", "CAN"),
  ##               iciodash_year = 2008,
  ##               iciodash_calcshare = TRUE)

  dat <- values[["ICIO6234APP"]]
  grtr <- dat$DATA.ICIO6234GRTR[match(input$iciodash_year, ui.icioDash.year),,]

  if (input$iciodash_chart %in% "GRTRnoDomestic") {
    ## remove domestic
    for (c in c(1:dim(grtr)[2])) grtr[((c-1)*ui.icioDash.noind+1):((c-1)*ui.icioDash.noind+ui.icioDash.noind), c] <- 0
    data.matrix <- grtr

  } else if (input$iciodash_chart %in% "VAsource") {
    ##  VA source by industry for each country
    vB <- dat$DATA.ICIO6234VB[match(input$iciodash_year, ui.icioDash.year),,]

    grtr.sum <- apply(grtr, 1, "sum")

    ## data.matrix <- vB %*% diag(grtr.sum) # too slow
    data.matrix.2346.2346 <- t(t(vB) * grtr.sum)
    ## dim(data.matrix)

    ## select only domestic from grtr
    ## create converter to select domestic elements from GRTR
    ## structure: [1 1 0 0 0 0 : 0 0 0 0 0 0 : 0 0 0 0 0 0 : ... ]
    ##            [0 0 0 0 0 0 : 0 0 1 1 0 0 : 0 0 0 0 0 0 : ... ]
    ##            [0 0 0 0 0 0 : 0 0 0 0 0 0 : 0 0 0 0 1 1 : ... ]
    ## conv.cou <- array(0, dim = c(ui.icioDash.noind * ui.icioDash.nocou, ui.icioDash.nocou)) # 2346 x 69
    conv.cou <- array(0, dim = c(ui.icioDash.nocou, ui.icioDash.nocou * ui.icioDash.noind)) # 69 x 2346
    ## c <- 1
    ## for (c in c(1:dim(conv.cou)[2])) {
    for (c in c(1:dim(conv.cou)[1])) {
      ## conv.cou[((c-1)*ui.icioDash.noind+1):((c-1)*ui.icioDash.noind+ui.icioDash.noind), c] <- 1
      conv.cou[c, ((c-1)*ui.icioDash.noind+1):((c-1)*ui.icioDash.noind+ui.icioDash.noind)] <- 1
    }
    ## showConverter_icioDash(conv.cou)
    ## grtr.dom <- grtr * conv.cou
    ## grtr[1:35, 1:5]
    ## grtr.dom[1:35, 1:10]
    ## conv.cou[1:35, 1:10]

    data.matrix.69.2346 <- conv.cou %*% data.matrix.2346.2346
    data.matrix <- t(data.matrix.69.2346)
    ## dim(data.matrix)
    ## data.matrix <- vB %*% grtr.dom
  }

  return(data.matrix)

})

output$uiId_couXplot <- renderUI({
  couXlist <- input$iciodash_couXlist
  selectInput("iciodash_couXplot", "Plot Exporting Country:", couXlist, selected = couXlist[1:2], multiple = TRUE)
})


output$ui_icioDash <- renderUI({
  list(
    selectInput("iciodash_chart", "Chart type:", ui.icioDash.charttype,
                selected = ui.icioDash.charttype[1],
                multiple = FALSE)
    ,
    selectInput("iciodash_couXlist", "Exporting Country List:", ui.icioDash.cou,
                selected = c("USA", "CAN", "CHN", "MEX", "DEU", "FRA"),
                multiple = TRUE)
    ,
    htmlOutput("uiId_couXplot")
    ,
    selectInput("iciodash_year", "Year:", ui.icioDash.year, selected = 2008, multiple = FALSE)
    ,
    checkboxInput("iciodash_calcshare", "calculate share in total exports", TRUE)
    ,
    ## checkboxInput("iciodash_singlecouX", "show distribution for single country", TRUE)
    ## ,
    helpAndReport("ICIO Dashboards", "icioDash", inclMD(file.path("tools", "help", "icioDash.md")))
    )
})

icioDash_widthSize <- reactive({
    ifelse(is.null(input$icioDash_viz_plot_width), return(values$plotWidth), return(input$icioDash_viz_plot_width))
})
## icioDash_heightSize <- reactive({
##     ifelse(is.null(input$icioDash_viz_plot_height), return(values$plotHeight), return(input$icioDash_viz_plot_height))
## })
icioDash_heightSize <- reactive({
    ifelse(is.null(input$icioDash_viz_plot_height), return(values$plotHeight), return(input$icioDash_viz_plot_height))
})

output$icioDash <- renderUI({
  ## for input-output
  statTabPanel(menu_name = "ICIO", # menu_name: for side bar - coincide with navbarMenu
               fun_name = "ICIO Dashboards",           # fun_name
               rfun_label = ".icioDash",         # rfun_label
               fun_label = "icioDash"           # fun_label
               ,fun_tabs = c("Plots")
               ,widthFun = "icioDash_widthSize"
               ,heightFun = "icioDash_heightSize"
               )
})


.icioDash <- reactive({
    icioDash(
      ## iciodash_couX = input$iciodash_couX,
      iciodash_couXlist = input$iciodash_couXlist,
      iciodash_year = input$iciodash_year,
      iciodash_calcshare = input$iciodash_calcshare,
      iciodash_couXplot = input$iciodash_couXplot,
      icioDash_viz_plot_width = input$icioDash_viz_plot_width,
      icioDash_viz_plot_height = input$icioDash_viz_plot_height
      )
})

icioDash <- function(
  ## iciodash_couX = iciodash_couX,
  iciodash_couXlist = iciodash_couXlist,
  iciodash_year = iciodash_year,
  iciodash_calcshare = iciodash_calcshare,
  iciodash_couXplot = iciodash_couXplot,
  icioDash_viz_plot_width = icioDash_viz_plot_width,
  icioDash_viz_plot_height = icioDash_viz_plot_height
  ) {

  data.matrix <- icioDash.data.matrix()


  ## c <- 34
  ## data[((c-1)*noind+1):((c-1)*noind+noind), c]
  ## data[(c-1)*noind+1, c]


  toRow <- as.list(match(iciodash_couXlist, ui.icioDash.cou))
  names(toRow) <- iciodash_couXlist

  ## ROW not included
  toCol <- NULL
  for (region in ui.icioDash.namereg.select) {
    members <- ui.icioDash.namereg.df[["X"]][ui.icioDash.namereg.df[[region]]==1]
    members.list <- list(as.character(members))
    names(members.list) <- region
    toCol <- c(toCol, members.list)
  }
  ## use numeric values
  toCol <- lapply(toCol, match, ui.icioDash.cou)
  ## add total
  toCol <- c(toCol, list(Total = seq(along=ui.icioDash.cou)))

  ## ## display the converters
  ## aggMatrix_icioDash(data = data,
  ##                    toRow = toRow,
  ##                    toCol = toCol,
  ##                    displayConv = TRUE)

  ## toRow
  ## noind
  ## dim(data)
  ## dimnames(data)[[1]][(34-1)*34+1]
  ## dimnames(data)[[1]][(4-1)*34+1]


  data.out <- aggMatrix_icioDash(data = data.matrix,
                                 toRow = toRow,
                                 toCol = toCol,
                                 displayConv = FALSE)

  data.out.df <- as.data.frame(data.out)
  data.out.total <- subset(data.out.df, select = "Total")

  if (iciodash_calcshare==TRUE) {
    data.out.df <- data.out.df / data.out.df[["Total"]]
    data.out.df <- data.out.df[, !colnames(data.out.df)%in%c("Total")]
    data.out <- as.matrix(data.out.df)
    ## barchart(data.out.m)
  }

  return(
    list(
      ## iciodash_couX = iciodash_couX,
      iciodash_couXlist = iciodash_couXlist,
      iciodash_year = iciodash_year,
      data.out = data.out,
      data.out.total = data.out.total,
      toRow = toRow,
      toCol = toCol,
      iciodash_couXplot = iciodash_couXplot,
      icioDash_viz_plot_width = icioDash_viz_plot_width,
      icioDash_viz_plot_height = icioDash_viz_plot_height
      )
    )
}

summary_icioDash <- function(result = .icioDash()) {
    if (length(result) > 0) {

      ## iciodash_couX = result$iciodash_couX
      iciodash_couXlist = result$iciodash_couXlist
      iciodash_couXplot = result$iciodash_couXplot
      iciodash_year = result$iciodash_year
      toRow = result$toRow
      toCol = result$toCol
      data.out <- result$data.out

      list.print <- list(data.out = data.out,
                         ## couX = iciodash_couX,
                         couXlist = iciodash_couXlist,
                         year = iciodash_year,
                         toRow = toRow,
                         toCol = toCol)
      ## return(cat(data.out[1,1]))
      return(print(list.print))

    }
}

plots_icioDash <- function(result = .icioDash()) {
  if (length(result) > 0) {

      data.out <- result$data.out
      data.out.total <- result$data.out.total
      iciodash_couXlist <- result$iciodash_couXlist
      ## iciodash_couX <- result$iciodash_couX
      iciodash_couXplot = result$iciodash_couXplot

      op <- par(mfrow = c(2, 1),
                mar = c(5, 3, 3, 5))
      ## p <- barchart(data.out.m)
      ## p <- barplot(data.out.total$Total, names = iciodash_couXlist)
      p1 <- barplot(data.out.total$Total, names = rownames(data.out.total))

      ## data.out <- data.out.m
      ## data.pie <- data.out[rownames(data.out)=="ISR"]
      ## data.pie <- data.out[rownames(data.out)==iciodash_couX]

      ## p2 <- stars(data.out,
      ##             nrow = 3,
      ##             len = 0.8,
      ##             key.loc = c(12, 1.5),
      ##             draw.segments = TRUE)

      ## iciodash_couXplot <- c("USA", "CAN", "DEU", "FRA")
      ## data.out.df <- as.data.frame(data.out[rownames(data.out)%in%iciodash_couXplot,])

      p2 <- dotchart(
        ## data.out
        x = t(data.out[rownames(data.out)%in%iciodash_couXplot,])
                     )

## data.out[,]

##       class(data.out[rownames(data.out)%in%c("USA"),])

##       iciodash_couXplot

  }
}
