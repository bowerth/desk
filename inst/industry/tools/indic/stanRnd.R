data(stanDim)
##
ui.stanRnd.col <- c("#4F81BD", "#C0504D", "#9BBB59", "#8064A2", "#4BACC6", "#F79646")

ui.stanRnd.couoecd <- STAN.COU[["OECD"]]
ui.stanRnd.counonoecd <- c("CHN")
##

ui.stanRnd.ind <- c("D10T12",
                    "D10T11",
                    "D12",
                    "D13T15",
                    "D13",
                    "D14",
                    "D15",
                    "D16T18",
                    "D16",
                    "D17",
                    "D18",
                    "D19",
                    "D20",
                    "D21",
                    "D22",
                    "D23",
                    "D24T25",
                    "D24",
                    "D25",
                    "D26",
                    "D27",
                    "D28",
                    "D29",
                    "D30",
                    "D301",
                    "D303",
                    "D31",
                    "D32",
                    "D33")
## STANi4.INDLABEL$label[STANi4.INDLABEL$ind%in%ui.stanRnd.ind]
## ui.stanRnd.ind.label <- STANi4.INDLABEL$label[STANi4.INDLABEL$ind%in%ui.stanRnd.ind]
ui.stanRnd.ind.label <- paste(ui.stanRnd.ind, STANi4.INDLABEL$label[STANi4.INDLABEL$ind%in%ui.stanRnd.ind])
ui.stanRnd.ind <- as.list(ui.stanRnd.ind)
names(ui.stanRnd.ind) <- ui.stanRnd.ind.label

##
ui.stanRnd.nameind <- STANi4.INDALL
##
ui.stanRnd.namegroup <- rbind.data.frame(
    c(1, "high-tech"),
    c(2, "medium high-tech"),
    c(3, "medium low-tech"),
    c(4, "low-tech")
    )
names(ui.stanRnd.namegroup) <- c("group.no", "group.label")
ui.stanRnd.namegroup$group.label <- factor(ui.stanRnd.namegroup$group.label, levels = ui.stanRnd.namegroup$group.label[order(ui.stanRnd.namegroup$group.no)])
##
ui.stanRnd.ind.groupno <- rbind.data.frame(c("D21", 1),
                                           c("D26", 1),
                                           c("D303", 1),
                                           ##
                                           c("D20", 2),
                                           c("D27", 2),
                                           c("D28", 2),
                                           c("D29", 2),
                                           c("D30", 2),
                                           c("D302A9", 2),
                                           ##
                                           c("D19", 3),
                                           c( "D22T23", 3),
                                           c( "D22", 3),
                                           c( "D23", 3),
                                           c( "D24T25", 3),
                                           c( "D24", 3),
                                           c( "D25", 3),
                                           c( "D301", 3),
                                           c( "D33", 3),
                                           ##
                                           c("D10T12", 4),
                                           c("D10T11", 4),
                                           c( "D10", 4),
                                           c( "D11", 4),
                                           c( "D12", 4),
                                           c( "D13T15", 4),
                                           c( "D13T14", 4),
                                           c( "D13", 4),
                                           c( "D14", 4),
                                           c( "D15", 4),
                                           c( "D16T18", 4),
                                           c( "D16", 4),
                                           c( "D17", 4),
                                           c( "D18", 4),
                                           c( "D31T32", 4),
                                           c( "D31", 4),
                                           c( "D32", 4))
names(ui.stanRnd.ind.groupno) <- c("ind", "group.no")
##
ui.stanRnd.group.ind <- merge(ui.stanRnd.ind.groupno, ui.stanRnd.namegroup)
ui.stanRnd.nameind <- ui.stanRnd.nameind[ui.stanRnd.nameind%in%ui.stanRnd.group.ind$ind]
ui.stanRnd.group.ind$ind <- factor(ui.stanRnd.group.ind$ind, levels = ui.stanRnd.nameind)

##
## move part to global.R script
##
## dat <- isolate(values[["STANNAi4"]])
## ##
## ui.stanRnd.xrates <- dat$DATA.XRATES
## names(ui.stanRnd.xrates) <- sub("var", "cur", names(ui.stanRnd.xrates))
## ## apply exchange rates
## ui.stanRnd.anberd <- dat$DATA.ANBERD
## ui.stanRnd.anberd <- merge(ui.stanRnd.anberd, ui.stanRnd.xrates, by = c("cou", "year"))
## ui.stanRnd.anberd$value <- ui.stanRnd.anberd$value.x / ui.stanRnd.anberd$value.y
## ui.stanRnd.anberd <- subset(ui.stanRnd.anberd, select = c("cou", "var", "ind", "year", "value", "cur"))
## ##
## ui.stanRnd.stan <- dat$DATA.STAN[dat$DATA.STAN$var%in%c("VALU", "PROD"),]
## ui.stanRnd.stan <- merge(ui.stanRnd.stan, ui.stanRnd.xrates, by = c("cou", "year"))
## ui.stanRnd.stan$value <- ui.stanRnd.stan$value.x / ui.stanRnd.stan$value.y
## ui.stanRnd.stan <- subset(ui.stanRnd.stan, select = c("cou", "var", "ind", "year", "value", "cur"))
##
##
##
ui.stanRnd.stderr <- function(x){sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))}
ui.stanRnd.lowsd <- function(x){return(mean(x)-ui.stanRnd.stderr(x))}
ui.stanRnd.highsd <- function(x){return(mean(x)+ui.stanRnd.stderr(x))}

stanRnd_plot_points <- function(data.calc,
                                stanrnd_var,
                                ui.stanRnd.group.ind,
                                stanrnd_error,
                                stanrnd_centr,
                                ui.stanRnd.lowsd,
                                ui.stanRnd.highsd,
                                stanrnd_coulabel,
                                stanrnd_coulabel.size,
                                nameyear
                                )
    {

        ## ## aggregate over time period using mean
        ## data.agg <- aggregate(data.calc[colnames(data.calc)%in%c(stanrnd_var, "RDNC")], by = list(data.calc$cou, data.calc$ind), "mean")
        ## names(data.agg)[1:2] <- c("cou", "ind")
        ## data.agg$ratio <- data.agg[, "RDNC"] / data.agg[, stanrnd_var]
        data.agg <- data.calc

        data.agg <- merge(ui.stanRnd.group.ind, data.agg) # ui.stanRnd.group.ind first to use order of factor levels
        p <- ggplot(data.agg, aes(x = ind, y = ratio, label = cou, order = ind)) +
            geom_point() +
                facet_grid(. ~ group.label, scales = "free", space = "free") +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                        xlab(NULL) +
                            ylab(paste('RDNC per', stanrnd_var)) +
                                ggtitle(paste0('Central tendency and error (red) by ISIC Rev. 4 industry, grouped by corresponding ISIC Rev. 3 intensity definition, ',
                                               min(nameyear), '-', max(nameyear)))
        if (stanrnd_error=="minmax") {
                p <- p + stat_summary(fun.y = stanrnd_centr, fun.ymin = min, fun.ymax = max, colour='red')
            }
        if (stanrnd_error=="sderr") {
                p <- p + stat_summary(fun.y = stanrnd_centr, fun.ymin = ui.stanRnd.lowsd, fun.ymax = ui.stanRnd.highsd, colour='red')
            }
        if (stanrnd_coulabel==TRUE) {
                p <- p + geom_text(size = stanrnd_coulabel.size)
            }
        return(p)
    }

stanRnd_plot_line <- function(data.calc,
                              stanrnd_ind,
                              stanrnd_var,
                              ui.stanRnd.group.ind,
                              stanrnd_scalelogs,
                              stanrnd_scalefree,
                              stanrnd_coulabel,
                              stanrnd_coulabel.size,
                              stanrnd_showvalues,
                              nameyear
                              ) {

    ## data.plot <- data.calc[data.calc$ind%in%stanrnd_ind,]
    ## data.plot$ratio <- data.plot[, "RDNC"] / data.plot[, stanrnd_var]

    data.plot <- data.calc
    data.plot <- merge(ui.stanRnd.group.ind, data.plot)
    ## data.plot.m <- melt(data.plot, id.vars=c("ind", "cou", "cur", "year", "type", "group.no", "group.label"), variable.name="var")
    data.plot.m <- melt(data.plot, id.vars=c("ind", "cou", "cur", "year", "group.no", "group.label"), variable.name="var")
    ##
    ncol <- length(unique(data.plot.m$cou))
    color.fill <- colorRampPalette(ui.sdmxBrowser.col)(ncol)
    ## Ratio plot
    p.ratio <- ggplot(data.plot.m[data.plot.m$var%in%c("ratio"),], aes(x = year, y = value, label = cou)) +
        geom_line(aes(colour = cou)) +
            geom_smooth(method = "loess") +
                scale_colour_manual(values = color.fill) +
                    theme_bw() +
                        xlab(NULL) +
                            ylab(NULL) +
                                ggtitle(paste0('Ratio: RDNC per ', stanrnd_var, ', ', min(nameyear), '-', max(nameyear)))
    if (stanrnd_coulabel==TRUE) {
        p.ratio <- p.ratio + geom_text(size = stanrnd_coulabel.size)
    }
    if (stanrnd_showvalues==TRUE) {
        ## Values plots
        p.values <- ggplot(data = data.plot.m[data.plot.m$var%in%c("RDNC", stanrnd_var),], aes(x = year, y = value, group = cou, label = cou)) +
            geom_line(aes(colour = cou)) +
                scale_colour_manual(values = color.fill) +
                    theme_bw() +
                        guides(colour = FALSE) +
                            xlab(NULL) +
                                ggtitle(paste("Values"))
        if (stanrnd_scalelogs==TRUE) {
            p.values <- p.values + scale_y_log10(name = "USD, in logs")
        } else {
            p.values <- p.values + ylab("USD")
        }
        if (stanrnd_scalefree==TRUE) {
            p.values <- p.values + facet_grid(var ~ ., space = "fixed", scales = "free")
        } else {
            p.values <- p.values + facet_grid(var ~ ., space = "fixed", scales = "fixed")
        }
        if (stanrnd_coulabel==TRUE) {
            p.values <- p.values + geom_text(size = stanrnd_coulabel.size)
        }
        p <- arrangeGrob(p.values, p.ratio, ncol = 2)
    } else {
        p <- p.ratio
    }
    return(p)
}

ui.stanRnd.plotchoices <- c("Points", "Lines")

output$ui_stanRnd <- renderUI({

  ## doLogin()
  ## if (loginData$LoggedIn) {

    list(

      ## selectInput("type", "R&D Measure:",
      ##             list(
      ##               "Main Activity" = "MA",
      ##               "Product Field" = "PF"
      ##               )
      ##             ),

        conditionalPanel(condition = "input.tabs_stanRnd == 'Plots'",
                         wellPanel(
                             checkboxInput("viz_plot_controls", "Plot options", FALSE),
                             conditionalPanel(condition = "input.viz_plot_controls == true",
                                              sliderInput(inputId = "stanRnd_viz_plot_width", label = "Width:", min = 400, max = 1600, value = 850, step = 50)
                                              ,
                                              sliderInput(inputId = "stanRnd_viz_plot_height", label = "Height:", min = 300, max = 1200, value = 500, step = 50)
                                              ,
                                              wellPanel(h5("Export"),
                                                        numericInput("stanrnd_downloadplotwidth", "Width", 18),
                                                        numericInput("stanrnd_downloadplotheight", "Height", 10),
                                                        selectInput("stanrnd_downloadplotformat", "Format", as.list(c("PDF", "PNG")), selected = "PDF", multiple = FALSE),
                                                        downloadButton("download_stanRnd", "Download Plot")
                                                        )
                                              )
                             )
                         ,
                         selectInput("stanrnd_plot", "Choose plot:", ui.stanRnd.plotchoices, state_init("stanrnd_plot", "Points"))
                         ,
                         conditionalPanel(condition="input.stanrnd_plot=='Points'",
                                          wellPanel(
                                              selectInput("stanrnd_centr", "Measure of Central Tendency:",
                                                          list("Mean" = "mean",
                                                               "Median" = "median")
                                                          )
                                              ,
                                              selectInput("stanrnd_error", "Error bars:",
                                                          list("Standard error" = "sderr",
                                                               "Minimum and maximum" = "minmax")
                                                          )
                                              )
                                          )
                         ,
                         conditionalPanel(condition="input.stanrnd_plot=='Lines'",
                                          wellPanel(
                                              selectInput("stanrnd_ind", "Industry:", as.list(ui.stanRnd.ind), selected = "D10T12", multiple = FALSE)
                                              ,
                                              checkboxInput("stanrnd_showvalues", "show values separately", TRUE),
                                              conditionalPanel(condition="input.stanrnd_showvalues==true",
                                                               checkboxInput("stanrnd_scalelogs", "y-scale in logs", TRUE),
                                                               checkboxInput("stanrnd_scalefree", "free y-scale", TRUE)
                                                               )
                                              )
                                          )
                         ,
                         wellPanel(
                             checkboxInput("stanrnd_coulabel", "Add country labels", FALSE)
                             ,
                             conditionalPanel(condition="input.stanrnd_coulabel==true",
                                              numericInput("stanrnd_coulabel.size", "Size of country labels:", 3)
                                              )
                             )
                         )
        ,

        selectInput("stanrnd_var", "STAN Variable:",
                    list("Value-Added" = "VALU",
                         "Gross Output" = "PROD")
                    )
        ,
        selectInput("stanrnd_cur", "Currency:",
                    list(
                        "Exchange Rate" = "EXCH"
                        ,
                        "Purchasing Power Parities" = "PPPS" # add to DATA.XRATES
                        ## unique(DATA.XRATES$var)
                        )
                    )
        ,
        selectInput("stanrnd_couoecd", "OECD countries", as.list(ui.stanRnd.couoecd), selected = sample(ui.stanRnd.couoecd, 10), multiple = TRUE)
        ,
        selectInput("stanrnd_counonoecd", "Non-OECD countries", as.list(ui.stanRnd.counonoecd), selected = ui.stanRnd.counonoecd, multiple = TRUE)
        ,
        sliderInput("stanrnd_time",
                    "Time Range:",
                    value = c(2000,2011),
                    min = 2000,
                    max = 2011,
                    step = 1,
                    format="#")
        ,
        helpAndReport("STAN R&D", "stanRnd", inclMD("tools/help/stanRnd.md"))

      ) # list(...

  ## } else
  ##   {
  ##     h3("Please log in")
  ##   }

})

stanRnd_widthSize <- reactive({
    ifelse(is.null(input$stanRnd_viz_plot_width), return(values$plotWidth), return(input$stanRnd_viz_plot_width))
})
stanRnd_heightSize <- reactive({
    ifelse(is.null(input$stanRnd_viz_plot_height), return(values$plotHeight), return(input$stanRnd_viz_plot_height))
})

output$stanRnd <- renderUI({
  ## for input-output
  statTabPanel(menu_name = "STAN", # menu_name: for side bar - coincide with navbarMenu
               fun_name = "R&D Intensity",   # fun_name
               rfun_label = ".stanRnd", # rfun_label
               fun_label = "stanRnd" # fun_label
               ## ,rChart_lib = input$stanindic_rchartlib
               ,fun_tabs = c("DataTables", "Plots") # , "Tables")
               ,widthFun = "stanRnd_widthSize"
               ,heightFun = "stanRnd_heightSize"
               )
})

.stanRnd <- reactive({
  ## reactive that calls the function for main analysis
  ## . used to indicate this is an 'internal' function
  ##
  ## if (length(input$stanindic_dimS) == 0) return ()
  ##
  stanRnd(
    stanrnd_plot = input$stanrnd_plot,
    stanrnd_var = input$stanrnd_var,
    stanrnd_cur = input$stanrnd_cur,
    stanrnd_centr = input$stanrnd_centr,
    stanrnd_ind = input$stanrnd_ind,
    stanrnd_error = input$stanrnd_error,
    stanrnd_time = input$stanrnd_time,
    stanrnd_showvalues = input$stanrnd_showvalues,
    stanrnd_scalelogs = input$stanrnd_scalelogs,
    stanrnd_scalefree = input$stanrnd_scalefree,
    stanrnd_coulabel = input$stanrnd_coulabel,
    stanrnd_coulabel.size = input$stanrnd_coulabel.size,
    stanrnd_couoecd = input$stanrnd_couoecd,
    stanrnd_counonoecd = input$stanrnd_counonoecd,
    stanrnd_downloadplotwidth = input$stanrnd_downloadplotwidth,
    stanrnd_downloadplotheight = input$stanrnd_downloadplotheight,
    stanrnd_downloadplotformat = input$stanrnd_downloadplotformat,
    ## stanrnd_downloadPlot = input$stanrnd_downloadPlot,
    viz_plot_height = input$viz_plot_height,
    viz_plot_width = input$viz_plot_width)
})
## isolate(.stanRnd())
##
## observe({
##   if(is.null(input$stanRndReport) || input$stanRndReport == 0) return()
##   isolate({
##     inp <- list(
##       input$datasets,
##
##       input$stanindic_indic,
##       ui.stanRnd.year[as.numeric(input$stanindic_time)],
##       input$stanindic_demand,
##       ui.stanRnd.namesec.agg[as.numeric(input$stanindic_indX)],
##       ## indS = ui.stanRnd.namesec.agg[as.numeric(input$indS)],
##       names(ui.stanRnd.namereg.agg)[as.numeric(input$stanindic_couS)],
##       names(ui.stanRnd.namereg.agg)[as.numeric(input$stanindic_couX)],
##       names(ui.stanRnd.namereg.agg)[as.numeric(input$stanindic_couD)]
##       )
##
##     updateReport(inp,"stanRnd")
##   })
## })

## ## input <- list(
##     stanrnd_couoecd = c("AUT", "BEL")
##     stanrnd_counonoecd = c("CHN")
##     stanrnd_time = c(2000, 2010)
##     stanrnd_cur = "EXCH"
##     stanrnd_var = "VALU"
##     ## )


stanRnd <- function(
  stanrnd_plot = stanrnd_plot,
  stanrnd_var = stanrnd_var,
  stanrnd_cur = stanrnd_cur,
  stanrnd_centr = stanrnd_centr,
  stanrnd_ind = stanrnd_ind,
  stanrnd_error = stanrnd_error,
  stanrnd_time = stanrnd_time,
  stanrnd_showvalues = stanrnd_showvalues,
  stanrnd_scalelogs = stanrnd_scalelogs,
  stanrnd_scalefree = stanrnd_scalefree,
  stanrnd_coulabel = stanrnd_coulabel,
  stanrnd_coulabel.size = stanrnd_coulabel.size,
  stanrnd_couoecd = stanrnd_couoecd,
  stanrnd_counonoecd = stanrnd_counonoecd,
  stanrnd_downloadplotwidth = stanrnd_downloadplotwidth,
  stanrnd_downloadplotheight = stanrnd_downloadplotheight,
  stanrnd_downloadplotformat = stanrnd_downloadplotformat,
  ## stanrnd_downloadPlot = stanrnd_downloadPlot,
  viz_plot_height = viz_plot_height,
  viz_plot_width = viz_plot_width)
{

    namecou <- NULL
    namecou <- c(namecou, stanrnd_couoecd)
    namecou <- c(namecou, stanrnd_counonoecd)

    nameyear <- c(stanrnd_time[1]:stanrnd_time[2])

    ## dat <- isolate(values[["STANNAi4"]])
    dat <- values[["STANNAi4"]]

    names(dat$DATA.XRATES) <- sub("var", "cur", names(dat$DATA.XRATES))

    data.xrates <- dat$DATA.XRATES[dat$DATA.XRATES$cur==stanrnd_cur &
                                   dat$DATA.XRATES$cou%in%namecou &
                                   dat$DATA.XRATES$year%in%nameyear,]

    data.anberd <- dat$DATA.ANBERD[## dat$DATA.ANBERD$cur==stanrnd_cur &
                                   dat$DATA.ANBERD$cou%in%namecou &
                                   dat$DATA.ANBERD$ind%in%ui.stanRnd.nameind &
                                   dat$DATA.ANBERD$year%in%nameyear,]

    data.stan <- dat$DATA.STAN[dat$DATA.STAN$var==stanrnd_var &
                               ## dat$DATA.STAN$cur==stanrnd_cur &
                               dat$DATA.STAN$cou%in%namecou &
                               dat$DATA.STAN$ind%in%ui.stanRnd.nameind &
                               dat$DATA.STAN$year%in%nameyear,]

    data.anberd <- merge(data.anberd, data.xrates, by = c("cou", "year"))
    data.anberd$value <- data.anberd$value.x / data.anberd$value.y
    data.anberd <- subset(data.anberd, select = c("cou", "var", "ind", "year", "value", "cur"))

    ## ui.stanRnd.stan <- dat$DATA.STAN[dat$DATA.STAN$var%in%c("VALU", "PROD"),]
    data.stan <- merge(data.stan, data.xrates, by = c("cou", "year"))
    data.stan$value <- data.stan$value.x / data.stan$value.y
    data.stan <- subset(data.stan, select = c("cou", "var", "ind", "year", "value", "cur"))

    ## ## create data
    ## data.anberd <- ui.stanRnd.anberd[# anberd$type==stanrnd_type &
    ##                                  ui.stanRnd.anberd$cur==stanrnd_cur &
    ##                                  ui.stanRnd.anberd$cou%in%namecou &
    ##                                  ui.stanRnd.anberd$ind%in%ui.stanRnd.nameind &
    ##                                  ui.stanRnd.anberd$year%in%nameyear,]
    ## ## data.anberd.d <- dcast(data.anberd, cou + ind + cur + year + type ~ var, value.var = "value")
    data.anberd.d <- dcast(data.anberd, cou + ind + cur + year ~ var, value.var = "value")

    ## data.stan <- ui.stanRnd.stan[ui.stanRnd.stan$var==stanrnd_var &
    ##                              ui.stanRnd.stan$cur==stanrnd_cur &
    ##                              ui.stanRnd.stan$cou%in%namecou &
    ##                              ui.stanRnd.stan$ind%in%ui.stanRnd.nameind &
    ##                              ui.stanRnd.stan$year%in%nameyear,]
    data.stan.d <- dcast(data.stan, cou + ind + cur + year ~ var, value.var = "value")

    data.calc <- merge(data.anberd.d, data.stan.d, by = c("cou", "cur", "ind", "year"))
    data.calc <- data.calc[!is.na(data.calc[, "RDNC"]) & !is.na(data.calc[, stanrnd_var]), ]
    ## data.calc$ratio <- data.calc[, "RDNC"] / data.calc[, stanrnd_var] # ratio calculated in plot functions - after aggregation (mean)
    if (stanrnd_plot == "Points") {
        ## aggregate over time period using mean
        data.calc <- aggregate(data.calc[colnames(data.calc)%in%c(stanrnd_var, "RDNC")], by = list(data.calc$cou, data.calc$ind), "mean")
        names(data.calc)[1:2] <- c("cou", "ind")
    } else if (stanrnd_plot == "Lines") {
        data.calc <- data.calc[data.calc$ind%in%stanrnd_ind,]
    }
    data.calc$ratio <- data.calc[, "RDNC"] / data.calc[, stanrnd_var]

    return(list(
        stanrnd_plot = stanrnd_plot,
        stanrnd_var = stanrnd_var,
        stanrnd_cur = stanrnd_cur,
        stanrnd_centr = stanrnd_centr,
        stanrnd_ind = stanrnd_ind,
        stanrnd_error = stanrnd_error,
        stanrnd_time = stanrnd_time,
        stanrnd_showvalues = stanrnd_showvalues,
        stanrnd_scalelogs = stanrnd_scalelogs,
        stanrnd_scalefree = stanrnd_scalefree,
        stanrnd_coulabel = stanrnd_coulabel,
        stanrnd_coulabel.size = stanrnd_coulabel.size,
        stanrnd_couoecd = stanrnd_couoecd,
        stanrnd_counonoecd = stanrnd_counonoecd,
        stanrnd_downloadplotwidth = stanrnd_downloadplotwidth,
        stanrnd_downloadplotheight = stanrnd_downloadplotheight,
        stanrnd_downloadplotformat = stanrnd_downloadplotformat,
        ## stanrnd_downloadPlot = stanrnd_downloadPlot,
        viz_plot_width = viz_plot_height,
        viz_plot_width = viz_plot_width,
        data.calc = data.calc,
        nameyear = nameyear)
           )
}

summary_stanRnd <- function(result = .stanRnd())
{ if (length(result) > 0) {

    data.calc <- result$data.calc
    stanrnd_plot <- result$stanrnd_plot
    stanrnd_ind <- result$stanrnd_ind
    ## stanrnd_var <- result$stanrnd_var
    nameyear = result$nameyear


    time_range <- paste(min(nameyear), max(nameyear), sep = '-')
    ## data.calc$ratio <- data.calc[, "RDNC"] / data.calc[, stanrnd_var]
    stat_sum <- summary(data.calc[,!colnames(data.calc)%in%c("cou", "cur", "ind", "year")])
    list.print <- list(Time_Range = time_range,
                       Summary_Statistics = stat_sum)

    if (stanrnd_plot=="Points") {
        stanrnd_couoecd <- result$stanrnd_couoecd
        stanrnd_counonoecd <- result$stanrnd_counonoecd
        coulabel.oecd <- STAN.COUEN$countryUNen[STAN.COUEN$cou%in%stanrnd_couoecd]
        coulabel.nonoecd <- STAN.COUEN$countryUNen[STAN.COUEN$cou%in%stanrnd_counonoecd]
        list.print <- c(list(OECD_Countries = toString(coulabel.oecd),
                             Non_OECD_Countries = toString(coulabel.nonoecd)
                             ),
                        list.print)
    }
    if (stanrnd_plot=="Lines") {
        indlabel <- names(ui.stanRnd.ind[ui.stanRnd.ind==stanrnd_ind])
        list.print <- c(list(ISIC4_Industry = indlabel),
                        list.print)
    }

    return(list.print)

}}


tables_stanRnd <- function(result = .stanRnd())
{ if (length(result) > 0) {

  data.calc <- result$data.calc
  data.calc

}}

datatables_stanRnd <- function(result = .stanRnd())
{    if (length(result) > 0) {

    data.calc <- result$data.calc
    data.calc

}}

plots_stanRnd <- function(result = .stanRnd())
{ if (length(result) > 0) {

  data.calc <- result$data.calc
  stanrnd_plot <- result$stanrnd_plot
  stanrnd_var <- result$stanrnd_var
  stanrnd_ind <- result$stanrnd_ind
  stanrnd_error <- result$stanrnd_error
  stanrnd_coulabel <- result$stanrnd_coulabel
  stanrnd_centr <-result$stanrnd_centr
  stanrnd_coulabel.size <- result$stanrnd_coulabel.size
  stanrnd_scalelogs <- result$stanrnd_scalelogs
  stanrnd_scalefree <- result$stanrnd_scalefree
  stanrnd_showvalues <- result$stanrnd_showvalues
  nameyear <- result$nameyear

  if (stanrnd_plot == "Points") {
      p <- stanRnd_plot_points(data.calc = data.calc,
                               stanrnd_var = stanrnd_var,
                               ui.stanRnd.group.ind = ui.stanRnd.group.ind,
                               stanrnd_error = stanrnd_error,
                               stanrnd_centr = stanrnd_centr,
                               ui.stanRnd.lowsd = ui.stanRnd.lowsd,
                               ui.stanRnd.highsd = ui.stanRnd.highsd,
                               stanrnd_coulabel = stanrnd_coulabel,
                               stanrnd_coulabel.size = stanrnd_coulabel.size,
                               nameyear = nameyear)

  } else if (stanrnd_plot == "Lines") {
      p <-  stanRnd_plot_line(data.calc = data.calc,
                              stanrnd_ind = stanrnd_ind,
                              stanrnd_var = stanrnd_var,
                              ui.stanRnd.group.ind = ui.stanRnd.group.ind,
                              stanrnd_scalelogs = stanrnd_scalelogs,
                              stanrnd_scalefree = stanrnd_scalefree,
                              stanrnd_coulabel = stanrnd_coulabel,
                              stanrnd_coulabel.size = stanrnd_coulabel.size,
                              stanrnd_showvalues = stanrnd_showvalues,
                              nameyear = nameyear)
  }

  ## print(p)
  return(p)
}}

download_stanRnd <- function(result = .stanRnd(), zipfile = fname) {
    if (length(result) > 0) {

        data.calc <- result$data.calc
        stanrnd_plot <- result$stanrnd_plot
        stanrnd_var <- result$stanrnd_var
        stanrnd_ind <- result$stanrnd_ind
        stanrnd_error <- result$stanrnd_error
        stanrnd_coulabel <- result$stanrnd_coulabel
        stanrnd_centr <-result$stanrnd_centr
        stanrnd_coulabel.size <- result$stanrnd_coulabel.size
        stanrnd_scalelogs <- result$stanrnd_scalelogs
        stanrnd_scalefree <- result$stanrnd_scalefree
        stanrnd_showvalues <- result$stanrnd_showvalues
        nameyear <- result$nameyear

        stanrnd_downloadplotwidth <- result$stanrnd_downloadplotwidth
        stanrnd_downloadplotheight <- result$stanrnd_downloadplotheight
        stanrnd_downloadplotformat <- result$stanrnd_downloadplotformat

        tempdir = tempdir()
        unlink(paste0(tempdir, list.files(tempdir)))
        file.remove(file.path(tempdir, list.files(tempdir)))

        file <- file.path(tempdir, paste0('chart', '.', stanrnd_downloadplotformat))

        if (stanrnd_downloadplotformat=="PDF") pdf(file=file, width=stanrnd_downloadplotwidth, height=stanrnd_downloadplotheight)
        if (stanrnd_downloadplotformat=="PNG") png(file=file, width=stanrnd_downloadplotwidth, height=stanrnd_downloadplotheight, units = "in", res = 300)

        if (stanrnd_plot == "Points") {
            p <- stanRnd_plot_points(data.calc = data.calc,
                                     stanrnd_var = stanrnd_var,
                                     ui.stanRnd.group.ind = ui.stanRnd.group.ind,
                                     stanrnd_error = stanrnd_error,
                                     stanrnd_centr = stanrnd_centr,
                                     ui.stanRnd.lowsd = ui.stanRnd.lowsd,
                                     ui.stanRnd.highsd = ui.stanRnd.highsd,
                                     stanrnd_coulabel = stanrnd_coulabel,
                                     stanrnd_coulabel.size = stanrnd_coulabel.size,
                                     nameyear = nameyear)

        } else if (stanrnd_plot == "Lines") {
            p <-  stanRnd_plot_line(data.calc = data.calc,
                                    stanrnd_ind = stanrnd_ind,
                                    stanrnd_var = stanrnd_var,
                                    ui.stanRnd.group.ind = ui.stanRnd.group.ind,
                                    stanrnd_scalelogs = stanrnd_scalelogs,
                                    stanrnd_scalefree = stanrnd_scalefree,
                                    stanrnd_coulabel = stanrnd_coulabel,
                                    stanrnd_coulabel.size = stanrnd_coulabel.size,
                                    stanrnd_showvalues = stanrnd_showvalues,
                                    nameyear = nameyear)
        }
        print(p)
        dev.off()

        zip(zipfile = zipfile, files = tempdir, extras = "-j")

}}



