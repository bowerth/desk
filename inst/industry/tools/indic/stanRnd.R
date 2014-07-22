data(stanDim)

ui.couoecd <- STAN.COU
ui.counonoecd <- c("CHN")

ind.rnd <- c("D10T12",
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
STANi4.INDLABEL$label[STANi4.INDLABEL$ind%in%ind.rnd]
ui.ind <- as.list(ind.rnd)
names(ui.ind) <- STANi4.INDLABEL$label[STANi4.INDLABEL$ind%in%ind.rnd]
## ui.ind <- list(
##   "D10T12 Food products, beverages and tobacco" = "D10T12",
##   "D10T11 Food products and beverages" = "D10T11",
##   ## "D10 Food products" = "D10",
##   ## "D11 Beverages" = "D11",
##   "D12 Tobacco products" = "D12",
##   "D13T15 Textiles, wearing apparel and leather" = "D13T15",
##   ## "D13T14 Textiles and wearing apparel" = "D13T14",
##   "D13 Textiles" = "D13",
##   "D14 Wearing apparel" = "D14",
##   "D15 Leather and related products" = "D15",
##   "D16T18 Wood, paper products and printing" = "D16T18",
##   "D16 Wood and products of wood and cork" = "D16",
##   "D17 Paper and paper products" = "D17",
##   "D18 Printing and reproduction of recorded media" = "D18",
##   "D19 Coke and refined petroleum products" = "D19",
##   "D20 Chemicals and chemical products" = "D20",
##   "D21 Basic pharmaceutical products" = "D21",
##   ## "D22T23 Rubber and plastics products" = "D22T23",
##   "D22 Rubber and plastics products" = "D22",
##   "D23 Other non-metallic mineral products" = "D23",
##   "D24T25 Basic metals and fabricated metal products" = "D24T25",
##   "D24 Basic metals" = "D24",
##   "D25 Fabricated metal products" = "D25",
##   "D26 Computer, electronic and optical products" = "D26",
##   "D27 Electrical equipment" = "D27",
##   "D28 Machinery and equipment n.e.c." = "D28",
##   "D29 Motor vehicles, trailers and semi-trailers" = "D29",
##   "D30 Other transport equipment" = "D30",
##   "D301 Building of ships and boats" = "D301",
##   "D303 Air and spacecraft and related machinery" = "D303",
##   ## "D302A9 Railroad equipment and transport equipment n.e.c." = "D302A9",
##   ## "D31T32 Furniture, other manufacturing" = "D31T32",
##   "D31 Furniture" = "D31",
##   "D32 Other manufacturing" = "D32",
##   "D33 Repair and installation of machinery and equipment" = "D33"
##   )
## ##

nameind <- STANi4.INDALL
##
namegroup <- rbind.data.frame(
  c(1, "high-tech"),
  c(2, "medium high-tech"),
  c(3, "medium low-tech"),
  c(4, "low-tech")
  )
names(namegroup) <- c("group.no", "group.label")
namegroup$group.label <- factor(namegroup$group.label, levels = namegroup$group.label[order(namegroup$group.no)])
##
ind.groupno <- rbind.data.frame(c("D21", 1),
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
names(ind.groupno) <- c("ind", "group.no")
##
group.ind <- merge(ind.groupno, namegroup)
nameind <- nameind[nameind%in%group.ind$ind]
group.ind$ind <- factor(group.ind$ind, levels = nameind)

dat <- isolate(values[["STANNAi4"]])

xrates <- dat$DATA.XRATES
names(xrates) <- sub("var", "cur", names(xrates))


## anberd <- dat$DATA.ANBERD[,!colnames(dat$DATA.ANBERD)%in%c("var")]
anberd <- dat$DATA.ANBERD
## names(anberd) <- sub("var", "type", names(anberd))
## anberd$var <- "RDNC"
anberd <- merge(anberd, xrates, by = c("cou", "year"))
anberd$value <- anberd$value.x / anberd$value.y
head(anberd)
## anberd <- subset(anberd, select = c("cou", "var", "ind", "year", "value", "type", "cur"))
anberd <- subset(anberd, select = c("cou", "var", "ind", "year", "value", "cur"))
##
stan <- dat$DATA.STAN[dat$DATA.STAN$var%in%c("VALU", "PROD"),]
stan <- merge(stan, xrates, by = c("cou", "year"))
stan$value <- stan$value.x / stan$value.y
stan <- subset(stan, select = c("cou", "var", "ind", "year", "value", "cur"))
##
stderr <- function(x){sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))}
lowsd <- function(x){return(mean(x)-stderr(x))}
highsd <- function(x){return(mean(x)+stderr(x))}


ui.stanRnd.plotchoices <- c("Points", "Lines")

output$ui_stanRnd <- renderUI({

  doLogin()
  if (loginData$LoggedIn) {

    list(

      ## selectInput("type", "R&D Measure:",
      ##             list(
      ##               "Main Activity" = "MA",
      ##               "Product Field" = "PF"
      ##               )
      ##             ),

      conditionalPanel(condition = "input.tabs_stanRnd == 'Plots' | input.tabs_stanRnd == 'Maps'",
                       wellPanel(
                         checkboxInput("viz_plot_controls", "Plot options", FALSE),
                         conditionalPanel(condition = "input.viz_plot_controls == true",
                                          sliderInput(inputId = "viz_plot_width", label = "Width:", min = 400, max = 1600, value = 800, step = 50),
                                          conditionalPanel(condition = "input.tabs_stanRnd == 'Plots'",
                                                           sliderInput(inputId = "viz_plot_height", label = "Height:", min = 300, max = 1200, value = 600, step = 50)
                                                           )
                                          ## ,
                                          ## conditionalPanel(condition = "input.tabs_stanRnd == 'Maps'",
                                          ##                  numericInput(inputId = "viz_plot_ncut", label = "Number of colours:", 5)
                                          ##                  )
                                          )
                         )
                       ,
                       conditionalPanel(condition = "input.tabs_stanRnd == 'Plots'",
                                        selectInput("stanrnd_plot", "Choose plot:", ui.stanRnd.plotchoices, state_init("stanrnd_plot", "dot"))
                                        )
                       )
      ,
      selectInput("stanrnd_var", "STAN Variable:",
                  list("Value-Added" = "VALU",
                       "Gross Output" = "PROD")
                  ),
      selectInput("stanrnd_cur", "Currency:",
                  list("Purchasing Power Parities" = "PPPS",
                       "Exchange Rate" = "EXCH")
                  ),
      conditionalPanel(condition="input.tabs_stanRnd!=2",
                       selectInput("stanrnd_centr", "Measure of Central Tendency:",
                                   list("Mean" = "mean",
                                        "Median" = "median")
                                   )
                       ),
      conditionalPanel(condition="input.stanrnd_plot=='Lines'",
                       selectInput("stanrnd_ind", "Industry:", as.list(ui.ind), selected = "D10T12", multiple = FALSE)
                       ),
      conditionalPanel(
        condition="input.tabs_stanRnd==1",
        selectInput("stanrnd_error", "Error bars:",
                    list("Standard error" = "sderr",
                         "Minimum and maximum" = "minmax")
                    )
        ),
      sliderInput("stanrnd_time",
                  "Time Range:",
                  value = c(2000,2011),
                  min = 2000,
                  max = 2011,
                  step = 1,
                  format="#"),
      conditionalPanel(condition="input.stanrnd_plot=='Lines'",
                       checkboxInput("stanrnd_showvalues", "Show values separately", TRUE),
                       conditionalPanel(condition="input.stanrnd_showvalues==true",
                                        checkboxInput("stanrnd_scalelogs", "Value scale in logs", TRUE)
                                        )
                       ),
      checkboxInput("stanrnd_coulabel", "Add country labels", FALSE),
      conditionalPanel(condition="input.stanrnd_coulabel==true",
                       numericInput("stanrnd_coulabel.size", "Size of country labels:", 3)
                       ),
      selectInput("stanrnd_couoecd", "OECD countries", as.list(ui.couoecd), selected = ui.couoecd, multiple = TRUE),
      selectInput("stanrnd_counonoecd", "Non-OECD countries", as.list(ui.counonoecd), selected = ui.counonoecd, multiple = TRUE),
      conditionalPanel(condition="input.input.stanrnd_plot=='Points' | input.stanrnd_plot=='Lines'",
                       wellPanel(h5("Export"),
                                 numericInput("stanrnd_downloadplotwidth", "Width", 18),
                                 numericInput("stanrnd_downloadplotheight", "Height", 10),
                                 selectInput("stanrnd_downloadplotformat", "Format", as.list(c("PDF", "PNG")), selected = "PDF", multiple = FALSE),
                                 downloadButton("stanrnd_downloadPlot", "Download Plot")
                                 )
                       )
      
      ) # list(...

  } else
    {
      h3("Please log in")
    }

})
output$stanRnd <- renderUI({
  ## for input-output
  statTabPanel(menu_name = "STAN", # menu_name: for side bar - coincide with navbarMenu
               fun_name = "R&D Intensity",   # fun_name
               rfun_label = ".stanRnd", # rfun_label
               fun_label = "stanRnd" # fun_label
               ## ,rChart_lib = input$stanindic_rchartlib
               ,fun_tabs = c("Plots", "Tables")
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
    stanrnd_coulabel = input$stanrnd_coulabel,
    stanrnd_coulabel.size = input$stanrnd_coulabel.size,
    stanrnd_couoecd = input$stanrnd_couoecd,
    stanrnd_counonoecd = input$stanrnd_counonoecd,
    stanrnd_downloadplotwidth = input$stanrnd_downloadplotwidth,
    stanrnd_downloadplotheight = input$stanrnd_downloadplotheight,
    stanrnd_downloadplotformat = input$stanrnd_downloadplotformat,
    stanrnd_downloadPlot = input$stanrnd_downloadPlot,
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
  stanrnd_coulabel = stanrnd_coulabel,
  stanrnd_coulabel.size = stanrnd_coulabel.size,
  stanrnd_couoecd = stanrnd_couoecd,
  stanrnd_counonoecd = stanrnd_counonoecd,
  stanrnd_downloadplotwidth = stanrnd_downloadplotwidth,
  stanrnd_downloadplotheight = stanrnd_downloadplotheight,
  stanrnd_downloadplotformat = stanrnd_downloadplotformat,
  stanrnd_downloadPlot = stanrnd_downloadPlot,
  viz_plot_height = viz_plot_height,
  viz_plot_width = viz_plot_width)
{

  namecou <- NULL
  namecou <- c(namecou, stanrnd_couoecd)
  namecou <- c(namecou, stanrnd_counonoecd)
  namecou

  nameyear <- c(stanrnd_time[1]:stanrnd_time[2])

    ## create data
        data.anberd <- anberd[# anberd$type==stanrnd_type &
                              anberd$cur==stanrnd_cur &
                              anberd$cou%in%namecou &
                              anberd$ind%in%nameind &
                              anberd$year%in%nameyear,]
        ## data.anberd.d <- dcast(data.anberd, cou + ind + cur + year + type ~ var, value.var = "value")
        data.anberd.d <- dcast(data.anberd, cou + ind + cur + year ~ var, value.var = "value")

        data.stan <- stan[stan$var==stanrnd_var &
                          stan$cur==stanrnd_cur &
                          stan$cou%in%namecou &
                          stan$ind%in%nameind &
                          stan$year%in%nameyear,]
        data.stan.d <- dcast(data.stan, cou + ind + cur + year ~ var, value.var = "value")

        data.calc <- merge(data.anberd.d, data.stan.d, by = c("cou", "cur", "ind", "year"))
        data.calc <- data.calc[!is.na(data.calc[, "RDNC"]) & !is.na(data.calc[, stanrnd_var]), ]


  
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
    stanrnd_coulabel = stanrnd_coulabel,
    stanrnd_coulabel.size = stanrnd_coulabel.size,
    stanrnd_couoecd = stanrnd_couoecd,
    stanrnd_counonoecd = stanrnd_counonoecd,
    stanrnd_downloadplotwidth = stanrnd_downloadplotwidth,
    stanrnd_downloadplotheight = stanrnd_downloadplotheight,
    stanrnd_downloadplotformat = stanrnd_downloadplotformat,
    stanrnd_downloadPlot = stanrnd_downloadPlot,
    viz_plot_width = viz_plot_height,
    viz_plot_width = viz_plot_width,
    data.calc = data.calc)
         )
}

summary_stanRnd <- function(result = .stanRnd())
{ if (length(result) > 0) {

}}


tables_stanRnd <- function(result = .stanRnd())
{ if (length(result) > 0) {

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
  stanrnd_showvalues <- result$stanrnd_showvalues

  if (stanrnd_plot == "Points")
    {
      ## aggregate over time period using mean
      data.agg <- aggregate(data.calc[colnames(data.calc)%in%c(stanrnd_var, "RDNC")], by = list(data.calc$cou, data.calc$ind), "mean")
      names(data.agg)[1:2] <- c("cou", "ind")
      data.agg$ratio <- data.agg[, "RDNC"] / data.agg[, stanrnd_var]
      data.agg <- merge(group.ind, data.agg) # group.ind first to use order of factor levels
      p <- ggplot(data.agg, aes(x = ind, y = ratio, label = cou, order = ind)) +
        geom_point() +
          facet_grid(. ~ group.label, scales = "free", space = "free")
      if (stanrnd_error=="minmax")
        {
          p <- p + stat_summary(fun.y = stanrnd_centr, fun.ymin = min, fun.ymax = max, colour='red')
        }
      if (stanrnd_error=="sderr")
        {
          p <- p + stat_summary(fun.y = stanrnd_centr, fun.ymin = lowsd, fun.ymax = highsd, colour='red')
        }
      if (stanrnd_coulabel==TRUE)
        {
          p <- p + geom_text(size = stanrnd_coulabel.size)
        }

    } else if (stanrnd_plot == "Lines")
      {
        data.plot <- data.calc[data.calc$ind%in%stanrnd_ind,]
        data.plot$ratio<- data.plot[, "RDNC"] / data.plot[, stanrnd_var]
        data.plot <- merge(group.ind, data.plot)
        ## data.plot.m <- melt(data.plot, id.vars=c("ind", "cou", "cur", "year", "type", "group.no", "group.label"), variable.name="var")
        data.plot.m <- melt(data.plot, id.vars=c("ind", "cou", "cur", "year", "group.no", "group.label"), variable.name="var")
        p1 <- ggplot(data.plot.m[data.plot.m$var%in%c("RDNC", stanrnd_var),], aes(x = year, y = value, group = cou, label = cou)) +
          geom_line(aes(colour = cou)) +
            guides(colour = FALSE) +
              xlab(NULL) +
                ggtitle(paste("Values"))
        if (stanrnd_scalelogs==TRUE)
          {
            p1 <- p1 + scale_y_log10(name = "Value in logs, free scales") +
              facet_grid(var ~ ., space = "free", scales = "free")
          } else {
            p1 <- p1 + ylab(NULL) +
              facet_grid(var ~ ., space = "fixed", scales = "fixed")
          }
        ##
        p2 <- ggplot(data.plot.m[data.plot.m$var%in%c("ratio"),], aes(x = year, y = value, label = cou)) +
          geom_line(aes(colour = cou)) +
            geom_smooth(method = "loess") +
              xlab(NULL) +
                ylab(NULL) +
                  ggtitle(paste("Ratio: RDNC per", stanrnd_var))
        if (stanrnd_coulabel==TRUE)
          {
            p1 <- p1 + geom_text(size = stanrnd_coulabel.size)
            p2 <- p2 + geom_text(size = stanrnd_coulabel.size)
          }
        if (stanrnd_showvalues==TRUE)
          {
            p <- arrangeGrob(p1, p2, ncol = 2)
          } else {
            p <- p2
          }
        
      }
  
  print(p)
  
}}

    ## ## Tab: Table: Data (display underlying data in table)
    ## output$table_data <- renderTable({
    ##     data.calc <- data.calc()
    ##     data.calc
    ## })

    ## ## ## Tab: Plot: Line
    ## plot_line <- reactive({
    ##     data.calc <- data.calc()

    ##     data.plot <- data.calc[data.calc$ind%in%stanrnd_ind,]
    ##     data.plot$ratio<- data.plot[, "RDNC"] / data.plot[, stanrnd_var]
    ##     data.plot <- merge(group.ind, data.plot)
    ##     data.plot.m <- melt(data.plot, id.vars=c("ind", "cou", "cur", "year", "type", "group.no", "group.label"), variable.name="var")
    ##     p1 <- ggplot(data.plot.m[data.plot.m$var%in%c("RDNC", stanrnd_var),], aes(x = year, y = value, group = cou, label = cou)) +
    ##         geom_line(aes(colour = cou)) +
    ##             guides(colour = FALSE) +
    ##                 xlab(NULL) +
    ##                 ggtitle(paste("Values"))
    ##     if (stanrnd_scalelogs==TRUE)
    ##     {
    ##         p1 <- p1 + scale_y_log10(name = "Value in logs, free scales") +
    ##             facet_grid(var ~ ., space = "free", scales = "free")
    ##     } else {
    ##         p1 <- p1 + ylab(NULL) +
    ##             facet_grid(var ~ ., space = "fixed", scales = "fixed")
    ##     }
    ##     ##
    ##     p2 <- ggplot(data.plot.m[data.plot.m$var%in%c("ratio"),], aes(x = year, y = value, label = cou)) +
    ##         geom_line(aes(colour = cou)) +
    ##             geom_smooth(method = "loess") +
    ##                 xlab(NULL) +
    ##                     ylab(NULL) +
    ##                         ggtitle(paste("Ratio: RDNC per", stanrnd_var))
    ##     if (stanrnd_coulabel==TRUE)
    ##     {
    ##         p1 <- p1 + geom_text(size = stanrnd_coulabel.size)
    ##         p2 <- p2 + geom_text(size = stanrnd_coulabel.size)
    ##     }
    ##     if (stanrnd_showvalues==TRUE)
    ##     {
    ##         p <- arrangeGrob(p1, p2, ncol = 2)
    ##     } else {
    ##         p <- p2
    ##     }
    ##     p
    ## })

    ## output$plot_line <- renderPlot({
    ##     p <- plot_line()

    ##     print(p)

    ## })

    ## output$downloadPlot <- downloadHandler(

    ##     filename = function() { paste0('plot.', tolower(stanrnd_downloadplotformat)) },
    ##     content = function(file) {
    ##         if (stanrnd_downloadplotformat=="PDF") pdf(file=file, width=stanrnd_downloadplotwidth, height=stanrnd_downloadplotheight)
    ##         if (stanrnd_downloadplotformat=="PNG") png(file=file, width=stanrnd_downloadplotwidth, height=stanrnd_downloadplotheight, units = "in", res = 300)
    ##         if (stanrnd_conditionedPanels==1) print(plot_dot())
    ##         if (stanrnd_conditionedPanels==2) print(plot_line())
    ##         dev.off()
    ##     })
