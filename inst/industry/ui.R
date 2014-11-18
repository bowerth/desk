
command.ui.prefix <- paste('title = "stan-icio indicators",',
                           ## 'withMathJax(),',
                           'header=div(HTML(\'',
                           '<link rel="stylesheet" href="style.css"></link>',
                           '<script type="text/javascript" src="js/jquery-ui.custom.min.js"></script>',
                           ## '<script type="text/javascript" src = "https://c328740.ssl.cf1.rackcdn.com/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"></script>',
                           ## '<script type="text/javascript" src="js/mathscribe/jqmath-0.4.0.min.js"></script>',
                           '<script type="text/javascript" src="js/busy.js"></script>',
                           ## '<script type="text/javascript" src="js/d3.v3.min.js"></script>',
                           ## d3Network:
                           '<script type="text/javascript" src="http://d3js.org/d3.v3.min.js"></script>',
                           ## ggvis:
                           '<script type="text/javascript" src="ggvis/jquery.min.js"></script>',
                           '<script type="text/javascript" src="ggvis/jquery-ui-1.10.4.custom.min.js"></script>',
                           '<script type="text/javascript" src="ggvis/d3.min.js"></script>',
                           '<script type="text/javascript" src="ggvis/vega.min.js"></script>',
                           '<script type="text/javascript" src="ggvis/lodash.min.js"></script>',
                           '<script> var lodash = _.noConflict();</script>',
                           '<script type="text/javascript" src="ggvis/ggvis.js"></script>',
                           '<link rel="stylesheet" href="ggvis/jquery-ui-1.10.4.custom.min.css" />',
                           '<link rel="stylesheet" href="ggvis/ggvis.css" />',
                           ## end ggvis
                           '\')),',
                           'footer = div(HTML(\'<center><a href="http://www.oecd.org"><img src="OECD_grey.png", width = 80></a></center>\'), br()),',
                           'id = "nav_radiant",',
                           'inverse = TRUE,',
                           'collapsable = TRUE,'
                           ,
                           ## 'tabPanel("Disclaimer", includeRmd("tools/app/disclaimer.Rmd"), icon = icon("globe")),\n',
                           ## 'tabPanel("Disclaimer", uiOutput("ui_disclaimer"), icon = icon("globe")),\n',
                           sep = '\n')
## cat(command.ui.prefix)

if(app.menu$disclaimer) command.ui.prefix <- paste(command.ui.prefix,
                                                   'tabPanel("Disclaimer", includeRmd("tools/app/disclaimer.Rmd"), icon = icon("globe")),\n',
                                                   sep = '\n')

if(app.menu$about) command.ui.suffix <- paste(## 'tabPanel("Report", uiOutput("report")),\n',
                                                 'tabPanel("About", includeRmd("tools/app/about.Rmd"))',
                                                 sep = '\n')
## cat(command.ui.suffix)

shinyUI(
    eval(parse(text = paste0('navbarPage(', command.ui.prefix, command.ui, command.ui.suffix, ')')))
    )
    ## navbarPage(
        ## ## navbarMenu("ICIO",
        ## ##            tabPanel("Foreign Demand Domestic Value Added", uiOutput("icioFddva"))
        ## ##            )
        ## ## ,
        ## ## navbarMenu("STAN",
        ## ##            tabPanel("STAN Indicators", uiOutput("stanIndic")),
        ## ##            tabPanel("R&D Intensity", uiOutput("stanRnd"))
        ## ##            )
        ## ## ,
        ## ## navbarMenu("STANi3",
        ## ##            tabPanel("STAN ISIC3 Estimate", uiOutput("stani3Estimate"))
        ## ##            )
        ## ## ,
        ## ## navbarMenu("SKILL",
        ## ##            tabPanel("LFS Share", uiOutput("lfsShare"))
        ## ##            )
        ## ## ,
        ## ## navbarMenu("SMDX",
        ## ##            tabPanel("SDMX Browser", uiOutput("sdmxBrowser"))
        ## ##            )
        ## ## ,
        ## ## navbarMenu("R",
        ## ##            tabPanel("Report", uiOutput("report")), # app/report.R l.36
        ## ##            tabPanel("Code", uiOutput("rcode")) # app/report.R l.174
        ## ##            ),
     ## )
