
command.ui.prefix <- paste('title = "stan-icio indicators",',
                           'header=div(HTML(\'',
                           '<script type="text/javascript" src="js/jquery-ui.custom.min.js"></script>',
                           '<link rel = "stylesheet" href = "style.css"></link>',
                           ## tags$script(src="js/MathJax.js?config=TeX-AMS-MML_HTMLorMML"),
                           '<script type="text/javascript" src="js/busy.js"></script>',
                           '\')),',
                           'footer = div(HTML(\'<center><a href="http://www.oecd.org"><img src="OECD_grey.png", width = 80></a></center>\'), br()),',
                           'id = "nav_radiant",',
                           'inverse = TRUE,',
                           'collapsable = TRUE,',
                           'tabPanel("Disclaimer", includeRmd("tools/app/disclaimer.Rmd"), icon = icon("globe")),\n',
                           ## 'tabPanel("Disclaimer", uiOutput("ui_disclaimer"), icon = icon("globe")),\n',
                           sep = '\n')
## cat(command.ui.prefix)

command.ui.suffix <- paste('tabPanel("About", includeRmd("tools/app/about.Rmd"))')

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
