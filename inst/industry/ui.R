









shinyUI(
    navbarPage(
        title = "STI/EAS Indicators",
        footer = div(HTML('<center><a href="http://www.oecd.org"><img src="OECD_grey.png", width = 80></a></center>'), br()),
        id = "nav_radiant", inverse = TRUE, collapsable = TRUE,

        tabPanel("Disclaimer", uiOutput("ui_disclaimer"), icon = icon("globe")),

        navbarMenu("ICIO",
                   tabPanel("Foreign Demand Domestic Value Added", uiOutput("icioFddva"))
                   )
        ,

        ## navbarMenu("STAN",
        ##            tabPanel("STAN Indicators", uiOutput("stanIndic")),
        ##            tabPanel("R&D Intensity", uiOutput("stanRnd"))
        ##            )
        ## ,

        navbarMenu("STANi3",
                   tabPanel("STAN ISIC3 Estimate", uiOutput("stani3Estimate"))
                   )
        ,

        navbarMenu("SKILL",
                   tabPanel("LFS Share", uiOutput("lfsShare"))
                   )
        ,

        ## navbarMenu("R",
        ##            tabPanel("Report", uiOutput("report")), # app/report.R l.36
        ##            tabPanel("Code", uiOutput("rcode")) # app/report.R l.174
        ##            ),

        ## tabPanel("State", uiOutput("state")), # app/state.R l.27

        ## tabPanel("About", includeRmd("../base/tools/app/about.Rmd"))
        tabPanel("About", includeRmd("tools/app/about.Rmd"))
        ))
