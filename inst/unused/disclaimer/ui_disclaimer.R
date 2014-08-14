#######################################
## Shiny interface for data functions
#######################################

output$ui_disclaimer <- renderUI({

    ## list(
        ## includeCSS("www/style.css"),
        ## ## includeMathJax("www/js/MathJax.js"),
        ## tags$head(
        ##     tags$script(src = "js/jquery-ui.custom.min.js"),
        ##     tags$script(src = "js/busy.js"),
        ##     ## tags$script(src = "js/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
        ##     ## tags$script(src = 'https://c328740.ssl.cf1.rackcdn.com/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML',
        ##     ##             type = 'text/javascript')
        ##     tags$script(src = 'js/MathJax.js?config=TeX-AMS-MML_HTMLorMML')
        ##     ),

        sidebarLayout(
            sidebarPanel = NULL,
            mainPanel(uiOutput("tabs_disclaimer"),
                      width = 15
                      )
            )

})

output$tabs_disclaimer <- renderUI({

    ## doLogin()
    ## if (loginData$LoggedIn) {
    ##     doLogout()

    navlistPanel(
        "Disclaimer",
        tabPanel("General Information", htmlOutput("html_disclaimer"))
        ,tabPanel("Territories", htmlOutput("html_territories"))
        ,tabPanel("Israel", htmlOutput("html_israel"))
        )

    ## } else
    ## {
    ##     navlistPanel(
    ##         "Disclaimer",
    ##         tabPanel("General Information", loginUI(), htmlOutput("html_disclaimer"))
    ##         ,tabPanel("Territories", htmlOutput("html_territories"))
    ##         ,tabPanel("Israel", htmlOutput("html_israel"))
    ##         )
    ## }

})
