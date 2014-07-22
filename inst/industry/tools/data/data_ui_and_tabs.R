
output$data_ui_and_tabs <- renderUI({

      doLogin()
      if (loginData$LoggedIn) {
          doLogout()

    list(
        includeCSS("www/style.css"),
        ## includeMathJax("www/js/MathJax.js"),
        tags$head(
            tags$script(src = "js/jquery-ui.custom.min.js"),
            tags$script(src = "js/busy.js"),
            ## tags$script(src = "js/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
            tags$script(src = 'https://c328740.ssl.cf1.rackcdn.com/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML',
                        type = 'text/javascript')
            ),

        sidebarLayout(
            sidebarPanel(
                div(class = "busy",
                    p("Calculation in progress ..."),
                    img(src="ajaxloaderq.gif")
                    ),

                ## wellPanel(
                uiOutput("uiDatasets"), # manage.R
                ## ),
                ## include "Manage" to prepare modules like "analysis_skeleton.R"
                conditionalPanel(condition = "input.datatabs == 'Manage'",
                                 uiOutput("ui_Manage") # manage.R
                                 )


                ),
            mainPanel(id = "datatabs",
                      uiOutput("tabs_data")
                      )
            ) # list(...
        )

    } else {
        sidebarLayout(
            sidebarPanel = NULL,
            mainPanel(
                navlistPanel(
                    "Disclaimer",
                    tabPanel("Enter Credentials", loginUI()) # mainPanel((h5("Login"), loginUI()))
                    ),
                width = 15
                )
            )
    }

})

output$tabs_data <- renderUI({
    tabsetPanel(id = "datatabs",

                ## tabPanel("Manage", htmlOutput("htmlDataExample"), # tools/data/manage.R l.258
                tabPanel("Manage", verbatimTextOutput("htmlDataExample") # tools/data/manage.R l.258
                         ## ,
                         ## HTML('<label>10 (max) rows shown. See View-tab for details.</label>'),
                         ## checkboxInput("man_add_descr","Add/edit data description", FALSE)
                         ## conditionalPanel(condition = "input.man_add_descr == false",
                         ##                  HTML(dataDescriptionOutput('html'))
                         ##                  ),
                         ## conditionalPanel(condition = "input.man_add_descr == true",
                         ##                  HTML("<label>Add data description:</label>"),
                         ##                  tags$textarea(id="man_data_descr", rows="10", cols="12", dataDescriptionOutput('md'))
                         ##                  )
                         )
                ## ,
                ## tabPanel("Plot: Bar: Ind",
                ##          plotOutput("plot_bar_ind", height=500),
                ##          verbatimTextOutput("blurb_ind"),
                ##          plotOutput("image_conv1", height=700),
                ##          verbatimTextOutput("label_cou"),
                ##          value=3),
                ## tabPanel("Table: Data: Ind", tableOutput("table_data_ind"), value=4),
                ## tabPanel("Plot: Bar: Cou",
                ##          plotOutput("plot_bar_cou", height=500),
                ##          verbatimTextOutput("blurb_cou"),
                ##          ## plotOutput("image_conv1", height=700),
                ##          ## verbatimTextOutput("label_cou"),
                ##          value=5),
                ## tabPanel("Table: Data: Cou", tableOutput("table_data_cou"), value=6),
                ## tabPanel("Plot: Map: Cou", showOutput("rchart_map_cou", "datamaps"), value=5)

                )
})
