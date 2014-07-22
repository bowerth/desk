#######################################
## Shiny interface for data functions
#######################################
## data ui and tabs
output$data_ui_and_tabs <- renderUI({
  ## for input-output
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
        ## based on https://groups.google.com/forum/?fromgroups=#!topic/shiny-discuss/PzlSAmAxxwo
        div(class = "busy",
            p("Calculation in progress ..."),
            img(src="ajaxloaderq.gif")
            ),
        wellPanel(
          uiOutput("uiDatasets")
          ),
        conditionalPanel(condition = "input.datatabs == 'Manage'",
                         uiOutput("ui_Manage")
                         ),
        conditionalPanel(condition = "input.datatabs == 'View'",
                         uiOutput("ui_View")
                         ),
        conditionalPanel(condition = "input.datatabs == 'Visualize'",
                         uiOutput("ui_Visualize")
                         ),
        conditionalPanel(condition = "input.datatabs == 'Explore'",
                         uiOutput("ui_Explore")
                         ),
        conditionalPanel(condition = "input.datatabs == 'Merge'",
                         uiOutput("ui_Merge")
                         ),
        conditionalPanel(condition = "input.datatabs == 'Transform'",
                         uiOutput("ui_Transform")
                         )
        ),
      mainPanel(id = "datatabs",
                uiOutput("tabs_data")
                )
      )
    )
})

## data tabs
output$tabs_data <- renderUI({
  tabsetPanel(id = "datatabs",
              tabPanel("Manage", htmlOutput("htmlDataExample"), # tools/data/manage.R l.258
                       HTML('<label>10 (max) rows shown. See View-tab for details.</label>'),
                       ## checkboxInput("man_add_descr","Add/edit data description", FALSE)
                       conditionalPanel(condition = "input.man_add_descr == false",
                                        HTML(dataDescriptionOutput('html'))
                                        ),
                       conditionalPanel(condition = "input.man_add_descr == true",
                                        HTML("<label>Add data description:</label>"),
                                        tags$textarea(id="man_data_descr", rows="10", cols="12", dataDescriptionOutput('md'))
                                        )
                       ),
              tabPanel("View", dataTableOutput("dataviewer")), # tools/data/view.R l.20
              tabPanel("Visualize",
                       conditionalPanel(condition="!$('html').hasClass('shiny-busy')",
                                        plotOutput("visualize", width = "100%", height = "100%")  # tools/data/visualize.R l.98
                                        )
                       ),
              tabPanel("Explore",
                       conditionalPanel(condition="!$('html').hasClass('shiny-busy')",
                                        verbatimTextOutput("expl_summary"),
                                        plotOutput("expl_plots", width = "100%", height = "100%") # tools/data/explore.R l.177
                                        )
                       ),
              tabPanel("Merge", htmlOutput("mergePossible"), htmlOutput("mergeData1"), htmlOutput("mergeData2")), # tools/data/merge.R l.94 and l.110
              tabPanel("Transform", htmlOutput("transform_data"), verbatimTextOutput("transform_summary")) # tools/data/transform.R l.274

              )
})
