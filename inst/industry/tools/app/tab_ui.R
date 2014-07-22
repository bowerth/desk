## output$ui_plot_options <-
##     isolate(
##         renderUI({
##             list(
##                 sliderInput(inputId = "viz_plot_height", label = "Height:", min = 400, max = 1000, value = 750, step = 50),
##                 sliderInput(inputId = "viz_plot_width", label = "Width:", min = 400, max = 1200, value = 750, step = 50)
##                 )
##         })
##         )
