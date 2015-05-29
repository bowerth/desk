
output$ui_temPlate <- renderUI({

    list(

        ## helpAndReport("ICIO Networks", "template", inclMD(file.path("tools", "help", "temPlate.md")))
    )

})

temPlate_widthSize <- reactive({
    ifelse(is.null(input$temPlate_viz_plot_width), return(values$plotWidth), return(input$temPlate_viz_plot_width))
})
temPlate_heightSize <- reactive({
    ifelse(is.null(input$temPlate_viz_plot_height), return(values$plotHeight), return(input$temPlate_viz_plot_height))
})

output$temPlate <- renderUI({
  statTabPanel(menu_name = "ICIO",
               fun_name = "ICIO Networks",
               rfun_label = ".temPlate",
               fun_label = "temPlate",
               fun_tabs = c("Plots"),
               widthFun = "temPlate_widthSize",
               heightFun = "temPlate_heightSize"
               )
})


.temPlate <- reactive({

    temPlate(
        temPlate_viz_plot_width = input$temPlate_viz_plot_width,
        temPlate_viz_plot_height = input$temPlate_viz_plot_height
        )
})


temPlate <- function(
    temPlate_viz_plot_width = temPlate_viz_plot_width,
    temPlate_viz_plot_height = temPlate_viz_plot_height
    ) {

    return(
        list(
            temPlate_viz_plot_width = temPlate_viz_plot_width,
            temPlate_viz_plot_height = temPlate_viz_plot_height
        )
    )
}

summary_temPlate <- function(result = .temPlate()) {
    if (length(result) > 0) {

        list.print <- NULL

        return(list.print)
    }
}

plots_temPlate <- function(result = .temPlate()) {
    if (length(result) > 0) {

    }
}
