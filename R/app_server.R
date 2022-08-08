#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import dplyr
#' @import ggplot2
#' @import plotly
#' @import purrr
#' @import shiny
#'
#' @noRd
app_server <- function(input, output, session) {
  # output$files <- renderTable(input$upload)

  data_log <- reactive({
    req(input$upload)

    input$upload$datapath[grep('*.*LOG.*\\.txt$',input$upload$name)]%>%
      purrr::map(astr::read_ast_log, shiny=TRUE) %>%
      dplyr::bind_rows()
  })

  data_header <- reactive({
    req(input$upload)

    input$upload$datapath[grep('*.*LOG.*\\.txt$',input$upload$name)]%>%
      purrr::map(astr::read_ast_header, shiny=TRUE) %>%
      dplyr::bind_rows()

  })


  output$logsummary <- renderTable({
    astr::upasv2x_sample_summary(data_header())
  })

  output$logmeta <- renderTable({
    astr::upasv2x_sample_meta(data_header())
  })

  output$logsettings <- renderTable({
    astr::upasv2x_sample_settings(data_header())
  })

  plot_geom <- reactive({
    switch(input$geom,
           point = geom_point(),
           line = geom_line()
    )
  })

  basicPlot <- reactive({
    ggplot(data_log(), aes(.data[[input$x_axis]], .data[[input$y_axis]],
                           color = as.factor(.data$UPASserial))) +
      plot_geom() +
      scale_color_manual("UPAS Serial",values = astr::jvcp) +
      theme_bw() +
      theme(legend.position = 'right')
  })

  observeEvent(data_log(), {
    freezeReactiveValue(input, "x_axis")
    freezeReactiveValue(input, "y_axis")
    updateSelectInput(inputId = "x_axis", choices = names(data_log()),
                      selected = 'DateTimeUTC')
    updateSelectInput(inputId = "y_axis", choices = names(data_log()),
                      selected = 'PumpingFlowRate')
  })

  #Make a plot and plotly plot.
  #TODO Down the line it will be good to make
  #this conditional but this portion doesn't seem to slow the app down
  #significantly since outputting these plots in the ui is conditional
  output$plot <- renderPlot({
    basicPlot()
  })

  output$plot_ly <- renderPlotly({
    ggplotly(
      basicPlot(),
      dynamicTicks = TRUE
    )
  })

  output$download <- downloadHandler(
    filename = function() {
      paste(input$plotname, input$extension, sep = ".")
    },
    content = function(file) {
      ggsave(file, basicPlot(), device = input$extension,
             width = 5000, height = 2500, units = "px")
    }
  )

}
