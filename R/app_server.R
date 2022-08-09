#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import dplyr
#' @import ggplot2
#' @import purrr
#' @import shiny
#'
#' @noRd
app_server <- function(input, output, session) {

  #--- READ IN LOG ---#
  data_log <- reactive({
    req(input$upload)

    input$upload$datapath[grep('*.*LOG.*\\.txt$',input$upload$name)]%>%
      purrr::map(astr::read_ast_log, shiny=TRUE) %>%
      dplyr::bind_rows()
  })

  # Filter log file to clean up table displaying interactive plot values
  data_log_filter <- reactive({
    data_log() %>%
      dplyr::select(UPASserial, input$x_axis, input$y_axis)
  })

  #--- READ IN HEADER ---#
  data_header <- reactive({
    req(input$upload)

    input$upload$datapath[grep('*.*LOG.*\\.txt$',input$upload$name)]%>%
      purrr::map(astr::read_ast_header, shiny=TRUE) %>%
      dplyr::bind_rows()

  })

  #--- SUMMARY HEADER TABLES ---#
  # Reactive variable for summary table formatting
  tableOptions <- reactive({
    list(paging=FALSE, searching=FALSE,
         columnDefs= list(list(className='dt-center', targets="_all")
                          #list(visible=FALSE, targets='ASTSampler')
         ))
  })

  # GENERAL UPAS OPERATION META DATA
  output$logmeta <- DT::renderDataTable({
    input$applyButton
    DT::datatable(
      astr::upasv2x_sample_meta(data_header(), shiny=TRUE, fract_units = isolate(input$unitformat)),
      options = tableOptions(),
      rownames=FALSE) %>%
      DT::formatStyle('SampleSuccess',
                      target = 'row',
                      backgroundColor = DT::styleEqual(
                        c('FAIL'), 'lightpink'
                      ))

  })

  # BASIC HEADER SUMMARY DATA
  output$logsummary <- DT::renderDataTable({
    input$applyButton
    DT::datatable(
      astr::upasv2x_sample_summary(data_header(), shiny=TRUE, fract_units = isolate(input$unitformat)),
      options = tableOptions(),
      rownames=FALSE) %>%
      DT::formatStyle('SampleSuccess',
                      target = 'row',
                      backgroundColor = DT::styleEqual(
                        c('FAIL'), 'lightpink'
                      ))
  })

  # SAMPLE SETTINGS DATA
  output$logsettings <- DT::renderDataTable({
    input$applyButton
    DT::datatable(
      astr::upasv2x_sample_settings(data_header(), shiny=TRUE, fract_units = isolate(input$unitformat)),
      options = tableOptions(),
      rownames=FALSE) %>%
      DT::formatStyle('SampleSuccess',
                      target = 'row',
                      backgroundColor = DT::styleEqual(
                        c('FAIL'), 'lightpink'
                      ))
  })

  #--- PLOT FORMATTING ---#
  # Reset x and y axis values when reading in a file
  observeEvent(data_log(), {
    freezeReactiveValue(input, "x_axis")
    freezeReactiveValue(input, "y_axis")
    updateSelectInput(inputId = "x_axis", choices = names(data_log()),
                      selected = 'DateTimeUTC')
    updateSelectInput(inputId = "y_axis", choices = names(data_log()),
                      selected = 'PumpingFlowRate')
  })

  # Format x and y plot labels with units and spaces
  plotx_label <- reactive({
    astr::shiny_axis(input$x_axis, fract_units = input$unitformat)
  })
  ploty_label <- reactive({
    astr::shiny_axis(input$y_axis, fract_units = input$unitformat)
  })

  # Select plot geometry
  plot_geom <- reactive({
    switch(input$geom,
           point = geom_point(),
           line = geom_line()
    )
  })

  #--- PLOTTING ---#

  # SET UP THE STATIC PLOT
  basicPlot <- reactive({
    input$applyButton
    ggplot(data_log(), aes(.data[[isolate(input$x_axis)]], .data[[isolate(input$y_axis)]],
                           color = as.factor(.data$UPASserial))) +
      labs(x = isolate(plotx_label()), y = isolate(ploty_label())) +
      isolate(plot_geom()) +
      scale_color_manual("UPAS Serial",values = astr::jvcp) +
      theme_bw() +
      theme(legend.position = 'none',
            text=element_text(size=isolate(input$plotfont)))
  })

  # SET UP THE ZOOMABLE PLOT
  # Set up dynamic x and y axis ranges
  ranges <- reactiveValues(x = NULL, y = NULL)

  # Set up plot
  basicPlot_zoom <- reactive({
    basicPlot() +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = TRUE) +
      theme(legend.position = 'right')
  })

  # When a double-click happens on the static plot, check if there's a brush on the plot.
  # If so, zoom the zoomable plot to the brush bounds; if not, reset the zoom.
  observeEvent(input$plot_dblclick, {
    brush <- input$plot_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      if(input$x_axis=="DateTimeUTC" | input$x_axis=="DateTimeLocal"){
        ranges$x = as.POSIXct(ranges$x, origin = "1970-01-01")
      }
      if(input$y_axis=="DateTimeUTC" | input$y_axis=="DateTimeLocal"){
        ranges$y = as.POSIXct(ranges$y, origin = "1970-01-01")
      }
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })

  # RENDER THE STATIC AND ZOOMABLE PLOTS
  output$plot <- renderPlot({
    input$applyButton
    basicPlot()
  })
  output$plot_zoom <- renderPlot({
    basicPlot_zoom()
  })

  #--- RENDER DATATABLE FOR DISPLAYING INTERACTIVE PLOT VALUES ---#
  # Reactive variable for click table formatting
  clickTableOptions <- reactive({
    list(paging=FALSE, searching=FALSE,
         columnDefs= list(list(className='dt-center', targets="_all")))
  })

  output$click_info <- DT::renderDataTable({
    req(input$plot_click)
    # SampleTime is formatted to display only 3 decimal places
    if(input$x_axis=="SampleTime" | input$y_axis=="SampleTime"){
      DT::datatable(
        nearPoints(data_log_filter(), input$plot_click, xvar=input$x_axis, yvar=input$y_axis,
                   threshold = 5, maxpoints = 5),
        options = clickTableOptions(),
        rownames=FALSE) %>%
        DT::formatRound(columns="SampleTime", digits=3)
    }
    else{
      DT::datatable(
        nearPoints(data_log_filter(), input$plot_click, xvar=input$x_axis, yvar=input$y_axis,
                   threshold = 5, maxpoints = 5),
        options = clickTableOptions(),
        rownames=FALSE)
    }
  })


  #--- PLOT DOWNLOADING ---#
  output$download <- downloadHandler(
    filename = function() {
      paste(input$plotname, input$extension, sep = ".")
    },
    content = function(file) {
      # Download the zoomable plot
      ggsave(file, basicPlot_zoom(), device = input$extension,
             width = input$plotwidth, height = input$plotheight, units = "px")
    }
  )

}
