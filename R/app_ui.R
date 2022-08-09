#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    #shinythemes::themeSelector(),

    # Your application UI logic
    fluidPage(theme = shinythemes::shinytheme("spacelab"),

              #--- PAGE HEADER ---#
              titlePanel(
                title=p(tags$a(href='https://www.accsensors.com/', tags$img(src="www/AST.png", height = "20%", width = "20%"),
                               target="_blank"),
                        column(1, offset=10,
                               style = "position: absolute;
                          top: 2%;
                          right: 7%;",
                               # FEEDBACK BUTTON
                               actionButton("button", "Provide Feedback",
                                            onclick ="window.open('https://forms.office.com/r/id8y0B7THV', '_blank')",
                                            width = "200px",
                                            style = "background-color:#081B33;
                      color:#FFFFFF;
                      border-color:#000000;
                      border-style:solid;
                      border-width:2px;
                      border-radius:5%;
                      font-size:16px"
                               ),
                               fluidRow(style = "padding:2px"),
                               # SUBMIT GITHUB ISSUE BUTTON
                               actionButton("button", "Submit Github Issue",
                                            onclick ="window.open('https://github.com/accsensors/shinyAST/issues/new', '_blank')",
                                            width = "200px",
                                            style = "background-color:#081B33;
                      color:#FFFFFF;
                      border-color:#000000;
                      border-style:solid;
                      border-width:2px;
                      border-radius:5%;
                      font-size:16px"
                               ),
                               # CONTACT EMAIL
                               fluidRow(style = "padding:4px"),
                               p("contact@accsensors.com",
                                 style = "font-size:16px" )
                        )
                )
              ),

              #--- DIRECTIONS FOR APP USE ---#
              wellPanel(
                h4("Directions:"),
                h5("1. Upload one or more files up to 40MB to generate summary tables"),
                h5("2. Configure the plot settings below"),
                h5("3. Click 'Generate Plot'"),
                h5("4. To zoom in on a plot, click and drag along an area of the left-side plot then double click inside the zoom box"),
                h5("5. The zoomed plot will show up on the right side"),
                h5("6. Double click again on either plot to reset the right-side plot"),
                h5("6. Click 'Save Plot' to save the right-side plot")
              ),

              #--- UPLOAD LOG FILES ---#
              fileInput("upload", NULL, buttonLabel = "Upload...",
                        accept = c(".csv",".txt"), multiple = TRUE),

              #--- HEADER FILE SUMMARY TABLES ---#
              tabsetPanel(
                tabPanel("Sample Summary", DT::dataTableOutput("logsummary")),
                tabPanel("Sample Settings",DT::dataTableOutput("logsettings")),
                tabPanel("UPAS Operation",DT::dataTableOutput("logmeta"))
              ),

              fluidRow(style = "padding:10px"),

              #--- PLOT SETTINGS AND VARIABLE SELECTION ---#
              fluidRow(
                column(4,
                       wellPanel(fluidRow(
                         h4("Plot Settings"),
                         column(6,
                                selectInput("x_axis", "X Variable", choices = NULL),
                                selectInput("y_axis", "Y Variable", choices = NULL),
                         ),
                         column(6,
                                selectInput("geom", "Line Type", c("point", "line")),
                                #selectInput("plottype", "Toggle Interactive Plot", c("Standard", "Interactive")),
                                selectInput(
                                  "unitformat",
                                  "Unit Format",
                                  c(
                                    "Fractional (L/min)" = "TRUE",
                                    "Log File (L min^-1)" = "FALSE"
                                  )
                                ),
                                # GENERATE PLOT BUTTON
                                actionButton(
                                  "applyButton",
                                  "Generate Plot",
                                  width = "200px",
                                  style = "background-color:#081B33;
                      color:#FFFFFF;
                      border-color:#000000;
                      border-style:solid;
                      border-width:2px;
                      border-radius:5%;
                      font-size:18px"
                                )
                         )
                       ))),

                #--- DOWNLOADABLE PLOT SETTINGS ---#
                column(4,
                       wellPanel(
                         h4("Download Plot"),
                         textInput(
                           inputId = "plotname",
                           value = "UPAS_Plot",
                           label = "Plot Name"
                         ),
                         fluidRow(
                           column(3,
                                  numericInput(
                                    inputId = "plotwidth",
                                    label = "Width (px)",
                                    value = "5000"
                                  )
                           ),
                           column(3,
                                  numericInput(
                                    inputId = "plotheight",
                                    label = "Height (px)",
                                    value = "2500"
                                  )
                           ),
                           column(3,
                                  numericInput(
                                    inputId = "plotfont",
                                    value = 18,
                                    min = 10,
                                    max = 30,
                                    label = "Font Size"
                                  )
                           )
                         ),
                         radioButtons(
                           "extension",
                           "Select File Type:",
                           choices = c("png", "pdf", "svg", "jpg"),
                           inline = TRUE
                         ),
                         downloadButton("download", "Save Plot")
                       )
                ),

                #--- DISPLAY PLOT VALUES NEAR CLICK ---#
                column(width = 4,
                       h4("Click the right-side plot below to display a table of values"),
                       DT::dataTableOutput("click_info")
                )
              ),

              #--- LOG PLOTS ---#
              # LEFT-SIDE PLOT
              fluidRow(
                column(width=5,
                       plotOutput("plot",
                                  dblclick = "plot_dblclick",
                                  brush = brushOpts(
                                    id = "plot_brush",
                                    resetOnNew = TRUE
                                  )
                       )
                ),
                # RIGHT-SIDE PLOT FOR ZOOMED VIEW AND POINT SELECTION
                column(width=7,
                       plotOutput("plot_zoom",
                                  dblclick = "plot_dblclick",
                                  click = "plot_click",
                                  brush = brushOpts(
                                    id = "plot_brush",
                                    resetOnNew = TRUE
                                  )
                       )

                )
              )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "shinyAST"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
