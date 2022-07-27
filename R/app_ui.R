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
    fluidPage(

      # Header
      headerPanel(
        title=p(tags$a(href='https://www.accsensors.com/', tags$img(src="www/AST.png", height = "20%", width = "20%"),
                       target="_blank"),
                column(1,
                  style = "position: absolute;
                          top: 40px;
                          right: 100px;",
                  #TODO Set button to be specific size instead of setting font
                  #TODO Add more space between the buttons
                  actionButton("button", "Provide Feedback",
                               onclick ="window.open('https://forms.office.com/r/id8y0B7THV', '_blank')",
                               width = "200px",
                               style = "background-color:#081B33;
                      color:#FFFFFF;
                      border-color:#000000;
                      border-style:solid;
                      border-width:2px;
                      border-radius:5%;
                      font-size:18px"
                  ),
                  fluidRow(style = "padding:2px"),
                  actionButton("button", "Submit Github Issue",
                               onclick ="window.open('https://github.com/accsensors/shinyAST/issues/new', '_blank')",
                               width = "200px",
                               style = "background-color:#081B33;
                      color:#FFFFFF;
                      border-color:#000000;
                      border-style:solid;
                      border-width:2px;
                      border-radius:5%;
                      font-size:18px"
                  ),
                  fluidRow(style = "padding:4px"),
                  p("contact@accsensors.com",
                    style = "font-size:16px" )
                )
        )
        #title=tags$a(href='https://www.accsensors.com/', "AST.png", target="_blank")
      ),

      #h1("shinyAST"),
      fileInput("upload", NULL, buttonLabel = "Upload...",
                accept = c(".csv",".txt"), multiple = TRUE),
      #tableOutput("files"),

      tabsetPanel(
        tabPanel("Sample Summary",tableOutput("logsummary")),
        tabPanel("Sample Settings",tableOutput("logsettings")),
        tabPanel("UPAS Operation",tableOutput("logmeta"))
      ),

      fluidRow(
        column(2,
               selectInput("x_axis", "X Variable", choices = NULL),
               selectInput("y_axis", "Y Variable", choices = NULL)
               ),
        column(2,
               selectInput("geom", "Line Type", c("point", "line")),
               selectInput("plot_type", "Toggle Interactive Plot", c("Standard", "Interactive"))
        ),
        column(4,
               # wellPanel(
               #   fileInput("file", "Upload CSV", accept = ".csv"),
               #   selectInput("variable", "Select Variable", choices = NULL),
               # ),
               wellPanel(
                 textInput(
                   inputId = "plotname",
                   placeholder = "Enter Plot Name",
                   label = "Plot Name"
                 ),
                 #TODO Add user ability to customize downloaded plot size
                 # fluidRow(
                 #  column(3,
                 #        textInput(
                 #          inputId = "plotwidth",
                 #          label = "Width (px)",
                 #          value = "4000"
                 #        )
                 #  ),
                 #  column(3,
                 #        textInput(
                 #          inputId = "plotheight",
                 #          label = "Height (px)",
                 #          value = "1500"
                 #        )
                 #  )
                 #),
                 radioButtons("extension", "Select File Type:",
                              choices = c("png", "pdf", "svg", "jpg"), inline = TRUE),
                 downloadButton("download", "Save Plot")
               )
        )
      ),

      # selectInput("geom", "geom", c("point", "smooth", "jitter","line")),
      conditionalPanel(
        condition = "input.plot_type == 'Standard'",
        plotOutput("plot")
      ),
      conditionalPanel(
        condition = "input.plot_type == 'Interactive'",
        plotlyOutput("plot_ly")
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
