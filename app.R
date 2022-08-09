# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file
options(shiny.maxRequestSize = 40*1024^2)
if(!require("golem")){
  install.packages("golem")
}
library(golem)
document_and_reload()
# if(!require("astr")){
#   devtools::install_github("accsensors/astr", ref = "ad9110575e5a236458a9f4146af11b75052924b1", upgrade = "always")
# }
# devtools::install_github("accsensors/astr", upgrade = "always")
pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)
publicShinyAST::run_app() # add parameters here (if any)

