# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file
options(shiny.maxRequestSize = 40*1024^2)
devtools::install_github("accsensors/astr", ref = "1c74022b9ea229896f5232520a05c7cfb9a40957", upgrade = "always")
pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)
publicShinyAST::run_app() # add parameters here (if any)

