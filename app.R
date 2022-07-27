# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file
options(shiny.maxRequestSize = 40*1024^2)
#devtools::install_github("accsensors/astr", ref = "public_shiny", upgrade = "always")
pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)
shinyAST::run_app() # add parameters here (if any)

