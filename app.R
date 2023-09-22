list.of.packages <- c('remotes')
if(!length(grep("connect/apps",getwd()))>0){
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) {install.packages(new.packages, dependencies = TRUE)}
  remotes::install_deps(upgrade="never")
  pkgload::load_all(export_all = FALSE,quiet =T,warn_conflicts =F)
  options(shiny.autoload.r=FALSE)
  imesc::run_app(options=list(quiet=T,shiny.autoload.r=FALSE, 
                              #,launch.browser=T
  ))
} else{
  source("inst/app/www/global.R")
  source("R/app_ui.R")
  source("R/app_server.R")
  pkgload::load_all(export_all = FALSE,quiet =T,warn_conflicts =F)
  shiny::shinyApp(imesc:::app_ui, imesc:::app_server)
}
