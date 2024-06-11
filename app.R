


list.of.packages <- c('remotes')
if(!length(grep("connect/apps",getwd()))>0){
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) {install.packages(new.packages, dependencies = TRUE)}
  remotes::install_deps("inst",upgrade="never")
}
suppressPackageStartupMessages(pkgload::load_all("inst",export_all = T,quiet =T,warn_conflicts =F))

#shiny::runGitHub("iMESc_beta","DaniloCVieira","main")
shinyApp(app_ui,app_server)



