#shiny::runGitHub("imesc","DaniloCVieira","main")
#source("D:\\R3\\imesc2024\\WRITE_LOGS.R")
"D:\\R3\\imesc2024\\app_new\\inst\\www\\dev\\copy_files.R"
#source("D:\\R3\\imesc2024\\app_new\\inst\\www\\dev\\copy_files.R")
message("Loading iMESc")
message("Please wait")
source("inst/R/install.R")
install_imesc()

## loading packages function
load_packs<-function(){

  #saveRDS(time000,'inst/www/time000.rds')
  base_packs<-c("shiny",'colorRamps','data.table','shinyBS','colorspace','shinyWidgets','shinyjs')
  sapply(seq_along(base_packs),function(i){
    suppressPackageStartupMessages(library(base_packs[[i]],character.only = T))
  })

}
## loading Modules
load_R<-function(){
  #if(!exists("data_migrate"))
  if(TRUE){
    www_funs<-list.files("inst/R",pattern=".R",full.names = T)
    sapply(seq_along(www_funs),function(i){
      source(www_funs[[i]])
    })

  }

}



loadedP<-load_packs()
loadedR<-load_R()
# run app
shiny::shinyApp(app_ui,app_server)


