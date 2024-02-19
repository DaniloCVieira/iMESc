library(shiny)
install_time<-readRDS("install_time.rds")
load_time<-readRDS("load_time2.rds")
server_time<-readRDS("server_time.rds")
observeEvent(input$imesc_version,{
  install_time<-readRDS("install_time.rds")
  load_time<-readRDS("load_time2.rds")
  server_time<-readRDS("server_time.rds")

  sys_info <- Sys.info()
  r_version <- R.version
  session_info<-sessionInfo()
  desc<-readLines("DESCRIPTION")
  desc_i<-which(grepl(c("Imports|Suggests"),desc))
  Number_of_packages<-length(desc[desc_i[1]:(desc_i[2]-1)])-1
  teste_date=Sys.Date()

  rstudio<- rstudioapi::versionInfo()
  hardware_info <- list("CPU" = benchmarkme::get_cpu(),
                        "RAM" = benchmarkme::get_ram() / (1024^3))
  machine_info<-hardware_info$CPU$model_name
  ram_info<-paste(round(as.numeric(hardware_info$RAM),1),"GB")

  sys_info<-data.frame(Value=c(paste(r_version["major"], r_version["minor"], sep = "."),
                               paste0('RStudio (',as.character(rstudio$version),")"),
                               session_info$platform,
                               session_info$running,
                               machine_info,
                               ram_info,
                               Number_of_packages,
                               paste(round(install_time,3),"secs"),
                               paste(round(load_time,3),"secs"),
                               paste0(round(server_time,3),"secs"),
                               format(teste_date)

  ),row.names = c('R version:', "GUI:",'Platform:', 'Operating System:','CPU Specifications:','Memory RAM', "Number of packages:",'Packages install time:','Packages load time:', 'Server load time:',"Parameters Collection Date:"))


  sys_info2<-lapply(rownames(sys_info),function(x){
    div(style="display: table-row",
        div(style="display: table-cell; padding-right: 5px; text-align: right",strong(x)),
        div(style="display: table-cell;  color: #05668D;",sys_info[x,1]))})

  saveRDS(sys_info2,'inst/www/sys_info.rds')

})
