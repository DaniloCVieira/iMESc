
iMESc<-function(...){


  need<-c('shinydashboard','shinydashboardPlus','shinyjs','shiny',"e1071",'readxl','vegan',"party",'caret','viridisLite','aweSOM','sp','raster','rasterVis','Rcpp','rgdal','gstat','ggspatial','ggplot2','sf','class','shinyWidgets', 'randomForestExplainer','data.table',"ggpubr", "shinyBS","terra","purrr","NbClust", "colorRamps","DBI","shinyBS","wesanderson","colorspace","gplots","dendextend","kohonen","shinypanels","writexl","DT","gbRd")

  installed<-rownames(installed.packages())
  if(any(!need%in%installed)){
    instals<-need[!need%in%rownames(installed.packages())]
    for(i in 1:length(instals)){
      install.packages(instals[i])
    }}

}
