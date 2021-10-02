
iMESc<-function(...){


  need<-c('shinydashboard','shinydashboardPlus','shinyjs','shiny',"e1071",'readxl','vegan',"party",'caret','viridisLite','aweSOM','sp','raster','rasterVis','Rcpp','rgdal','gstat','ggspatial','ggplot2','sf','class','shinyWidgets', 'randomForestExplainer','data.table',"ggpubr", "shinyBS","terra","purrr","NbClust", "colorRamps","DBI","shinyBS","wesanderson","colorspace","gplots","dendextend","kohonen","shinypanels","writexl")

  installed<-rownames(installed.packages())
  if(any(!need%in%installed)){
    instals<-need[!need%in%rownames(installed.packages())]
    for(i in 1:length(instals)){
      install.packages(instals[i])
    }}

  {
    library(colorspace)
    library(shinypanels)
    library(wesanderson)
    library(e1071)
    library("colorRamps")
    library(shinydashboard)
    library(shinydashboardPlus)
    library(shinyjs)
    library(shiny)
    library(readxl)
    library(writexl)
    library(vegan)
    library(caret)
    library(viridisLite)
    library(aweSOM)
    library(sp)
    library(raster)
    library(rasterVis)
    library(rgdal)
    library(gstat)
    library(ggspatial)
    library(ggplot2)
    library(sf)
    library(NbClust)
    library(class)
    library(shinyWidgets)
    library(randomForestExplainer)
    library(data.table)

    library("party")
    library("kohonen")
    source("meta/funs_pre_treat.R")
    source("meta/funs_sominter.R")
    source("meta/funs_somplot.R")
    source("meta/funs_mapplot.R")
    source("meta/funs_rf_inter.R")
    library(shinyBS)
    source('meta/funs_texts.R')
    library(purrr)
    library(gplots)
    library("dendextend")
    source('app.R')}
  shinyApp(ui, server)
}
