detach_package <- function(pkg,installed, character.only = FALSE)
{

  try({
    is_loaded <- pkg %in% loadedNamespaces()
    if(is_loaded){
      search_item <- paste("package", pkg, sep = ":")
      detach(search_item, unload = TRUE, character.only = TRUE)
    }
  },silent =T)



}

install_imesc<-function(lib=.libPaths(),repo="https://cran.rstudio.com/"){
  installed<-installed.packages(lib.loc=lib)[,"Package"]
  packs<-c('shiny','plotly','aweSOM','ggraph','ggpubr','ggforce','mice','caret','leaflet','GGally','ggplot2','sf','dplyr','igraph','readr','gstat','factoextra','partykit','ade4','scales','party','randomForestExplainer','sortable','ggparty','automap','scatterpie','tibble','colourpicker','DT','pdp','shinyWidgets','e1071','sp','klaR','shinydashboardPlus','shinyTree','htmlwidgets','NCmisc','ggrepel','dendextend','shinycssloaders','readxl','vegan','plot3D','httr','gridExtra','gplots','NeuralNetTools','shinybusy','shinydashboard','raster','colorspace','kernlab','ggridges','shinyjs','reshape2','webshot','viridis','shinyBS','leaflet.minicharts','beepr','data.table','segRDA','indicspecies','permute','kohonen','ggnewscale','randomForest','reshape','jsonlite','gbRd','plyr','base64enc','RColorBrewer','wesanderson','corrplot','writexl','geodist','Metrics', "pak",'progress','rstudioapi','colorRamps','waiter')

  if(!all(packs %in% installed)){
    temp<-sapply( packs[!packs %in% installed],function(x) detach_package(x,installed))
    if(!"pak"%in%installed){
      install.packages("pak",verbose=F,quiet=T,lib=lib, repo=repo)
      message("pak installed")
    }
    message("Installing...")
    if(!"progress"%in%installed){
      install.packages("progress",verbose=F,quiet=T, lib=lib, repo=repo)
      message("progress installed")
    }
    if(!"rstudioapi"%in%installed){install.packages("rstudioapi",verbose=F, lib=lib, repo=repo)}
    to_install<-packs[!packs %in% installed]
    if(!all(packs %in% installed)){
      pak::pkg_install(to_install,ask=F, lib=lib[1])
      pak::pak_cleanup(force=T)
    }
  }

}
