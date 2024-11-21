library(RANN)


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
  packages<-c(shiny='1.9.1', plotly='4.10.4', aweSOM='1.3', ggraph='2.2.1', ggpubr='0.6.0', ggforce='0.4.2', mice='3.16.0', caret='6.0.94', leaflet='2.2.2', GGally='2.2.1', ggplot2='3.5.1', sf='1.0.19', dplyr='1.1.4', igraph='2.1.1', readr='2.1.5', gstat='2.1.2', factoextra='1.0.7', partykit='1.2.22', ade4='1.7.22', scales='1.3.0', party='1.3.17', randomForestExplainer='0.10.1', sortable='0.5.0', ggparty='1.0.0', automap='1.1.12', scatterpie='0.2.4', tibble='3.2.1', colourpicker='1.3.0', DT='0.33', pdp='0.8.2', shinyWidgets='0.8.7', e1071='1.7.16', sp='2.1.4', klaR='1.7.3', shinydashboardPlus='2.0.5', shinyTree='0.3.1', htmlwidgets='1.6.4', NCmisc='1.2.0', ggrepel='0.9.6', dendextend='1.19.0', shinycssloaders='1.1.0', readxl='1.4.3', vegan='2.6.8', plot3D='1.4.1', httr='1.4.7', gridExtra='2.3', gplots='3.2.0', NeuralNetTools='1.5.3', shinybusy='0.3.3', shinydashboard='0.7.2', raster='3.6.30', colorspace='2.1.1', kernlab='0.9.33', ggridges='0.5.6', shinyjs='2.1.0', reshape2='1.4.4', webshot='0.5.5', viridis='0.6.5', shinyBS='0.61.1', leaflet.minicharts='0.6.2', beepr='2.0', data.table='1.16.2', segRDA='1.0.2', indicspecies='1.7.15', permute='0.9.7', kohonen='3.0.12', ggnewscale='0.5.0', randomForest='4.7.1.2', reshape='0.8.9', jsonlite='1.8.9', gbRd='0.4.12', plyr='1.8.9', base64enc='0.1.3', RColorBrewer='1.1.3', wesanderson='0.3.7', corrplot='0.95', writexl='1.5.1', geodist='0.1.0', Metrics='0.1.4', pak='0.8.0', progress='1.2.3', rstudioapi='0.17.1', colorRamps='2.3.4', waiter='0.2.5', gbm='2.2.2',deepnet="0.2.1",earth="5.3.4",nnet="7.3.19",rpart="4.1.23",monmlp="1.1.5",  RSNNS="0.4.17",evtree="1.0.8",RANN="2.6.2")

  packs<-names(packages)

  temp<-sapply( packs[!packs %in% installed],function(x) detach_package(x,installed))
  if(!"pak"%in%installed){
    install.packages("pak",verbose=F,quiet=T,lib=lib[1], repo=repo)
    message("pak installed")
  }
  message("Installing...")
  if(!"progress"%in%installed){
    install.packages("progress",verbose=F,quiet=T, lib=lib[1], repo=repo)
    message("progress installed")
  }
  if(!"rstudioapi"%in%installed){install.packages("rstudioapi",verbose=F, lib=lib[1], repo=repo)}
  pack_table <- data.frame(required = names(packages), required_version = packages, row.names = NULL)
  installed <- installed.packages(lib.loc=lib)[, "Package"]
  pack_table$installed <- names(packages) %in% installed
  pack_table$current_version <- sapply(1:nrow(pack_table), function(i) {
    if (isTRUE(pack_table$installed[i])) {
      packageVersion(pack_table$required[i]) >= package_version(pack_table$required_version[i])
    } else {
      FALSE
    }
  })
  to_install <- pack_table[which(apply(pack_table[c('installed', 'current_version')], 1, sum) < 2),"required"]

  if(!all(packs %in% installed)){
    if(exists('pb')){
      setWinProgressBar(pb, .9,
                        label=paste0("Installing ",length(to_install)," missing packages..."))
    }
    pak::pkg_install(to_install,ask=F, lib=lib[1])
    if(exists('pb')){
      close(pb)
    }

    pak::pak_cleanup(force=T)
  }


}
