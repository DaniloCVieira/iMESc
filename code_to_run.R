
library("shiny")
runGitHub('iMESc','DaniloCVieira', ref="main")
labels <-
  data.frame(read.csv('labels2.csv', stringsAsFactors = T, sep=",", row.names = 1))

data <-
  data.frame(read.csv('amb.csv', stringsAsFactors = T, sep=",", row.names = 1))
data<-na.omit(data)
coords <-data.frame(read.csv('coords.csv', stringsAsFactors = T, sep=";", row.names = 1))
coords<-coords[rownames(data),]
get<-"Salinity"
factors<-labels$Campanha

layer_shape<-get(gsub(" ","",capture.output(load('0006_layer_shape',verbose=T))[2]))
base_shape<-get(gsub(" ","",capture.output(load('0005_base_shape',verbose=T))[2]))

m<-map_discrete_variable(
  data = data,
  coords =coords,
  base_shape = base_shape,
  layer_shape = layer_shape,
  get = get,
  main = "input$var_map",
  factors=NULL,
  showcoords=T,
  cex.pt = 7,
  cex.coords=2,
  cex.fac=2,
  col.fac = 2,
  cex.axes=12,
  cex.lab=1,
  leg=NULL,
  scalesize_size= T,
  scalesize_color=F,
  points=T,
  as_factor=F,
  bmu=F,
  colored_by_factor=  factors,
  showguides=T)
m
