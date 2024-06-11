
#' @export
get_geo<-function(data,coords,get){
  newy=prev=data[get][,1]
  names(newy)=names(prev)=rownames(data)
  nlevels<-nlevels(newy)
  geopoint<-cbind(coords[names(prev),],prev)
  colnames(geopoint)<-c("x","y","pop")
  mybreaks<-if(is.factor(unlist(data[get]))){
    1:nlevels
  } else{ unique(round(quantile((geopoint$pop),c(0.1,0.25,0.5,0.75,1)),2))}
  return(list(geopoint=geopoint,mybreaks=mybreaks,newy=newy, prev=prev))
}
#' @export
get_limits<-function(limits,base_shape,layer_shape,coords){
  shapes<-get_shapes(base_shape, layer_shape, coords)
  base_shape<-shapes$base_shape
  crs.info=shapes$crs.info
  if(is.null(limits)){
    limits<-st_bbox(base_shape)}
  xlimits<-limits[c(1,3)]
  ylimits<-limits[c(2,4)]
  limits<-cbind(xlimits,ylimits)
  colnames(limits)<-colnames(coords)
  attr(limits,"crs.info")<-crs.info
  return(limits)
}
#' @export
get_shapes<-function(base_shape,layer_shape,coords,crs.info="+proj=longlat +datum=WGS84 +no_defs"){
  base_shape_int=F
  if(!is.null(base_shape)){
    class(base_shape)

    crs.info=sf::st_crs(base_shape)
    if(class(base_shape)[1]!='sf') {base_shape=st_as_sf(base_shape)}}else{
      base_shape<-st_as_sf(x =  coords,
                           coords = c(colnames( coords))
      )
      base_shape<-st_set_crs(base_shape,crs.info)
      #base_shape<-st_as_sf(base_shape)
      base_shape_int=T
    }
  if(class(layer_shape)[1]!='sf' & !is.null(layer_shape) ) {
    layer_shape=st_as_sf(layer_shape)
    suppressWarnings(    st_crs(layer_shape)<-st_crs(crs.info))
    layer_shape<-st_transform(layer_shape,crs=crs.info)


  }
  if(is.null(layer_shape)){ layer_shape= base_shape}


  return(list(base_shape=base_shape, layer_shape=layer_shape,base_shape_int=base_shape_int,crs.info=crs.info))

}
#' @export
get_grid<-function(coords,limits,crs.info,res){
  newgrid<- as.data.frame(spsample(to_spatial(rbind(coords, limits)), "regular", n=res))
  names(newgrid)       <- c("X", "Y")
  coordinates(newgrid) <- c("X", "Y")
  gridded(newgrid)     <- TRUE  # Create SpatialPixel object
  fullgrid(newgrid)    <- TRUE  # Create Spatialnewgrid object
  proj4string(newgrid) <- proj4string(to_spatial(coords))
  return(newgrid)

}
#' @export # breaks - if is numeric, length 3 -min, max, len. Else, the breaks
is_scaled_return_scaled<-function(data,newdata){
  scale_attr<-attr(attr(data,"data"),"scale")
  if(is.null(attr(newdata,"scale"))){
    if(!is.null(scale_attr)){
      req(length(scale_attr$center)==ncol(newdata))
      newdata<-scale(newdata,center=scale_attr$center,scale=scale_attr$scale)
      newdata<-data_migrate(datao,newdata,"")
    }
  }
  newdata
}
