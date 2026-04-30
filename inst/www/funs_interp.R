

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
  newgrid<- as.data.frame(sp::spsample(to_spatial(rbind(coords, limits)), "regular", n=res))
  names(newgrid)       <- c("X", "Y")
  sp::coordinates(newgrid) <- c("X", "Y")
  sp::gridded(newgrid)     <- TRUE  # Create SpatialPixel object
  sp::fullgrid(newgrid)    <- TRUE  # Create Spatialnewgrid object
  sp::proj4string(newgrid) <- sp::proj4string(to_spatial(coords))
  return(newgrid)

}

