
#' @export
mapraster<-function(data,get,coords,base_shape, limits=NULL){
  ymin=length(unique(coords[,2]))
  xmin=length(unique(coords[,1]))
  ncol=round(sqrt(xmin))
  nrow=round(sqrt(ymin))
  prop<-ncol/nrow
  ncol*nrow
  len<-length(data[,get])
  x<-round(sqrt(len)*prop)
  y<-round(len/round(sqrt(len)*prop))
  # set up an 'empty' raster, here via an extent object derived from your data
  e <- extent(to_spatial(coords))
  my_rst <- raster(e, ncol=x, nrow=y)
  my_rst <- rasterize(coords, my_rst, data[,get], fun=mean)
  base_shape0<-st_as_sf(x =  coords,
                        coords = c(colnames( coords))
  )
  base_shape0<-st_set_crs(base_shape0,"+proj=longlat +datum=WGS84 +no_defs")
  p<-ggplot(base_shape0)

  if(!is.null(base_shape)){
    my_rst<-raster::mask( raster::crop(my_rst, raster::extent(base_shape)), base_shape)}

  attr(p,"my_rst")<-my_rst
  limits<-get_limits(limits,base_shape,NULL,coords)
  attr(p,"limits")<-limits
  attr(p, 'base_shape')<-base_shape
  attr(p,"data_z")<-data[,get]
  p

}

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
get_knn<-function(k, newgrid,newy, coords,base_shape){
  prev<-newy

  ytrain<-na.omit(newy)
  traincoords<- coords[names(ytrain),]
  interp_model<-predy<-knn(traincoords,
                           coordinates(newgrid),
                           ytrain,use.all=T, k=k,l=0)

  knn_raster <- raster::raster(SpatialGridDataFrame(newgrid,data.frame(predy)))
  if(!is.null(base_shape)){
    knn_raster<-  raster::mask(knn_raster, base_shape)
  }
  my_rst0<- my_rst<-  knn_raster
  #my_rst@data@attributes[[1]]$label <- levels(prev)[which(levels((prev))%in% my_rst@data@attributes[[1]]$ID)]

  #lev.lab<- levels(prev)[which(levels((prev))%in% my_rst@data@attributes[[1]]$ID)]
  return(list(my_rst=my_rst,my_rst0=my_rst0))

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
#' @export
get_idw<-function(idp, data, base_shape,base_shape_int,coords, newgrid,newy,crs.info,...){
  prev=newy
  ytrain<-na.omit(newy)
  data_spat<-to_spatial(data.frame(coords[rownames(data),],prev=as.numeric(prev)))
  interp_model=df_idw = gstat::idw(prev~1, data_spat, newgrid,idp=idp)
  df_idwraster <- raster::raster(df_idw)
  if(isFALSE(base_shape_int)){
    df_raster<-  raster::mask(df_idwraster, base_shape)}else{
      df_raster<- df_idwraster
    }
  my_rst0<-my_rst<-  raster::ratify(df_raster)
  return(list(my_rst=my_rst,my_rst0=my_rst0))
}
#' @export
map_interp<-function(layer_shape,coords,as_factor,  data){

  base_shape0<-st_as_sf(x =  coords,
                        coords = c(colnames( coords))
  )
  base_shape0<-st_set_crs(base_shape0,"+proj=longlat +datum=WGS84 +no_defs")

  p<-ggplot(base_shape0)

  return(p)

}
#' @export
Map_model<-function(data,get,coords,base_shape=NULL,layer_shape=NULL,res=20000, k=4,idp=4,  limits=NULL,...){
  suppressWarnings({
    base_shape0<-base_shape
    shapes<-get_shapes(base_shape,layer_shape,coords)
    base_shape<-shapes$base_shape
    layer_shape<-shapes$layer_shape
    base_shape_int <-shapes$base_shape_int
    crs.info <-shapes$crs.info
    limits<-get_limits(limits,base_shape,layer_shape,coords)
    geo<-get_geo(data,coords,get)
    newy<-geo$newy
    geopoint=geo$geopoint
    mybreaks=geo$mybreaks
    as_factor<-is.factor(data[,get])
    newgrid<-get_grid(coords,limits,crs.info,res)
    #grd<-st_make_grid(st_multipoint(as.matrix(rbind(coords, limits))),n=c(round(sqrt(res)),round(sqrt(res))))
    #newgrid<-st_set_crs(grd,"+proj=longlat +datum=WGS84 +no_defs")
    interp_res<-if(isTRUE(as_factor)){
      get_knn(k, newgrid,newy, coords,base_shape0)
    } else {get_idw(idp, data, base_shape,base_shape_int,coords, newgrid,newy,crs.info,...) }
    my_rst=interp_res$my_rst
    my_rst0=interp_res$my_rst0
    bbox<-t(limits)
    p<-map_interp(layer_shape,coords,as_factor,  data)
    attr(my_rst0,"get")<-get
    attr(my_rst0,"ids")<-rownames(data)
    attr(p,"limits")<-limits
    attr(p,"base_shape")<-base_shape
    attr(p,"layer_shape")<-layer_shape
    attr(p,"interp_model")<-my_rst0
    attr(p,"my_rst")<-my_rst0
    attr(p,"data_z")<-data[,get]
    p
  })
}
#' @export # breaks - if is numeric, length 3 -min, max, len. Else, the breaks
map_style<-function(data,get,coords, p,main="",subtitle="",cex.axes=13,cex.lab=13,cex.main=13,cex.sub=13,cex.leg=13,factors=NULL,cex.fac=13,col.fac="red",showcoords=T, col.coords="black",cex.coords=13,showguides=T, layer_shape=NULL,col.palette, leg="",layer_col="gray",lighten=1.4,newcolhabs,extralayers=NULL,layer_shape_border="gray", breaks=NULL,key.height=1,keyscale=12, width_hint=0.15,cex_scabar=0.7){

  my_rst<-attr(p,"my_rst")

  rasterpoints <-data.frame(to_spatial(data.frame( rasterToPoints(my_rst))))

  colnames(rasterpoints)<-c("x","y","z")
  if(is.null(breaks)){breaks=pretty(rasterpoints$z)}


  if(isTRUE(is.factor(data[get][,1]))){
    fatores<-data[get][,1]

    #fatores<-fatores[,drop=F]
    colhabs=getcolhabs(newcolhabs,col.palette,nlevels(fatores))

    p<-p+ geom_tile(data = rasterpoints , aes(x = x, y = y, fill = factor(z,levels=my_rst@data@attributes[[1]][,1], labels=my_rst@data@attributes[[1]][,2]))) +   scale_fill_manual(values=c(colhabs), name=leg, drop=F,breaks=breaks)
  } else{



    p=p+geom_tile(data = rasterpoints , aes(x = x, y = y, fill = z))+
      scale_fill_gradientn(colours =scale_color_2(col.palette,newcolhabs),name=leg,breaks=breaks, limits=range(breaks))
  }

  limits<-attr(p, 'limits')
  base_shape<-attr(p, 'base_shape')
  bbox<-t(limits)


  if(!is.null(extralayers)){
    for(i in 1:length(  extralayers$layers)){
      col_extra<-getcolhabs(newcolhabs,extralayers$colors[i],nrow(as.data.frame(st_as_sf(extralayers$layers[[i]]))))
      p<-p+geom_sf(data=st_as_sf(extralayers$layers[[i]]), col=mylighten(   col_extra,extralayers$alphas[i]), lty=1)
      names( p$layers)[length( p$layers)]<-paste0("extra",i)
      if(extralayers$labels[i]!='None'){
        p<-p+geom_sf_text(data=st_as_sf(extralayers$layers[[i]]),aes(label=get(extralayers$labels[i])), size=extralayers$sizes[i],check_overlap=T,col=col_extra)
        names( p$layers)[length( p$layers)]<-paste0("extra_lab",i)
      }

    }

  }

  p<-p+
    # geom_sf(fill= "gray90") +
    coord_sf(xlim = c(bbox[1,]), ylim = c(bbox[2,]), expand = FALSE)+
    xlab("Longitude") +
    ylab("Latitude") +
    ggtitle(main, subtitle = subtitle) +
    theme(panel.grid.major = element_blank(), panel.background = element_rect(fill = "white"),

          panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"),

          legend.key.size = unit(key.height, 'pt'),
          axis.text=element_text(size=cex.axes),
          axis.title=element_text(size=cex.lab,face="bold"),
          plot.title=element_text(size=cex.main),
          plot.subtitle=element_text(size=cex.sub),
          legend.text=element_text(size=cex.leg),
          legend.title=element_text(size=cex.leg))
  if(!is.null(factors)){

    geopoint0<-cbind(coords[rownames(data),],data[,get],factors)
    colnames(geopoint0)<-c("x","y","pop","factors")
    p<-p+  geom_text( data=geopoint0, aes(x=x, y=y, label=factors),size=cex.fac,colour=col.fac)
  }
  if(isTRUE(showcoords)){
    geopoint0<-cbind(coords[rownames(data),],data[,get])
    colnames(geopoint0)<-c("x","y","pop")
    p<-p+geom_point( data=geopoint0, aes(x=x, y=y),  size=cex.coords, pch=3, colour=col.coords)}
  if(isTRUE(showguides)){
    xcoords<-pretty(coords[,1])
    ycoords<-pretty(coords[,2])
    p<-p+geom_hline(yintercept=ycoords,color = gray(.5), linetype = "dashed", size = .15)+
      geom_vline(xintercept =xcoords,color = gray(.5), linetype = "dashed", size = .15)
  }
  if(!is.null(layer_shape)){
    layer=get_shapes(base_shape , layer_shape, coords)$layer_shape
    suppressWarnings( st_crs(layer)<-st_crs(st_as_sf(base_shape)))

    layer<-st_transform(layer,crs=st_crs(st_as_sf(base_shape)))
    p<-p+geom_sf(data=layer, fill=mylighten(layer_col,lighten), color=layer_shape_border,lty=1) + coord_sf(xlim = c(bbox[1,]), ylim = c(bbox[2,]), expand = FALSE)
  }


  p<-p+annotation_scale(location = "br", width_hint = width_hint,text_cex=cex_scabar,height  =unit(cex_scabar/4,"cm")) +
    annotation_north_arrow(location = "tl",
                           which_north = "true",
                           width = unit(keyscale, "pt"),
                           height  = unit(keyscale, "pt"),
                           pad_x = unit(.1, "in"),
                           pad_y = unit(0.1, "in"),
                           style = north_arrow_fancy_orienteering)
  p

}
