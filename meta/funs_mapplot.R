

to_spatial<-function(coords,  crs.info){
  suppressWarnings({
    colnames(coords)[1:2]<-c("Long","Lat")
    coordinates(coords)<-~Long+Lat
    proj4string(coords) <-CRS(crs.info)
    return(coords)
  })
}



map_discrete_variable<-function(data,get,coords,base_shape,layer_shape,main="",size=.14,cex.main=15,cex.axes=13,cex.lab=15,cex.sub=14,cex.leg=11,cex.pt=7,subtitle="",leg="", factors=NULL,showcoords=F, cex.coords=NULL, col.coords="firebrick",col.palette='viridis',col.fac="firebrick",symbol=15, scalesize_size=T,scalesize_color=T, points=NULL, cex.fac=4, as_factor=F,bmu=F, colored_by_factor=NULL,showguides=F){
  suppressWarnings({

    if(class(base_shape)[1]!='sf') {base_shape=st_as_sf(base_shape)}
    if(class(layer_shape)[1]!='sf' & !is.null(layer_shape) ) {layer_shape=st_as_sf(layer_shape)}
    if(is.null(layer_shape)){ layer_shape= st_cast(base_shape, "MULTILINESTRING")}


    BS_ggplot<-st_as_sf(layer_shape)


    if(isTRUE(as_factor)){
      scalesize_size=F
      scalesize_color=T
      if(isTRUE(bmu)){
        somprev<-somC$som.model$unit.classif
        names(somprev)<-names(somC[[1]])
        colhabs<-somC$colunits
        nlevels<-nrow(somC$som.model$grid$pts)
        name.var="bmu"
      } else{
        somprev<-as.numeric(as.factor(data[,get]))
        names(somprev)<-rownames(data[get])
        nlevels<-nlevels(as.factor(somprev))
        colhabs=getcolhabs(col.palette,as.vector(nlevels))
        if(is.null(leg)){name.var=get} else {name.var=leg}
      }

      prev=newy<-factor(somprev, levels=1:nlevels)
      geopoint<-cbind(coords[names(prev),],prev)
      colnames(geopoint)<-c("x","y","pop")
      colfac= getcolhabs(col.fac,as.vector(nlevels))[as.factor(prev)]
    } else{
      if(!is.null(factors)){

        colfac= getcolhabs(col.fac,as.vector(nlevels(as.factor(factors))))[as.factor(factors)]
      }
    }

    if(!is.null(points)){
      geopoint<-cbind(coords[rownames(data),],data[,get])
      colnames(geopoint)<-c("x","y","pop")
      if(is.null(leg)){name.var=get} else {name.var=leg}
    }



    if(is.null(points)& as_factor==F)
    {
      geopoint<-cbind(coords[rownames(data),])
      colnames(geopoint)<-c("x","y")
      name.var=""

    }



    geopoint[geopoint==0]<-NA
    geopoint<-na.omit(geopoint)
    if(isTRUE(as_factor)){  mybreaks<-1:nlevels
    } else{
      if(is.factor(geopoint$pop)){ mybreaks<-1:nlevels(geopoint$pop)}else {
        mybreaks<-unique(round(quantile((geopoint$pop[which(geopoint$pop>0)]),c(0.1,0.25,0.5,0.75,1)),2))
      }
    }



    limits<-st_bbox(base_shape)
    xlimits<-limits[c(1,3)]
    ylimits<-limits[c(2,4)]
    bbox<-t(cbind(xlimits,ylimits))
    if(any(mybreaks==0)){mybreaks<-mybreaks[-which(mybreaks==0)]}


    p<-ggplot(BS_ggplot)
    if(!is.null(colored_by_factor)){

      colfactor<-as.factor(unlist(colored_by_factor))
      names(colfactor)<-rownames(data[get])
      nlevels_fac<-nlevels(as.factor(colfactor))
      prev_fac<-colfactor
      col_pts=getcolhabs(col.palette,as.vector(nlevels_fac))
      col_pts=col_pts[as.factor(prev_fac)]
      names(col_pts)<-names(prev_fac)
      colorFAC<-data.frame(prev_fac,col_pts)
      colorFAC<-colorFAC[ which(duplicated(colorFAC[,1])==F),]
      rownames(colorFAC)<-colorFAC[,1]
      colorFAC[levels(prev_fac),"levels"]<-levels(as.factor(as.numeric(prev_fac)))
      colorFAC<-colorFAC[order(colorFAC[,3]),]


      geopoint_fac<-cbind(geopoint,fac=prev_fac[rownames(geopoint)])
      col_pts<-col_pts[rownames(geopoint_fac)]
    } else {col_pts=getcolhabs(col.palette,100)[2]}


    if(!is.null(points)){
      if(isTRUE(scalesize_size)){
        if(isTRUE(scalesize_color))
        {  p <-  p+ geom_point( data=geopoint, aes(x=x, y=y, size=pop, col=pop), pch=symbol)
        } else if(isFALSE(scalesize_color)&is.null(colored_by_factor))
        {   p <- p+geom_point( data=geopoint, aes(x=x, y=y, size=pop), pch=symbol,color=col_pts)} else if(isFALSE(scalesize_color)&!is.null(colored_by_factor)){
          p <- p+geom_point( data=geopoint_fac, aes(x=x, y=y, size=pop, col=fac), pch=symbol)+ scale_color_manual(name=leg,labels =  colorFAC$prev_fac,values =  colorFAC$col_pts )
        }

      } else  if(isFALSE(scalesize_size))
      {
        if(isTRUE(scalesize_color)){
          p<-  p+ geom_point( data=geopoint, aes(x=x, y=y,  col=pop), pch=symbol,size=cex.pt)} else if(isFALSE((scalesize_color))){
            if(!is.null(colored_by_factor)) {
              p <- p+geom_point( data=geopoint_fac, aes(x=x, y=y, col=fac), pch=symbol,size=cex.pt)+ scale_color_manual(name=leg,labels =  colorFAC$prev_fac,values =  colorFAC$col_pts )
            } else {
              p<-   p+geom_point( data=geopoint, aes(x=x, y=y), pch=symbol,size=cex.pt, color=col_pts[rownames(geopoint)])
            }
          }
      }
    }

    p<-p  +
      scale_size(name=name.var, limits= range(mybreaks),range=c(0,cex.pt))

    #scale_color_viridis(name=name.var, limits= range(mybreaks)) +
    if(is.null(colored_by_factor)){
      if(isTRUE(as_factor)){p<- p +   geom_point( data=geopoint, aes(x=x, y=y), pch=symbol, color=colhabs[as.factor(prev)]) }else{p<-p+scale_color_matlab(name=name.var, limits= range(mybreaks),palette=col.palette) }
    }
    showguides_func<-function(showguides)
    {
      if(isTRUE(showguides)){
        element_line(color = gray(.5), linetype = "dashed", size = .15)}else{element_line(NULL)}
    }

    p<-p+
      guides( colour = guide_legend())+
      geom_sf(fill= "gray90") +
      annotation_scale(location = "br", width_hint = 0.15) +
      annotation_north_arrow(location = "tl", which_north = "true", pad_x = unit(.1, "in"), pad_y = unit(0.1, "in"), style = north_arrow_fancy_orienteering) +
      coord_sf(xlim = c(bbox[1,]), ylim = c(bbox[2,]), expand = FALSE) +


      xlab("Longitude") +
      ylab("Latitude") +
      ggtitle(main, subtitle = subtitle) +
      theme(panel.grid.major = showguides_func(showguides), panel.background = element_rect(fill = "white"),

            panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"),
            axis.line=element_line(),
            axis.text=element_text(size=cex.axes),
            axis.title=element_text(size=cex.lab,face="bold"),
            plot.title=element_text(size=cex.main),
            plot.subtitle=element_text(size=cex.sub),
            legend.text=element_text(size=cex.leg),
            legend.title=element_text(size=cex.leg))
    if(!is.null(factors)){

      geopoint0<-cbind(coords[rownames(data),],factors)
      colnames(geopoint0)<-c("x","y","factors")
      p<-p+  geom_text( data=geopoint0, aes(x=x, y=y, label=factors,),size=cex.fac,colour=colfac)
    }
    if(isTRUE(showcoords)){
      geopoint0<-cbind(coords[rownames(data),],data[,get])
      colnames(geopoint0)<-c("x","y","pop")
      p<-p+geom_point( data=geopoint0, aes(x=x, y=y),  size=cex.coords, pch=3, colour=col.coords)

    }

    p


  })
}




map_interp_variables<-function(data,get,coords,base_shape,layer_shape,main="",size=.14,cex.main=15,cex.axes=13,cex.lab=15,cex.sub=14,cex.leg=11,subtitle="",leg="",res=20000, k=4,idp=4,factors=NULL,showcoords=F, cex.coords=3,col.palette="viridis" ,col.coords="firebrick",cex.fac=3,col.fac='viridis', as_factor=F, bmu=F,showguides=NULL){
  suppressWarnings({

    crs.info=as.character(raster::crs(base_shape))
    if(class(base_shape)[1]!='sf') {base_shape=st_as_sf(base_shape)}
    if(class(layer_shape)[1]!='sf' & !is.null(layer_shape) ) {layer_shape=st_as_sf(layer_shape)}
    if(is.null(layer_shape)){ layer_shape= st_cast(base_shape, "MULTILINESTRING")}
    if(isTRUE(as_factor)){

      if(isTRUE(bmu)){
        somprev<-as.numeric(somC$som.model$unit.classif)
        names(somprev)<-names(somC[[1]])
        colhabs<-somC$colunits
        names(colhabs)<-1:length(colhabs)
        colhabs<-colhabs[-which(names(colhabs)%in%somprev==F)]

        nlevels<-nrow(somC$som.model$grid$pts)

      } else  {
        somprev<-as.numeric(as.factor(data[,get]))
        names(somprev)<-rownames(data[get])
        nlevels<-nlevels(as.factor(somprev))
        colhabs=getcolhabs(col.palette,as.vector(nlevels))
      }

      prev=newy<-factor(somprev, levels=1:nlevels)
      coords<-coords[names(prev),]
      shape=base_shape
      geopoint<-cbind(coords[names(prev),],prev)
      colnames(geopoint)<-c("x","y","pop")
      mybreaks<-1:nlevels
      colfac= getcolhabs(col.fac,as.vector(nlevels))[as.factor(prev)]

      limits<-st_bbox(base_shape)
      xlimits<-limits[c(1,3)]
      ylimits<-limits[c(2,4)]
      limits<-cbind(xlimits,ylimits)
      colnames(limits)<-colnames(coords)

      newgrid<- as.data.frame(spsample(to_spatial(rbind(coords, limits), crs.info = crs.info), "regular", n=res))
      names(newgrid)       <- c("X", "Y")
      coordinates(newgrid) <- c("X", "Y")
      gridded(newgrid)     <- TRUE  # Create SpatialPixel object
      fullgrid(newgrid)    <- TRUE  # Create Spatialnewgrid object
      proj4string(newgrid) <- proj4string(to_spatial(coords, crs.info = crs.info))


      coordsfalta<-coordinates(newgrid )
      ytrain<-na.omit(newy)
      traincoords<- coords[names(ytrain),]
      predy<-knn(traincoords,coordsfalta,ytrain,use.all=T, k=k,l=0)
      knn_raster <- raster::raster(SpatialGridDataFrame(newgrid,data.frame(predy)))

      df_raster<-  raster::mask(knn_raster, base_shape)
      my_rst<-  raster::ratify(df_raster)
      my_rst@data@attributes[[1]]$label <- levels(prev)[which(levels((prev))%in% my_rst@data@attributes[[1]]$ID)]
      names(colhabs)<- levels(prev)[which(levels((prev))%in% my_rst@data@attributes[[1]]$ID)]
      lev.lab<- levels(prev)[which(levels((prev))%in% my_rst@data@attributes[[1]]$ID)]
      rasterpoints <-data.frame( rasterToPoints(my_rst, spatial = TRUE))

    } else {
      if(!is.null(factors)){
        colfac= getcolhabs(col.fac,as.vector(nlevels(as.factor(factors))))[as.factor(factors)]}

      newy=prev=data[,get]
      names(newy)<-rownames(data)
      coords<-coords[rownames(data),]
      shape=base_shape
      geopoint<-cbind(coords[rownames(data),],prev)
      colnames(geopoint)<-c("x","y","pop")
      mybreaks<-unique(round(quantile((geopoint$pop),c(0.1,0.25,0.5,0.75,1)),2))
      limits<-st_bbox(base_shape)
      xlimits<-limits[c(1,3)]
      ylimits<-limits[c(2,4)]
      limits<-cbind(xlimits,ylimits)
      colnames(limits)<-colnames(coords)
      newgrid<- as.data.frame(spsample(to_spatial(rbind(coords, limits), crs.info = crs.info), "regular", n=res))
      names(newgrid)       <- c("X", "Y")
      coordinates(newgrid) <- c("X", "Y")
      gridded(newgrid)     <- TRUE  # Create SpatialPixel object
      fullgrid(newgrid)    <- TRUE  # Create Spatialnewgrid object
      proj4string(newgrid) <- proj4string(to_spatial(coords, crs.info = crs.info))
      coordsfalta<-coordinates(newgrid)
      ytrain<-na.omit(newy)
      data_spat<-to_spatial(data.frame(coords[rownames(data),],prev=as.numeric(prev)), crs.info=crs.info)
      df_idw = idw(prev~1, data_spat, newgrid,idp=idp)
      df_idwraster <- raster::raster(df_idw)
      df_raster<-  raster::mask(df_idwraster, base_shape)
      my_rst<-  raster::ratify(df_raster)
      rasterpoints <-data.frame( rasterToPoints(my_rst, spatial = TRUE))
    }


    bbox<-t(cbind(xlimits,ylimits))
    p<-ggplot(layer_shape)
    if(isTRUE(as_factor)){
      p<-p+ geom_raster(data = rasterpoints , aes(x = x, y = y, fill = factor(predy))) +   scale_fill_manual(values=c(colhabs), name=leg, drop=T)
    } else{

      p=p+geom_raster(data = rasterpoints , aes(x = x, y = y, fill = var1.pred))+
        scale_fill_gradientn(colours =scale_color_2(col.palette))
    }
    p<-p+

      geom_sf(fill= "gray90") +
      annotation_scale(location = "br", width_hint = 0.15) +
      annotation_north_arrow(location = "tl", which_north = "true", pad_x = unit(.1, "in"), pad_y = unit(0.1, "in"), style = north_arrow_fancy_orienteering) +
      coord_sf(xlim = c(bbox[1,]), ylim = c(bbox[2,]), expand = FALSE) +
      xlab("Longitude") +
      ylab("Latitude") +
      ggtitle(main, subtitle = subtitle) +
      theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = .15), panel.background = element_rect(fill = "white"),

            panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"),
            axis.line=element_line(),
            axis.text=element_text(size=cex.axes),
            axis.title=element_text(size=cex.lab,face="bold"),
            plot.title=element_text(size=cex.main),
            plot.subtitle=element_text(size=cex.sub),
            legend.text=element_text(size=cex.leg),
            legend.title=element_text(size=cex.leg))
    if(!is.null(factors)){

      geopoint0<-cbind(coords[rownames(data),],data[,get],factors)
      colnames(geopoint0)<-c("x","y","pop","factors")
      p<-p+  geom_text( data=geopoint0, aes(x=x, y=y, label=factors,),size=cex.fac,colour=colfac)
    }
    if(isTRUE(showcoords)){
      geopoint0<-cbind(coords[rownames(data),],data[,get])
      colnames(geopoint0)<-c("x","y","pop")
      p<-p+geom_point( data=geopoint0, aes(x=x, y=y),  size=cex.coords, pch=3, colour=col.coords)

    }

    p


  })
}








scale_color_matlab<-function (..., alpha = 1, begin = 0, end = 1, direction = 1,discrete = FALSE, option = "D",palette=c(
  "matlab.like2",'viridis', 'cividis', 'plasma',"Rushmore1","GrandBudapest1","FantasticFox1","Blues","heat"))
{
  palette=match.arg(palette,c(
    "matlab.like2",'viridis', 'cividis', 'plasma',"Rushmore1","GrandBudapest1","FantasticFox1","Blues","heat"))
  if (discrete) {
    discrete_scale("colour", palette, get(palette), ...)
  } else {
    scale_color_gradientn(colours = do.call(palette,args=list(n=256)),...)

  }
}




scale_color_2<-function (palette="viridis")
{

switch(palette,
       "viridis" = viridis(256),
       "plasma" = plasma(256),
       "matlab.like2" = matlab.like2(256),
       "Rushmore1"=wes_palette("Rushmore1",n,type ="continuous"),
       "FantasticFox1"=wes_palette("FantasticFox1",256,type ="continuous"),
       "Blues"=colorRampPalette(RColorBrewer::brewer.pal(9,"Blues"))(256),
       "heat"=heat.colors(256)
)


}


getcolhabs<-function (palette=c(
  "matlab.like2",'viridis', 'plasma',"Rushmore1","FantasticFox1","Blues","heat","black","gray","royalblue", "firebrick","forestGreen","goldenrod3"
), n)
{

  switch (palette,
          "viridis" = viridis(n),
          "plasma" = plasma(n),
          "matlab.like2" = matlab.like2(n+1)[-1],
          "black" = rep("black",n),
          "gray" = rep("gray",n),
          "royalblue" = rep("royalblue",n),
          "firebrick" = rep("firebrick",n),
          "forestGreen" = rep("forestGreen",n),
          "goldenrod3" = rep("goldenrod3",n),
          "Rushmore1"=wes_palette("Rushmore1",n,type ="continuous"),
          "FantasticFox1"=wes_palette("FantasticFox1",n,type ="continuous"),
          "Blues"=colorRampPalette(RColorBrewer::brewer.pal(9,"Blues"))(n),
          "heat"=heat.colors(n)

  )


}
