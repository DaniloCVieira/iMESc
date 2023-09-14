#' @export
anim_stack1<-function(rlist,zvalue=NULL,base_shape=NULL,layer_shape=NULL,exp=0.01,transp=1,alpha=1,newcolhabs,space=1,xlim=NULL,ylim=NULL,xlab="Longitude",ylab="Latitude",zlab="Layer",zmin=1,zmax=length(rlist)+1,col.labels="black",cex.labels=1,col.coords="black",cex.coords=1,show_coords=rep(T,length(data)), show_labels=rep(F,length(data)),coords=NULL,col_layer="gray",legbar.h=.2,labels=NULL,...){
  {

    zmin<-zmin-(zmin*0.05)
    if(is.null(zvalue)){

      zvalue=rep(1,length(rlist))

    }

    par3d(windowRect = c(0,50, 800, 600))
    res=0
    rlist0<-rlist
    shapes<-get_shapes(base_shape,layer_shape , coords)
    base_shape0<-shapes$base_shape
    layer_shape0<-shapes$layer_shape

    for(i in 1:length(rlist)){
      r0<-rlist[[i]]
      new<-scales::rescale(r0@data@values,c(zvalue[i],zvalue[i]),na.rm=T)
      r0@data@values<-new
      rlist[[i]]<-r0

    }

    i=1
    r2=NULL
    r1<-flip(rlist[[i]])
    r0<-flip(rlist0[[i]])
    col1<-attr(rlist[[i]],"col.palette")
    nas<-which(is.na(r1@data@values))
    r1@data@values[nas]<-0
    r0@data@values[nas]<-0


    r_points = rasterToPoints(r1)
    r_points0 = rasterToPoints(r0)
    z_column=if(!is.null(r2)){4} else{3}
    data = data.frame(r_points)
    data0 = data.frame(r_points0)
    data[,z_column]<-data[,z_column]*max(data[,3], na.rm = T)/max(data[,z_column], na.rm = T)

    #data[nas,3]<-NA
    x  = sort(unique(data[,1]))
    nx = length(x)
    y  = sort(unique(data[,2]))
    ny = length(y)


    x0  = sort(unique(data0[,1]))
    nx0 = length(x0)
    y0  = sort(unique(data0[,2]))
    ny0 = length(y0)

    var0<-data0[,3]
    var<-data[,3]
    #var[nas]<-NA
    z0<-z <- matrix(data[,3], nrow = nx, byrow = F)
    z01<-z1 <- matrix(data0[,3], nrow = nx0, byrow = F)
    #z0[nas]<-NA
    values<-data[,z_column]
    values0<-data0[,z_column]
    colors = adjustcolor(getcolhabs(newcolhabs,col1,n=nx0*ny0),transp)
    ii <- cut(values0, breaks = seq(min(values0), max(values0), len = nx0*ny0),
              include.lowest = TRUE)
    colors <- colors[ii]
    colvals<-matrix(colors, nx0, ny0)
    colvals[nas]<-NA
    z[nas]<-NA
    ztemp<-z
    ztemp[!is.na(ztemp)]<-NA

    persp3d(x=x,y=y,z=z, col=colvals, border=NA,ticktype='detailed',alpha=transp,zlim=c(zmin,zmax),xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab,zlab=zlab,...)



    #rpoints_co<-st_coordinates(st_as_sf(base_shape0))
    # rpoints_co<-data.frame(rpoints_co)
    # rpoints_co$z<-rep(  min(z, na.rm=T), nrow(rpoints_co))
    #polygon3d(rpoints_co[,1],rpoints_co[,2],rpoints_co[,3], fill=F)


  }
  i=2
  for(i in 1:length(rlist)){
    {

      r2=NULL
      r1<-flip(rlist[[i]])
      r0<-flip(rlist0[[i]])
      col1<-attr(rlist[[i]],"col.palette")
      nas<-which(is.na(r1@data@values))
      r1@data@values[nas]<-0
      r0@data@values[nas]<-0

      r_points = rasterToPoints(r1)
      r_points0 = rasterToPoints(r0)
      z_column=if(!is.null(r2)){4} else{3}
      data = data.frame(r_points)
      data0 = data.frame(r_points0)
      data[,z_column]<-data[,z_column]*max(data[,3], na.rm = T)/max(data[,z_column], na.rm = T)

      #data[nas,3]<-NA
      x  = sort(unique(data[,1]))
      nx = length(x)
      y  = sort(unique(data[,2]))
      ny = length(y)


      x0  = sort(unique(data0[,1]))
      nx0 = length(x0)
      y0  = sort(unique(data0[,2]))
      ny0 = length(y0)

      var0<-data0[,3]
      var<-data[,3]
      #var[nas]<-NA
      z0<-z <- matrix(data[,3], nrow = nx, byrow = F)
      z01<-z1 <- matrix(data0[,3], nrow = nx0, byrow = F)
      #z0[nas]<-NA
      values<-data[,z_column]
      values0<-data0[,z_column]
      colors = adjustcolor(getcolhabs(newcolhabs,col1,n=nx0*ny0),transp)
      ii <- cut(values0, breaks = seq(min(values0), max(values0), len = nx0*ny0),
                include.lowest = TRUE)
      colors <- colors[ii]
      colvals<-matrix(colors, nx0, ny0)
      colvals[nas]<-NA
      z[nas]<-NA


      if(!is.null(layer_shape)){


        sp.df<-as_Spatial(layer_shape0)
        ras<-rasterize(sp.df,raster(extent(sp.df),300,300))
        ras<-flip(ras)
        ras@data@values[!is.na(ras@data@values)]<-max(z,na.rm=T)
        datapol<-rasterToPoints(ras)
        zpol<- matrix(ras@data@values, ras@nrows,ras@ncols)
        persp3d(x = seq(min(datapol[,1]), max(datapol[,1]), len = nrow(zpol)), y = seq(min(datapol[,2]), max(datapol[,2]), len = ncol(zpol)), zpol,add=T, col=col_layer, depth_test ="always",zlim=c(zmin,zmax),...)


      }
      persp3d(x=x,y=y,z=z, col=colvals, border=NA,ticktype='detailed',alpha=transp,zlim=c(zmin,zmax), add=T,...)


     # points3d()

    }
  }



  for(i in 1:length(rlist)){
    id<-attr(rlist[[i]],"ids")
    if(isTRUE(show_coords[i])){

      xx <- coords[id,1]
      yy <- coords[id,2]
      zz <- rep(zvalue[i], length(xx))
      text3d(xx,yy,zz+(zz*0.03),texts=rep("+",length(xx)),col = col.coords, cex.coords=cex.labels)

    }
    if(isTRUE(show_labels[i])){

      xx <- coords[id,1]
      yy <- coords[id,2]
      zz <- rep(zvalue[i], length(xx))
      text3d(xx,yy,zz+(zz*0.03),texts=labels[[i]],col = col.labels, cex=cex.labels)
    }

  }

  bgplot3d({
    par(mfrow=c(1,3))
    plot.new()
    plot.new()
    plot(seq(1,50),seq(0,length(rlist)+1, len=50), xaxt = "n", yaxt = "n",xlab="", ylab="", col="white",main="", bty="n")
    for(i in 1:length(rlist))
    {
      co<-getcolhabs(newcolhabs,attr(rlist[[i]],"col.palette"),50)
      col1<-attr(rlist[[i]],"col.palette")
      attrs<-rlist0[[i]]@data@attributes[[1]]
      if(ncol(attrs)>1){
        lab<-attrs[,2]
      } else{
        lab<-round(quantile(seq(min(attrs[,1]), max(attrs[,1]), len=50),c(0,.33,.66,1)),1)
      }

      legend_image <- as.raster(matrix(getcolhabs(newcolhabs,col1,50), ncol=50))


      rasterImage(legend_image, 0.3, i-legbar.h, 50,i)
      text(x=seq(1,50,len=length(lab)),y=rep(i-.3,length(lab)),lab, xpd=T)
      text(x=25,y=i+.2,names(rlist)[i], xpd=T)
      abline(h=i+.3, lty=2, col="gray")


    }


  })
}
#' @export
stack_scatter_rgl<-function(data=NULL,coords,base_shape, layer_shape,z=NULL,col_base="white", col_layer="gray",pt_col="gray",showlabels=F,showpoints=T,pt_cex=rep(1,length(data)),fac_cex=1,fac_col="red",symbol=16,scale_points=F,xlim=NULL, ylim=NULL,labels=NULL,  theta = 0, phi = 40, r = sqrt(3), d = 1,expand = 1, colkey=F,col.grid="gray", zlab="Value", scale_size=T, spacing=2, col.palette=rep("viridis", length(data)), newcolhabs,breaks=5, legwidth=50,cex.axis=1,cex.lab=1, zmin=1,zmax=length(data)+1,   ticktype="detailed",xlab="Longitude", ylab="Latitude", leglab.adj=.85,legtit.posy=1.15,legtit.posx=1,title.srt=0,lab.srt=0,nshape=50,col.labels="black",cex.labels=1,col.coords="black",cex.coords=1,show_coords=rep(T,length(data)), show_labels=rep(F,length(data)),...) {
  {
    getpol<-function(x,y,pt_cex,raios,n){
      circle <- SpatialCircle(x =  x, y =y, r =    pt_cex*raios, n = n)
      co<-coordinates(circle)[[1]][[1]]
      pol<-SpatialPolygons(list( Polygons(list(Polygon(co,hole=F)),'1')))
      rpoints_co<-st_coordinates(st_as_sf(pol))
      rpoints_co<-data.frame(rpoints_co)
      ras<-rasterize(pol,raster(extent(to_spatial(rpoints_co)),n,n))
      datapol<-rasterToPoints(ras)
      ras@data@values[!is.na(ras@data@values)]<-z0[i]
      zpol<- matrix(ras@data@values, ras@nrows,ras@ncols)
      list(zpol,datapol)
    }
    zmin<-zmin-(zmin*0.05)


    #z=NULL
    z.adj=0
    get=1:length(data)
    if(is.null(z)){
      zinit=z0=seq(1,length(get)*spacing, by=spacing)
      zdiff<-diff(z0)[1]
      z=rep(z0, each=length(coords[,1]))
    } else{
      zdiff<-diff(z)[1]
      zinit=z0<-z
      z=rep(z0, each=length(coords[,1]))
    }


    x=rep(coords[,1], length(get))
    y=rep(coords[,2], length(get))
    shapes<-get_shapes(base_shape,layer_shape , coords)
    base_shape0<-shapes$base_shape
    layer_shape0<-shapes$layer_shape
    if(is.null(xlim)){
      xlim<-range(x)
    }
    if(is.null(ylim)){
      ylim<-range(y)
    }
    #col.palette<-list("turbo","viridis","Grays","Blues")
    palettes<-list()
    radiodf<-raio<-diff(quantile(coords[,1],c(0,.15)))
    sortcord<-as.matrix(dist(coords))[,1, drop=F]
    rownames(sortcord)<-rownames(coords)
    sortcord1<-sortcord[-which(duplicated(sortcord[,1])),,drop=F]

    sortcord2<-sortcord1[order(sortcord1[,1]),, drop=F]
    sortcord3<- sortcord2[which(sortcord2[,1]!=0), ,drop=F]


    raio<-if(nrow(sortcord3)>2){
      diff(coords[rownames(sortcord3)[c(1,3)],1])} else{
        diff(coords[rownames(sortcord3)[c(1,2)],1])
      }


    raios<-list()
    pts_cex<-list()
    i=1
    #raio<-    min(abs(data.frame(scale(data.frame(x=coords[rownames(data[[i]]),1],y=coords[rownames(data[[i]]),2],z=data[[i]]), center=T,scale =T))[,3]))


    for(i in 1:length(get)) {
      values<-unlist(data[[i]])
      if(is.factor(values)){
        scaleraio=F
      } else{
        scaleraio=scale_size
      }
      if(!is.factor(values)){
        if(isTRUE(scaleraio)){
          pts_cex[[i]]<-scales::rescale(values, c(0,pt_cex[[i]]))
          raios[[i]]<-scales::rescale(values, c(0,raio))

          pal<-getcolhabs(newcolhabs, col.palette[[i]],100)
          ii <- cut(values, breaks = seq(min(values), max(values), len = 100))
          palettes[[i]]<-pal[ii]

        } else{

          values<-unlist(data[[i]])
          pts_cex[[i]]<-scales::rescale(values, c(0,pt_cex[[i]]))
          raios[[i]]<-scales::rescale(values, c(raio,raio))

          pal<-getcolhabs(newcolhabs, col.palette[[i]],100)
          ii <- cut(values, breaks =seq(min(values), max(values), len = 100))
          palettes[[i]]<-pal[ii]


        }} else {
          values<-unlist(data[[i]])
          pts_cex[[i]]<-rep(pt_cex[[i]], length(values))
          pal<-getcolhabs(newcolhabs, col.palette[[i]],nlevels(values))
          pt_cex[i]<-pt_cex[i]*0.5
          palettes[[i]]<-pal[values]
          raios[[i]]<-rep(raio,length(values))

        }
    }

    x  = coords[rownames(data[[i]]),1]
    nx = length(x)
    y  = coords[,2]
    ny = length(y)

    z00<- matrix(rep(NA, length(z)), nrow = nx, byrow = F)
    {

     open3d(windowRect = c(0,50, 800, 800),useNULL =F)

      persp3d(x = seq(min(coords[,1]), max(coords[,1]), len = nrow(z00)), y = seq(min(coords[,2]), max(coords[,2]), len = ncol(z00)), z00,add=F, col="white", zlim=c(zmin,zmax),xlim=xlim,ylim=ylim,xlab=xlab,zlab=zlab,ylab=ylab, depth_test ="always",...)
      i=2

      withProgress(message = "Running ...",
                   min = 1,
                   max = length(get),
                   {
                     for(i in 1:length(get)){
                       if(!is.null(base_shape)){
                         sp.df<-as_Spatial(base_shape0)
                         ras<-rasterize(sp.df,raster(extent(sp.df),300,300))
                         ras<-flip(ras)
                         ras@data@values[!is.na(ras@data@values)]<-z0[i]
                         datapol<-rasterToPoints(ras)
                         zpol<- matrix(ras@data@values, ras@nrows,ras@ncols)
                         persp3d(x = seq(min(datapol[,1]), max(datapol[,1]), len = nrow(zpol)), y = seq(min(datapol[,2]), max(datapol[,2]), len = ncol(zpol)), zpol,add=F, col=col_base, zlim=c(zmin,zmax),xlim=xlim,ylim=ylim,xlab=xlab,zlab=zlab,ylab=ylab, depth_test ="always", ...)
                       }

                       if(!is.null(layer_shape)){


                         sp.df<-as_Spatial(layer_shape0)
                         ras<-rasterize(sp.df,raster(extent(sp.df),300,300))
                         ras<-flip(ras)
                         ras@data@values[!is.na(ras@data@values)]<-z0[i]
                         datapol<-rasterToPoints(ras)
                         zpol<- matrix(ras@data@values, ras@nrows,ras@ncols)
                         persp3d(x = seq(min(datapol[,1]), max(datapol[,1]), len = nrow(zpol)), y = seq(min(datapol[,2]), max(datapol[,2]), len = ncol(zpol)), zpol,add=T, col=col_layer, zlim=c(zmin,zmax),xlim=xlim,ylim=ylim,xlab=xlab,zlab=zlab,ylab=ylab, depth_test ="always",...)


                       }

                       l<-split(cbind(coords[rownames(data[[i]]),],raios[[i]],palettes[[i]],pts_cex[[i]]), 1:nrow(coords[rownames(data[[i]]),]))
                       l<-l[  which(pts_cex[[i]]!=0)]
                       lapply(l,function(x) {
                         try({
                           if(as.numeric( x[5])!=0)
                           {
                             res<-getpol(x = as.numeric(x[1]), y =as.numeric(x[2]), pt_cex=pt_cex[i],raios=as.numeric(x[3]), n = nshape)

                             zpol<-res[[1]]
                             datapol<-res[[2]]
                             persp3d(x = seq(min(datapol[,1]), max(datapol[,1]), len = nrow(zpol)), y = seq(min(datapol[,2]), max(datapol[,2]), len = ncol(zpol)), zpol,add=T, col=x[4], zlim=c(zmin,zmax),xlim=xlim,ylim=ylim,xlab=xlab,zlab=zlab,ylab=ylab, depth_test ="always",...)


                           }})
                       })
                       incProgress(1)
                     }

                   })



      for(i in 1:length(get)){
        if(isTRUE(show_coords[i])){
          xx <- coords[rownames(data[[i]]),1]
          yy <- coords[rownames(data[[i]]),2]
          zz <- rep(z0[i], length(xx))
          text3d(xx,yy,zz+(zz*0.03),texts=rep("+",length(xx)),col = col.coords, cex.coords=cex.labels)

        }
        if(isTRUE(show_labels[i])){

          xx <- coords[rownames(data[[i]]),1]
          yy <- coords[rownames(data[[i]]),2]
          zz <- rep(z0[i], length(xx))
          text3d(xx,yy,zz+(zz*0.03),texts=labels[[i]],col = col.labels, cex=cex.labels)
        }

      }

    }
  }
}
