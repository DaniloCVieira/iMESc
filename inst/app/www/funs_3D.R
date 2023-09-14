#' @export
SpatialCircle<-function (x, y, r, n = 100, proj4str)
{
  pts <- seq(0, 2 * pi, length.out = n)
  xy <- cbind(x + r * sin(pts), y + r * cos(pts))
  sl <- SpatialLines(list(Lines(list(Line(xy)), "line")))
  if (!missing(proj4str))
    sp::proj4string(sl) <- proj4str
  return(sl)
}


circle <- function(x, y, r=0.1) {
  edgeCount <- 50
  intervals <- (1:edgeCount) / edgeCount * 2 * pi
  res1<-list()
  res2<-list()
  i=1
  for(i in 1:length(x)) {
    res1[[i]]<-r[i]*sin(intervals) + x[i]
    res2[[i]]<-r[i]*cos(intervals) + y[i]

  }
  data.frame(x=res1[[1]], y=res2[[1]])
}


#' @export
stack_scatter3D<-function(data=NULL,coords,base_shape, layer_shape,z=NULL,col_base="white", col_layer="gray",pt_col="gray",showlabels=F,showpoints=T,pt_cex=c(1,1,1,1),fac_cex=1,fac_col="red",symbol=16,scale_points=F,xlim=NULL, ylim=NULL,labels=NULL,  theta = 0, phi = 40, r = sqrt(3), d = 1,expand = 1, colkey=F,col.grid="gray", zlab="Value", scale_size=T, spacing=2, col.palette, newcolhabs,breaks=5, legwidth=50,cex.axis=1,cex.lab=1, zmin=1,zmax=length(data)+1,   ticktype="detailed",xlab="Longitude", ylab="Latitude", leglab.adj=.85,legtit.posy=1.15,legtit.posx=1,title.srt=0,lab.srt=0,col.labels="black",cex.labels=1,col.coords="black",cex.coords=1,show_coords=rep(T,length(data)), show_labels=rep(F,length(data)),custom_legend=NULL) {
  #z=NULL
  z.adj=0
  layout(matrix(c(1,2),nrow=1), widths = c(50,legwidth))
  par(mar=c(1,1,1,1), xpd=T)
  {
    get=1:length(data)
    if(is.null(z)){
      z0=seq(1,length(get)*spacing, by=spacing)
      zdiff<-diff(z0)[1]
      z=rep(z0, each=length(coords[,1]))
    } else{
      zdiff<-diff(z)[1]
      z0<-z
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
    raio<-    min(abs(data.frame(scale(data.frame(
      x=coords[rownames(data[[i]]),1],
      y=coords[rownames(data[[i]]),2],
      z=as.numeric(data[[i]][,1])), center=T,scale =T))[,3]))


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






    pmat<-scatter3D(x, y, z,
                    pch = NA, cex = unlist(pts_cex),  xlim=xlim,ylim=ylim, bty="g", xlab=xlab, ylab=ylab, ticktype=ticktype,theta=theta,phi=phi, r=r, d=d,zlab=zlab,expand=expand,colkey=colkey,col.grid=col.grid, zlim=c(zmin,zmax), col=NA, axes=T,cex.axis=cex.axis, cex.lab=cex.lab,lab=c(50, 15, 20))

    i=1
    for(i in 1:length(get)){
      coords0<-coords[rownames(data[[i]]),]
      if(!is.null(base_shape)){
        rpoints<-st_coordinates(st_as_sf(base_shape0))
        rpoints[,3]<-rep(z0[i], nrow(rpoints))
        res<-trans3d(rpoints[,1],rpoints[,2],rpoints[,3],pmat)
        polygon(res, col=col_base)
      }


      if(!is.null(layer_shape)){
        sp.df<-as_Spatial(layer_shape0)
        df<-data.frame(raster::geom(sp.df))
        df$z<-rep(z0[i], nrow(df))
        cx<-df$x
        cy<-df$y
        l<-split(df,df[,3])
        lapply(l,function(x){
          cz<-rep(min(z), length(x[,'x']))
          res<-trans3d(x[,'x'],x[,'y'],min(x$z),pmat)
          polygon(res, col=col_layer, border=NA)
        }
        )
      }


      for (j in 1:nrow(coords0)) {

        if(pts_cex[[i]][j]!=0){
          circle <- SpatialCircle(x = coords0[j,1], y = coords0[j,2], r =    pt_cex[i]*raios[[i]][j], n = 100)
          co<-coordinates(circle)[[1]][[1]]

          res<-trans3d(co[,1],co[,2],rep(z0[i], length(co[,1])),pmat)
          polygon(res, col=palettes[[i]][j], border=NA)



        }}
      if(isTRUE(show_coords[i])){


        xx <- coords[rownames(data[[i]]),1]
        yy <- coords[rownames(data[[i]]),2]
        zz <- rep(z0[i], length(xx))
        mypoints <- trans3d(xx,yy,zz,pmat = pmat)
        points(mypoints,pch = 3,col =col.coords, cex=cex.coords)
      }

      if(isTRUE(show_labels[i])){

        xx <- coords[rownames(data[[i]]),1]
        yy <- coords[rownames(data[[i]]),2]
        zz <- rep(z0[i], length(xx))
        mypoints <- trans3d(xx,yy,zz,pmat = pmat)
        text(mypoints,labels=labels[[i]],pch = 3,col = col.labels, cex=cex.labels)
      }
    }


  }

  {

    plot.new()
    par(new=T,mar=c(1,1,1,1), xpd=T)
    pmat<-scatter3D(x, y, z,
                     pch = NA, cex = unlist(1),  xlim=xlim,ylim=ylim, bty="n", xlab="", ylab="",theta=theta,phi=phi, r=r, d=d,zlab=zlab,expand=expand,colkey=colkey,col.grid=col.grid, zlim=c(zmin,zmax), col=NA)
    for(i in 1:length(get))
    {
      coords0<-coords[rownames(data[[i]]),]
      values<-unlist(data[[i]])
      if(!is.factor(values)){

        cur<-  data.frame(cex=pt_cex[i], col=palettes[[i]],raios=raios[[i]], value=values, coords0, z=z0[i])
        res<-trans3d(co[,1],co[,2],rep(i, length(co[,1])),pmat)


        leg<-pretty(cur$value)
        leg<-round(seq(min(cur$value),max(cur$value), len=breaks), decimalplaces(leg[2]))
        cur1<-c()
        for(j in 1:length(leg)){
          cur1[j]<-which.min(abs(cur[,4] - leg[j]))
        }
        cexcol<-cur[cur1,]
        if(any(cexcol$value==0)){
          cexcol<-cexcol[-which(cexcol$value==0),]
        }

        if(any(leg==0)){
          leg<-leg[-which(leg==0)]
        }
      } else{

        leg<-levels(values)

        cexcol<-data.frame(
          col=getcolhabs(newcolhabs, col.palette[[i]], length(leg)),
          cex=rep(pt_cex[i],nlevels(values)),

          raios=rep(raio,nlevels(values))
        )
      }

      custom_legend=if(!is.null(custom_legend)){
        custom_legend
      } else{names(data)}


      xleg=seq(min(coords[,1]),max(coords[,1]),len=nrow(cexcol))
      yleg=rep(min(coords[,2]),nrow(cexcol))
      #xleg=xleg*max(xlim)/max(coords[,1])
      col=cexcol$col



      j=1
      for(j in 1:nrow(cexcol)){
        circle <- SpatialCircle(x = xleg[j], y = yleg[j]+z.adj, r =    cexcol$cex[j]*cexcol$raios[j], n = 100)
        co<-coordinates(circle)[[1]][[1]]

        res<-trans3d(co[,1],co[,2],rep(z0[i], length(co[,1])),pmat)

        polygon(res, col=cexcol$col[j], border=NA)

      }



      restext<-trans3d(xleg,yleg+z.adj,(rep(z0[i], length(yleg))*leglab.adj),pmat)
      text(restext, labels=leg, xpd=T, srt=lab.srt)
      resmain<-trans3d(mean(xleg)+(legtit.posx*radiodf),mean(yleg)+z.adj,z0[i]*legtit.posy,pmat)
      text(resmain, labels=custom_legend[i], xpd=T,srt=title.srt)
    }
  }
  res<-recordPlot()
  return(res)
}

#' @export
get_scatter3D<-function(z,base_shape, layer_shape,coords,col_base="gray", col_layer="gray",pt_col="gray",showlabels=F,showpoints=T,pt_cex=1,fac_cex=1,fac_col="red",symbol=16,scale_points=F,xlim=NULL, ylim=NULL,labels=NULL,  theta = 0, phi = 15, r = sqrt(3), d = 1,
                        expand = 1, colkey=F,col.grid="gray", zlab="Value" ){

  x=coords[,1]
  y=coords[,2]
  shapes<-get_shapes(base_shape,layer_shape , coords)
  base_shape0<-shapes$base_shape
  layer_shape0<-shapes$layer_shape
  if(is.null(xlim)){
    xlim<-range(x)
  }
  if(is.null(ylim)){
    ylim<-range(y)
  }

  pmat<-scatter3D(x, y, z,
                pch = 19, cex = pt_cex,  xlim=xlim,ylim=ylim, bty="g", xlab="Longitude", ylab="Latitude", ticktype="detailed",theta=theta,phi=phi, r=r, d=d,zlab=zlab,expand=expand,colkey=colkey,col.grid=col.grid )




  if(!is.null(base_shape)){
    rpoints<-st_coordinates(st_as_sf(base_shape0))
    rpoints[,3]<-rep(min(z), nrow(rpoints))
    res<-trans3d(rpoints[,1],rpoints[,2],rpoints[,3],pmat)
    polygon(res, col=col_base, border=NA)
  }
  if(!is.null(layer_shape)) {
    sp.df<-as_Spatial(layer_shape0)
    df<-data.frame(raster::geom(sp.df))
    cx<-df$x
    cy<-df$y
    l<-split(df,df[,3])
        lapply(l,function(x){
      cz<-rep(min(z), length(x[,'x']))
      res<-trans3d(x[,'x'],x[,'y'],min(z),pmat)
      polygon(res, col=col_layer, border=NA)
    }
    )
  }

  if(isTRUE(showlabels)){
    text(trans3d(x,y,z, pmat), col=pt_col, labels=labels,cex=fac_cex)}
  if(isTRUE(scale_points)){
    pt_cex<-z*pt_cex/max(z)
    pt.cex=round(quantile(pt_cex, c(0,0.33,0.66,1)),3)
    legend=round(quantile(z, c(0,0.33,0.66,1)),3)
    legend("bottomleft",legend=legend, pch=rep(symbol, length(legend)),pt.cex=pt.cex, bty="n",inset=-0.1, xpd=T)

  }
  if(isTRUE(showpoints)){
    points(trans3d(x,y,z, pmat), col=pt_col, pch=16,cex=pt_cex)}
}
#' @export
stack4D_2<-function (
    rlist,zvalues=T,base_shape=NULL,layer_shape=NULL,space=1,transp=1,colgrid="gray",axes=c(1,2,3,4),show_coords=rep(F,length(rlist)), show_labels=F,coords=NULL,cex.coords=1, col.coords="red", labels=NULL, cex.labels=1, col.labels="blue",stack_eye=1.73,newcolhabs,width.legend=20,legbar.h=.2,z.value=NULL,zmin=0,zmax=11,legy.adj=0,leglabpos=.3,legtitle.pos=.2,col_layer="gray",col_base="white",xlim=NULL,ylim=NULL,xlab="Longitude", ylab="Latitude", zlab="Layer",ticktype="detailed",box_all=T,box_border=F,z_text=zs,  legpal_zajd=1.03,
    labaxis_zadj=1.12,
    lab_xadj=1.01,
    kal=zs,
    kap=zs,
    kat=zs,
    ...
) {
  graphics.off()

  if(is.null(z.value)){
    z.value=seq(1:length(rlist))

  }
  shapes<-get_shapes(base_shape,layer_shape , coords)
  base_shape0<-shapes$base_shape
  layer_shape0<-shapes$layer_shape
  if(is.null(xlim)){
    xlim<-extent(layer_shape0)[1:2]
  }
  if(is.null(ylim)){
    ylim<-extent(layer_shape0)[3:4]
  }

  #m1<-rep(2,length(rlist))
  #m2<-seq(3,length(rlist)+2)
  # m<-as.matrix(cbind(m1,m2))

  #m<-rbind(c(1,1),m)
  #m<-rbind(m,c(max(m+1),max(m+1)))
  # m<-  cbind(rep(1,length(rlist)),rep(2,length(rlist)))
  #layout(m, widths = c(80,width.legend))
  #par(mar=c(0,0,0,0),xpd=T)
  # plot.new()
  par(xpd=T)


  res=0
  rlist0<-rlist

  for(i in 1:length(rlist)){
    r00<-rlist[[i]]
    new<-scales::rescale(r00@data@values,c(z.value[i],z.value[i]),na.rm=T)
    range(new,na.rm=T)
    r00@data@values<-new
    rlist[[i]]<-r00

  }

  {
    # zmax<-max(z.value)+min(z.value)
    r1<-rlist[[1]]
    #r1<-flip(r1, direction="x")
    #r1<-t(flip(r1, direction="y"))
    r0<-rlist0[[1]]


    col1<-attr(rlist[[1]],"col.palette")
    xyz<-rasterToPoints(r1,spatial=T)
    z<-  as.matrix(r1)
    z0<-  as.matrix(r0)
    xx<-seq(r1@extent[1],r1@extent[2], len=nrow(z))
    yy<-seq(r1@extent[3],r1@extent[4], len=ncol(z))

    # Generate the desired number of colors from this palette
    color <- adjustcolor(getcolhabs(newcolhabs,col1,100), transp)
    nrz <- nrow(z)
    ncz <- ncol(z)
    zfacet <- z0[-1, -1] + z0[-1, -ncz] + z0[-nrz, -1] + z0[-nrz, -ncz]
    facetcol <- cut(zfacet, 100)
    # z<-t(apply(z,1,rev))
    #z<-apply(z,2,rev)
    box_border=if(isFALSE(box_border)){ NA    } else{ 'black'}
    pmat<-persp(xx,yy,z, border=NA, zlim=c(zmin,zmax), xlab=xlab, ylab=ylab, zlab=zlab,ticktype=ticktype, r=stack_eye,xlim=xlim,ylim=ylim,box=box_all,...)
  }





  {dtext<-data.frame(x=max(xx)+lab_xadj,y=mean(yy), z=z.value)
    text_trans<-trans3d(dtext$x, dtext$y, dtext$z, pmat)
    kal_trans<-text_trans
    kal_trans$x<-kal_trans$x+kal
    text(kal_trans$x, text_trans$y, labels=z_text, cex=cex.labels, adj=-lab_xadj)}

  {
    leglist<-list()
    for(i in z.value)  {
      leglist[[i]]<-list()
      pic<-which(z.value %in% i)
      co<-getcolhabs(newcolhabs,attr(rlist[[pic]],"col.palette"),50)
      col1<-attr(rlist[[pic]],"col.palette")
      val<-na.omit(rlist0[[i]]@data@values)
      rangleg<-range(val)

      lab<-pretty(val)
      legend_image <- as.raster(matrix(getcolhabs(newcolhabs,col1,50), ncol=50))
      leglist[[i]]$lab=lab
      leglist[[i]]$legend_image=legend_image
      leglist[[i]]$names<-names(rlist)[pic]

    }
  }

  diff_zval<-diff(c(z.value))
  dz<-sapply(z.value,function(x) x)


  #dkap<-dtext
 # dkap$x<-max(xx)*kap
  #dkap<-data.frame(x=max(xx)*kap,y=mean(yy), z=z.value)
  kap_trans<-text_trans
  kap_trans$x<-kap_trans$x+kap


  kat_trans<-text_trans
  kat_trans$x<-kat_trans$x+kat


  for(i in 1:length(leglist)){
    xx<-leglist[[i]]
    xl<-kap_trans$x[i]+.15
    xr<-kap_trans$x[i]+.45
    yb<-kap_trans$y[i]-(legpal_zajd)
    yt<-kap_trans$y[i]-(legpal_zajd*2.5)

    rasterImage(xx$legend_image,
                xleft=xl,
                ybottom=yb,
                xright=xr,
                ytop=yt
    )


    text( seq(xl,xr,len=    length(xx$lab)) , kat_trans$y[i]+labaxis_zadj,xx$lab)
    Sys.sleep(0.1)

  }


  for(i in 1:length(rlist)){
    par(new=T)
    r1<-rlist[[i]]
    r0<-rlist0[[i]]
    id<-attr(r1,"ids")
    r1<-flip(r1, direction="x")
    r1<-t(flip(r1, direction="y"))

    r0<-flip(r0, direction="x")
    r0<-t(flip(r0, direction="y"))

    col1<-attr(rlist0[[i]],"col.palette")
    xyz<-rasterToPoints(r1)

    z<-matrix(r1@data@values, r1@nrows,r1@ncols, byrow = T)
    z0<-matrix(r0@data@values, r0@nrows,r0@ncols, byrow = T)
    z<-apply(z,2,rev)
    z0<-apply(z0,2,rev)
    xx<-seq(rlist[[i]]@extent[1],rlist[[i]]@extent[2], len=nrow(z))
    yy<-seq(rlist[[i]]@extent[3],rlist[[i]]@extent[4], len=ncol(z))
    color <- adjustcolor(getcolhabs(newcolhabs,col1,100), transp)
    try({})
    nrz <- nrow(z)
    ncz <- ncol(z)
    zfacet <- z0[-1, -1] + z0[-1, -ncz] + z0[-nrz, -1] + z0[-nrz, -ncz]
    facetcol <- cut(zfacet, 100)
    minz<-min(z, na.rm=T)
    z[!is.na(z)]<-max(z, na.rm=T)
    par(new=T)

    if(!is.null(base_shape)){
      rpoints<-st_coordinates(st_as_sf(base_shape0))
      rpoints[,3]<-rep(min(z, na.rm=T), nrow(rpoints))
      res<-trans3d(rpoints[,1],rpoints[,2],rpoints[,3],pmat)
      polygon(res, col=col_base, border=if(isFALSE(box_border)){NA} else{col_base})
    }

    if(!is.null(layer_shape)) {
      sp.df<-as_Spatial(layer_shape0)
      df<-data.frame(raster::geom(sp.df))
      cx<-df$x
      cy<-df$y
      l<-split(df,df[,3])
      lapply(l,function(x){
        cz<-rep(min(z), length(x[,'x']))
        res<-trans3d(x[,'x'],x[,'y'],min(z, na.rm=T),pmat)
        polygon(res, col=col_layer, border=if(isFALSE(box_border)){NA} else{col_layer})
      }
      )
    }
    par(new=T)
    persp(xx,yy,z, border=NA, col=color[facetcol], zlim=c(zmin,zmax), axes=F, box=F,r=stack_eye,xlim=xlim,ylim=ylim,...)
    if(isTRUE(show_coords[i])){


      xx <- coords[id,1]
      yy <- coords[id,2]
      zz <- rep(minz, length(xx))
      mypoints <- trans3d(xx,yy,zz,pmat = pmat)
      points(mypoints,pch = 3,col =col.coords, cex=cex.coords)
    }

    if(isTRUE(show_labels[i])){

      xx <- coords[id,1]
      yy <- coords[id,2]
      zz <- rep(minz, length(xx))
      mypoints <- trans3d(xx,yy,zz,pmat = pmat)
      text(mypoints,labels=labels[[i]],pch = 3,col = col.labels, cex=cex.labels)
    }





  }


  #x11()
  #plot(seq(1,50),seq(z.value[1],z.value[length(z.value)], len=50), ann=F, axes=F, type="n", ylim=c(zmin,zmax))






  return(pmat)

}


#stack4D_2(rlist,newcolhabs=newcolhabs, width.legend = 30,legbar.h=0.3)


#stack4D_2(rlist,newcolhabs=newcolhabs, base_shape=base_shape,layer_shape=layer_shape)
#' @export
anim_4D<-function(r1=NULL,r2=NULL,r3=NULL,colors,exp,zvalues=T,transp=1,alpha,newcolhabs,...){

  nas<-which(is.na(r1@data@values))
  if(!is.null(r3)){
    r1 <- raster(st_as_sf(r3), ncol=r1@ncols, nrow=r1@nrows)
    r1 <- rasterize(r3, r)
  }
  if(!is.null(r2)){
    nas2<-which(is.na(r2@data@values))
  }
  rlist<-list(r1,r2)
  maxres<-which.max(unlist(lapply(rlist, function(x)if(!is.null(x)){ x@ncols*x@nrows})))

  {
    r1@data@values[nas]<-0
    if(!is.null(r2)){

      r2@data@values[nas2]<-0
      r_c1 <- raster::crop(r1,extent(r2))
      r_c2<-raster::crop(r2,extent(r1))
      extent(r_c1)<-extent(r_c2)
      s<-stack(r_c1,r_c2)

      r_points = rasterToPoints( s)
    } else{
      r_points = rasterToPoints(r1)
    }


    z_column=if(!is.null(r2)){4} else{3}
    data = data.frame(r_points)
    data[,z_column]<-data[,z_column]*max(data[,3], na.rm = T)/max(data[,z_column], na.rm = T)
    #data[nas,3]<-NA
    x  = sort(unique(data[,1]))
    nx = length(x)
    y  = sort(unique(data[,2]))
    ny = length(y)

    var<-data[,3]
    #var[nas]<-NA
    z0<-z <- matrix(data[,3], nrow = nx, byrow = F)
    #z0[nas]<-NA
    values<-data[,z_column]
    colors = adjustcolor(getcolhabs(newcolhabs,colors,n=nx*ny),transp)
    ii <- cut(values, breaks = seq(min(values), max(values), len = nx*ny),
              include.lowest = TRUE)
    colors <- colors[ii]
    colvals<-matrix(colors, nx, ny)
    colvals[nas]<-NA
    z[nas]<-NA
    if(!isTRUE(zvalues)){
      z[!is.na(unlist(z))]<-zvalues
    }
  }
  persp3d(x=x,y=y,z=z, col=colvals, border=NA,ticktype='detailed',alpha=transp,...)

}
#' @export
get_4D<-function(my_rst1,my_rst2=NULL,colors,exp=0.2,wlegend=20,hlegend=20,tictype='detailed',...)
{

  col0<-colors
  m<-  matrix(c(1,2), ncol=1)
  layout(m, heights = c(80,hlegend))
par(mar=c(1,4,1,1))

  coarser=NULL
  {
    nas<-which(is.na(my_rst1@data@values))
    if(is.null(my_rst2)){
      my_rst1@data@values[nas]<-0
    }
    if(!is.null(my_rst2)){
      nas2<-which(is.na(my_rst2@data@values))
      my_rst2@data@values[nas2]<-0
      r2 <- raster::crop(my_rst1,extent(my_rst2))
      r3<-raster::crop(my_rst2,extent(my_rst1))
      extent(r2)<-extent(r3)
      r3e<-r3@ncols*r3@nrows
      r2e<-r2@ncols*r2@nrows
      if(r3e!=r2e){

        r23<-list(r2=r2,r3=r3)
        nas23<-list(r2=nas,r3=nas2)
        min_res<-which.min(c(r2e,r3e))
        max_res<-which.max(c(r2e,r3e))
        r_change<-r23[[max_res]]
        r_base<-r23[[min_res]]
        coarser=min_res
        e <- extent(r_base)
        r_new <- raster(e, ncol=r_base@ncols, nrow=r_base@nrows)
        r_tochange<-rasterToPoints(r_change)
        nas<-nas23[[min_res]]
        r_new2 <- rasterize( r_tochange[,1:2], r_new, r_tochange[,3], fun=mean)
        r23[[max_res]]<-r_new2
        r2<-r23$r2
        r3<-r23$r3
      }
      s<-stack(r2,r3)
      r_points = rasterToPoints( s)
    } else{
      r_points = rasterToPoints(my_rst1)
    }
    z_column=if(!is.null(my_rst2)){4} else{3}
    data = data.frame(r_points)
    x=1
    y=2
    z=3
    color_var=3
    x  = sort(unique(data[,x]))
    nx = length(x)
    y  = sort(unique(data[,y]))
    ny = length(y)
    rotate <- function(x) t(apply(x, 1, rev))
    z0<-data[,z]
    if(is.null(my_rst2)){
      z0[nas]<-NA

    }

    z  = matrix(data[,z], nrow = nx, byrow = F)
  }

  z0  = matrix(z0, nrow = nx, byrow = F)


  z_col0=z_col = matrix(data[,z_column], nrow = length(x), byrow = F)
  z<-rotate(z)
  z_col<-rotate(z_col)
  z0<-rotate(z0)
  nas<-which(is.na(z0))
  ## Average the values at the corner of each facet
  hgt = 0.25 * (z_col[-nx,-ny] + z_col[-1,-ny] + z_col[-nx,-1] + z_col[-1,-1])


  class_borders=seq(min(data[,z_column]),max(data[,z_column]), length.out=100)
  colfunc = colorRampPalette(colors)
  cols = cut(hgt, breaks = class_borders)
  cols = colfunc(length(class_borders))[as.numeric(cols)]
  z[nas]<-NA
  pmat<-persp(x=x,y=y,z=z, col=cols, border=NA,exp=exp,ticktype=tictype,...)
  attr(pmat,"coarser")<-coarser
  par(mar=c(2,0,0,0), xpd=T)
  plot(seq(1,100),rep(1,100), xaxt = "n", yaxt = "n",xlab="", ylab="", col="white",main="", bty="n")
  legend_image <- as.raster(matrix(col0, ncol=length(col0)))
  leg1<-(100-wlegend)/2
  leg2<-leg1+(wlegend)
 # legw<-quantile( 1:100,c(wlegend,100-wlegend)/100)
  #leg1<-(100-wlegend)/2/100

  rasterImage(legend_image,  leg1, .5, leg2,1)
  if(is.null(my_rst2)){
    rlist0<-my_rst1
  } else{
    rlist0<-my_rst2
  }
try({
  attrs<-rlist0@data@attributes[[1]]
  if(ncol(attrs)>1){
    lab<-attrs[,2]
  } else{
    lab<-round(quantile(seq(min(attrs[,1]), max(attrs[,1]), len=100),c(0,.33,.66,1)),1)

  }



  xlab<-seq(leg1,leg2,len=length(lab))
  text(x=xlab,y=rep(0.2,length(xlab)),lab, xpd=T)
  text(x= mean(xlab),y=1.2,attr(rlist0,"get"), xpd=T)

})
  return(pmat)

}

#' @export
grid4D<-function(coords,r1, col=3, pmat, lwd,  lty){

  ppx<-pretty(rasterToPoints(r1)[,1])
  if(ppx[1]<(min(rasterToPoints(r1)[,1]))){
    ppx[1]<-min(rasterToPoints(r1)[,1])
  }
  if(ppx[length(ppx)]>(max(rasterToPoints(r1)[,1]))){
    ppx[length(ppx)]<-max(rasterToPoints(r1)[,1])
  }

  ppy<-pretty(rasterToPoints(r1)[,2])
  if(ppy[1]<(min(rasterToPoints(r1)[,2]))){
    ppy[1]<-min(rasterToPoints(r1)[,2])
  }
  if(ppy[length(ppy)]>(max(rasterToPoints(r1)[,2]))){
    ppy[length(ppy)]<-max(rasterToPoints(r1)[,2])
  }

  ppz<-pretty(rasterToPoints(r1)[,3])

  if(ppz[length(ppz)]>(max(rasterToPoints(r1)[,3]))){
    ppz[length(ppz)]<-max(rasterToPoints(r1)[,3])
  }


  xx <- ppx
  yy <- ppy
  zz<-ppz

  xy<-expand.grid(xx,yy)
  xx<-xy[,1]
  yy<-xy[,2]
  z1=min(zz)
  z2=max(zz)
  z1 <- trans3d(xx,yy,z1,pmat = pmat)
  z2 <- trans3d(xx,yy,z2,pmat = pmat)
  arrows(z1[[1]], z1[[2]], z2[[1]], z2[[2]],
         col=col,
         lty=lty,
         length=0,
         lwd=lwd)


  x1=min(xx)
  x2=max(xx)
  xy<-expand.grid(yy,zz)
  yy<-xy[,1]
  zz<-xy[,2]
  x1 <- trans3d(x1,yy,zz,pmat = pmat)
  x2 <- trans3d(x2,yy,zz,pmat = pmat)
  arrows(x1[[1]], x1[[2]], x2[[1]], x2[[2]],
         col=col,
         lty=lty,
         length=0,
         lwd=lwd)

  y1=min(yy)
  y2=max(yy)
  xy<-expand.grid(xx,zz)
  xx<-xy[,1]
  zz<-xy[,2]
  y1 <- trans3d(xx,y1,zz,pmat = pmat)
  y2 <- trans3d(xx,y2,zz,pmat = pmat)
  arrows(y1[[1]], y1[[2]], y2[[1]], y2[[2]],
         col=col,
         lty=lty,
         length=0,
         lwd=lwd)

}

#' @export
decimalplaces <- function(x) {
  if ((x %% 1) != 0) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}



