
create_pretty_breaks <- function(vec, n_breaks = 5) {
  min_val <- min(vec)
  max_val <- max(vec)

  # Create a little extra space around the range for pretty breaks
  extension <- (max_val - min_val) * 0.1
  pretty_breaks <- pretty(c(min_val - extension, max_val + extension), n_breaks)

  # Ensure the breaks are within the actual range of the vector
  pretty_breaks <- pretty_breaks[pretty_breaks >= min_val & pretty_breaks <= max_val]

  return(pretty_breaks)
}

true_north<-function (x, y, crs, delta_crs = 0.1, delta_lat = 0.1)
{
  pt_crs <- sf::st_sfc(sf::st_point(c(x, y)), crs = crs)
  pt_crs_coords <- as.data.frame(sf::st_coordinates(pt_crs))
  pt_latlon <- sf::st_transform(pt_crs, crs = 4326)
  pt_latlon_coords <- as.data.frame(sf::st_coordinates(pt_latlon))
  pt_grid_north <- sf::st_sfc(sf::st_point(c(x, y + delta_crs)),
                              crs = crs)
  pt_grid_north_coords <- as.data.frame(sf::st_coordinates(pt_grid_north))
  pt_true_north <- sf::st_transform(sf::st_sfc(sf::st_point(c(pt_latlon_coords$X,
                                                              pt_latlon_coords$Y + delta_lat)), crs = 4326), crs = crs)
  pt_true_north_coords <- as.data.frame(sf::st_coordinates(pt_true_north))
  a <- c(x = pt_true_north_coords$X - pt_crs_coords$X, y = pt_true_north_coords$Y -
           pt_crs_coords$Y)
  b <- c(x = pt_grid_north_coords$X - pt_crs_coords$X, y = pt_grid_north_coords$Y -
           pt_crs_coords$Y)
  theta <- acos(sum(a * b)/(sqrt(sum(a * a)) * sqrt(sum(b *
                                                          b))))
  cross_product <- a[1] * b[2] - a[2] * b[1]
  rot_degrees <- theta * 180/pi * sign(cross_product)[1]
  rot_degrees
}
is_grob_like<-function (x)
{
  grid::is.grob(x) || inherits(x, "gList") || inherits(x, "gTree")
}


scalebar_grobs<-function (params, style = c("ticks", "bar"), location = c("bl",
                                                                          "br", "tr", "tl"), bar_cols = c("black", "white"), line_width = 1,
                          line_col = "black", height = unit(0.25, "cm"), pad_x = unit(0.25,
                                                                                      "cm"), pad_y = unit(0.25, "cm"), text_pad = unit(0.15,
                                                                                                                                       "cm"), text_cex = 0.7, text_col = "black", text_face = NULL,
                          text_family = "", tick_height = 0.6)
{
  style <- match.arg(style)
  location <- match.arg(location)
  adj_x <- as.numeric(grepl("r", location))
  adj_y <- as.numeric(grepl("t", location))
  width <- unit(params$widthnpc, "npc")
  origin_x <- unit(adj_x, "npc") - adj_x * width + (0.5 - adj_x) *
    2 * pad_x
  origin_y <- unit(adj_y, "npc") - adj_y * height + (0.5 -
                                                       adj_y) * 2 * pad_y
  text_origin_x <- unit(adj_x, "npc") + (0.5 - adj_x) * 2 *
    (pad_x + text_pad + width)
  text_origin_y <- unit(adj_y, "npc") + (0.5 - adj_y) * 2 *
    (pad_y + 0.5 * height)
  if (style == "bar") {
    bar_grob <- grid::rectGrob(x = origin_x + unit((seq_len(params$majordivs) -
                                                      1) * params$majordivnpc, "npc"), y = origin_y, width = unit(params$majordivnpc,
                                                                                                                  "npc"), height = height, hjust = 0, vjust = 0, gp = grid::gpar(fill = rep(bar_cols,
                                                                                                                                                                                            lengh.out = params$majordivs), col = line_col, lwd = line_width))
  }
  else if (style == "ticks") {
    bar_grob <- grid::gList(grid::segmentsGrob(x0 = origin_x +
                                                 unit((seq_len(params$majordivs + 1) - 1) * params$majordivnpc,
                                                      "npc"), y0 = origin_y, x1 = origin_x + unit((seq_len(params$majordivs +
                                                                                                             1) - 1) * params$majordivnpc, "npc"), y1 = origin_y +
                                                 grid::unit.c(height, rep(height * tick_height, params$majordivs -
                                                                            1), height), gp = grid::gpar(lwd = line_width,
                                                                                                         col = line_col)), grid::segmentsGrob(x0 = origin_x,
                                                                                                                                              y0 = origin_y, x1 = origin_x + width, y1 = origin_y,
                                                                                                                                              gp = grid::gpar(lwd = line_width, col = line_col)))
  }
  else {
    stop("not implemented")
  }
  grid::gList(bar_grob, grid::textGrob(label = params$labeltext,
                                       x = text_origin_x, y = text_origin_y, hjust = adj_x,
                                       vjust = 0.5, gp = grid::gpar(cex = text_cex, col = text_col,
                                                                    fontfamily = text_family, fontface = text_face)))
}
.tosi<-function (unitvalue, unit)
{
  if (unit == "km") {
    unitvalue * 1000
  }
  else if (unit == "m") {
    unitvalue
  }
  else if (unit == "ft") {
    unitvalue/3.28084
  }
  else if (unit == "mi") {
    unitvalue * 1609.344051499
  }
  else if (unit == "in") {
    unitvalue/39.3700799999998
  }
  else if (unit == "cm") {
    unitvalue/100
  }
  else {
    stop("Unrecognized unit: ", unit)
  }
}
.fromsi<-function (sivalue, unit)
{
  if (unit == "km") {
    sivalue/1000
  }
  else if (unit == "m") {
    sivalue
  }
  else if (unit == "ft") {
    sivalue * 3.28084
  }
  else if (unit == "mi") {
    sivalue/1609.344051499
  }
  else if (unit == "in") {
    sivalue * 39.3700799999998
  }
  else if (unit == "cm") {
    sivalue * 100
  }
  else {
    stop("Unrecognized unit: ", unit)
  }
}
.torad<-function (deg) {
  deg * pi/180
}
.geodist<-function (lonlat1, lonlat2) {
  long1 <- .torad(lonlat1[1])
  lat1 <- .torad(lonlat1[2])
  long2 <- .torad(lonlat2[1])
  lat2 <- .torad(lonlat2[2])
  R <- 6371009
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1, sqrt(a)))
  d = R * c
  return(d)

}

scalebar_params<-function (sf_bbox, plotunit = NULL, sf_crs = NULL, widthhint = 0.25,
                           unitcategory = c("metric", "imperial")) {

  unitcategory <- match.arg(unitcategory)
  if (!is.null(sf_crs) && is.null(plotunit)) {
    point_coords <- expand.grid(x = c(sf_bbox["xmin"], sf_bbox["xmax"]),
                                y = c(sf_bbox["ymin"], mean(c(sf_bbox["ymin"], sf_bbox["ymax"])),
                                      sf_bbox["ymax"]))
    latlon_coords <- sf::st_coordinates(sf::st_transform(sf::st_as_sf(point_coords,
                                                                      coords = c("x", "y"), crs = sf_crs), 4326))
    widthbottom <- .geodist(latlon_coords[1, ], latlon_coords[2,
    ])
    widthmiddle <- .geodist(latlon_coords[3, ], latlon_coords[4,
    ])
    widthtop <- .geodist(latlon_coords[5, ], latlon_coords[6,
    ])
    percentdiff <- (max(widthbottom, widthmiddle, widthtop) -
                      min(widthbottom, widthmiddle, widthtop))/min(widthbottom,
                                                                   widthmiddle, widthtop)
    if (percentdiff > 0.1) {
      message("Scale on map varies by more than 10%, scale bar may be inaccurate")
    }
    widthm <- unname(widthmiddle)
    mperplotunit <- unname(widthmiddle/(sf_bbox["xmax"] -
                                          sf_bbox["xmin"]))
  }
  else {
    if (is.null(plotunit)) {
      message("Using plotunit = 'm'")
      plotunit <- "m"
    }
    plotunit <- match.arg(plotunit, choices = c("km", "m",
                                                "cm", "mi", "ft", "in"))
    heightm <- .tosi(sf_bbox["ymax"] - sf_bbox["ymin"], plotunit)
    widthm <- unname(.tosi(sf_bbox["xmax"] - sf_bbox["xmin"],
                           plotunit))
    mperplotunit <- unname(.tosi(1, plotunit))
  }
  geowidthm <- unname(widthm * widthhint)
  if (geowidthm < 1) {
    scaleunits <- c("cm", "in")
  }
  else if (geowidthm < 1600) {
    scaleunits <- c("m", "ft")
  }
  else {
    scaleunits <- c("km", "mi")
  }
  if (unitcategory == "metric") {
    unit <- scaleunits[1]
  }
  else {
    unit <- scaleunits[2]
  }
  widthhintu <- .fromsi(geowidthm, unit)
  tenfactor <- floor(log10(widthhintu))
  widthintens <- floor(widthhintu/(10^tenfactor))
  if (widthintens == 1) {
    widthintens <- 10
    tenfactor = tenfactor - 1
  }
  else if (widthintens == 7) {
    widthintens <- 6
  }
  else if (widthintens == 9) {
    widthintens <- 8
  }
  if (widthintens < 6) {
    majdivtens <- 1
  }
  else {
    majdivtens <- 2
  }
  widthu <- widthintens * 10^tenfactor
  majordiv <- majdivtens * 10^tenfactor
  majordivs <- round(widthu/majordiv)
  widthplotunit <- .tosi(widthu, unit)/mperplotunit
  majordivplotunit <- widthplotunit/majordivs
  params = list()
  params$plotwidthu <- .fromsi(widthm, unit)
  params$widthu <- widthu
  params$widthnpc <- params$widthu/params$plotwidthu
  params$unit <- unit
  params$majordivu <- majordiv
  params$majordivnpc <- params$majordivu/params$plotwidthu
  params$majordivs <- majordivs
  params$widthplotunit <- widthplotunit
  params$majordivplotunit <- majordivplotunit
  params$labeltext <- paste(as.integer(widthu), unit)
  params$extents <- sf_bbox
  params

}






#' @export
north_arrow_fancy_orienteering<-function (line_width = 1, line_col = "black", fill = c("white","black"), text_col = "black", text_family = "",text_face = NULL, text_size = 10, text_angle = 0)
{
  arrow_x <- c(0.25, 0.5, 0.5, 0.75, 0.5, 0.5)
  arrow_y <- c(0.1, 0.8, 0.3, 0.1, 0.8, 0.3)
  arrow_id <- c(1, 1, 1, 2, 2, 2)
  text_y <- 0.95
  text_x <- 0.5
  grid::gList(grid::circleGrob(x = 0.505, y = 0.4, r = 0.3,
                               default.units = "npc",
                               gp = grid::gpar(fill = NA,col = line_col, lwd = line_width)), grid::polygonGrob(x = arrow_x,y = arrow_y, id = arrow_id, default.units = "npc",gp = grid::gpar(lwd = line_width, col = line_col, fill = fill)),
              grid::textGrob(label = "N", x = text_x, y = text_y,
                             rot = text_angle, gp = grid::gpar(fontfamily = text_family,fontface = text_face, fontsize = text_size, col = text_col)))
}


#' @export
annotation_north_arrow<-function (mapping = NULL, data = NULL, ..., height = unit(1.5,"cm"), width = unit(1.5, "cm"), pad_x = unit(0.25,"cm"), pad_y = unit(0.25, "cm"), rotation = NULL,style = north_arrow_orienteering)
{
  if (is.null(data)) {
    data <- data.frame(x = NA)
  }
  GeomNorthArrow<-readRDS("inst/app/www/GeomNorthArrow.rds")
  ggplot2::layer(data = data, mapping = mapping, stat = ggplot2::StatIdentity,
                 geom = GeomNorthArrow, position = ggplot2::PositionIdentity,
                 show.legend = FALSE, inherit.aes = FALSE,
                 params = list(...,height = height, width = width, pad_x = pad_x, pad_y = pad_y,rotation = rotation, style = style))
}


#' @export
annotation_scale<-function (mapping = NULL, data = NULL, ..., plot_unit = NULL,
                     bar_cols = c("black", "white"), line_width = 1,
                     height = unit(0.25, "cm"), pad_x = unit(0.25, "cm"),
                     pad_y = unit(0.25, "cm"), text_pad = unit(0.15, "cm"),
                     text_cex = 0.7, text_face = NULL, text_family = "",
                     tick_height = 0.6)
{
  if (is.null(data)) {
    data <- data.frame(x = NA)
  }
  GeomScaleBar<-readRDS("inst/app/www/GeomScaleBar.rds")
  ggplot2::layer(data = data, mapping = mapping, stat = ggplot2::StatIdentity,
                 geom = GeomScaleBar, position = ggplot2::PositionIdentity,
                 show.legend = FALSE, inherit.aes = FALSE,
                 params = list(...,plot_unit = plot_unit, bar_cols = bar_cols, line_width = line_width,height = height, pad_x = pad_x, pad_y = pad_y, text_pad = text_pad,text_cex = text_cex, text_face = text_face, text_family = text_family,
tick_height = tick_height))
}


#' @export
getcolhabs<-function(newcolhabs,palette,n){
  newcolhabs[[palette]](n)
}

#' @export
plotshape<-function(shape){
  ggplot(st_as_sf(shape)) + geom_sf()+
    theme(panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"))
}

#' @export
#args<-readRDS("args.rds")
#attach(args)
#vals<-readRDS("savepoint.rds")
#args$pie<-T
#args$facpizza<-attr(vals$saved_data$`nema_hellinger (1)`,"factors")$HC6_euc


map_discrete_variable<-function(data,get,coords,base_shape=NULL,layer_shape=NULL,main="",size=.14,cex.main=15,cex.axes=13,cex.lab=15,cex.sub=14,cex.leg=11,cex.pt=7,subtitle="",leg="", factors=NULL,showcoords=F, cex.coords=NULL, col.coords="firebrick",col.palette='turbo',col.fac="firebrick",symbol=15, scalesize_size=T,scalesize_color=T, points=T, cex.fac=4, as_factor=F,bmu=F,key.height=1, colored_by_factor=NULL,showguides=F, limits=NULL,layer_col="gray",lighten=0.5,base_col="white",base_lighten=1,newcolhabs, extralayers=NULL,  data_depth=if(!is.null(extralayers)){3+(length(extralayers$layers)*2)} else{NULL},breaks_len=5,mybreaks=NULL,cexmin.pt=0,layer_shape_border="Grey",base_shape_border="gray", keyscale=12,  width_hint=0.15,cex_scabar=0.7, pie=F,facpizza=NULL
                               ){


  {
    base_shape0<-base_shape
    layer_shape0<-layer_shape
    shapes<-get_shapes(base_shape,layer_shape,coords)
    base_shape<-shapes$base_shape
    layer_shape<-shapes$layer_shape
    BS_ggplot<-layer_shape
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
        colhabs=getcolhabs(newcolhabs,col.palette,as.vector(nlevels))
        if(is.null(leg)){name.var=get} else {name.var=leg}
      }
      prev=newy<-factor(somprev, levels=1:nlevels)
      geopoint<-cbind(coords[names(prev),],prev)
      colnames(geopoint)<-c("x","y","pop")
      colfac= getcolhabs(newcolhabs,col.fac,as.vector(nlevels))[as.factor(prev)]
    } else{
      if(!is.null(factors)){
        colfac= getcolhabs(newcolhabs,col.fac,as.vector(nlevels(na.omit(as.factor(factors)))))[as.factor(factors)]
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


    geopoint<-na.omit(geopoint)
    if(isTRUE(as_factor)){  mybreaks<-1:nlevels
    } else{
      if(is.factor(geopoint$pop)){ mybreaks<-1:nlevels(geopoint$pop)}else {

      }
    }

    if(is.null(limits)){
      limits<-st_bbox(base_shape)}
    xlimits<-limits[c(1,3)]
    ylimits<-limits[c(2,4)]
    bbox<-t(cbind(xlimits,ylimits))
    #if(any(mybreaks==0)){mybreaks<-mybreaks[-which(mybreaks==0)]}

    suppressWarnings(st_crs(BS_ggplot)<-"+proj=longlat +datum=WGS84 +no_defs")

    p<-ggplot(st_as_sf(to_spatial(geopoint)))
    if(!is.null(layer_shape0)){
      p<-p+geom_sf(data=st_as_sf(layer_shape), fill=mylighten(layer_col,lighten), lty=1,color=layer_shape_border)
      names(p$layers)[1]<-"layer_shape"
    }
    if(!is.null(base_shape0)){
      p<-p+geom_sf(data=base_shape, fill=mylighten(base_col,base_lighten),color=base_shape_border, lty=1)

      names(p$layers)[length(p$layers)]<-"base_shape"
    }
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

    if(!is.null(colored_by_factor)){
      colfactor<-colored_by_factor[,1]
      names(colfactor)<-rownames(data[get])
      nlevels_fac<-nlevels(as.factor(colfactor))
      prev_fac<-colfactor
      col_pts=getcolhabs(newcolhabs,col.palette,as.vector(nlevels_fac))
      colorFAC<-  data.frame(prev_fac=levels(colfactor),col_pts, levels=1:nlevels(colfactor))
      geopoint_fac<-cbind(geopoint,fac=prev_fac[rownames(geopoint)])
      col_pts<-col_pts[rownames(geopoint_fac)]
    } else {col_pts=getcolhabs(newcolhabs,col.palette,100)[2]}

  }
  p0<-p

  mylimits<-range(mybreaks)

  if(!is.null(points)) {


    if(isTRUE(scalesize_size)){
      if(isTRUE(scalesize_color)){  p <-  p+ geom_point( data=geopoint, aes(x=x, y=y, size=pop, col=pop), pch=symbol)
      if(!any(geopoint$pop<0)){
        p<-p+scale_radius(name=name.var, range=c(cexmin.pt,cex.pt),guide="none",breaks=mybreaks)
      }

      }
       else if(isFALSE(scalesize_color)&is.null(colored_by_factor))
      {   p <- p+geom_point( data=geopoint, aes(x=x, y=y, size=pop), pch=symbol,color=col_pts)+scale_radius(name=name.var, range=c(cexmin.pt,cex.pt),breaks=mybreaks)} else if(isFALSE(scalesize_color)&!is.null(colored_by_factor)){
        p <- p+geom_point( data=geopoint_fac, aes(x=x, y=y, size=pop, col=fac), pch=symbol)+ scale_color_manual(name=leg,labels =  colorFAC$prev_fac,values =  colorFAC$col_pts,drop=F )+scale_radius(name=name.var, range=c(cexmin.pt,cex.pt),breaks=mybreaks)
      }

    } else  if(isFALSE(scalesize_size))
    {

      if(isTRUE(scalesize_color)){
        p<-  p+ geom_point( data=geopoint, aes(x=x, y=y,  col=pop), pch=symbol,size=cex.pt)+scale_radius(name=name.var, range=c(cexmin.pt,cex.pt),guide="none",breaks=mybreaks)} else if(isFALSE((scalesize_color))){
          if(!is.null(colored_by_factor)) {
            if(isTRUE(pie)){
              req(length(facpizza)>0)
              #vals<-readRDS("savepoint.rds")
              # facpizza<-attr(vals$saved_data$`nema_hellinger (1)`,"factors")[rownames(geopoint),'estacao']
              geopoint$fac<-facpizza
              require("scatterpie")
              li<-split(geopoint,geopoint$fac)
              x<-li[[1]]
              newdf<-data.frame(do.call(rbind,lapply(li,function(x){
                c(x=mean(x$x),y=mean(x$y),group=NA,table(x$pop))
              }))
              )
              newdf$group<-1:nrow(newdf)
              colnames(newdf)[4:ncol(newdf)]<-levels(geopoint$pop)
              newdf<-na.omit(newdf)


              p1<-p0+ geom_scatterpie(aes(x=x, y=y, group=group), data=newdf,
                                      cols=colnames(newdf)[4:ncol(newdf)],
                                      color=NA,
                                      alpha=.8)

              p<-p1+ scale_fill_manual(name=leg,labels =  colorFAC$prev_fac,values =  colorFAC$col_pts,drop=F)
            } else{

              color_factors<-getcolhabs(newcolhabs,col.palette,length(na.omit(levels(geopoint_fac$fac))))


              p <- p+geom_point( data=geopoint_fac, aes(x=x, y=y, col=fac), pch=symbol,size=cex.pt)+ scale_color_manual(name=leg,
                                                                                                                       # labels =  colorFAC$prev_fac,
                                                                                                                        values =  color_factors,drop=F)+scale_radius(name=name.var, range=c(cexmin.pt,cex.pt),guide="none",breaks=mybreaks)
            }} else {
              p<-   p+geom_point( data=geopoint, aes(x=x, y=y), pch=symbol,size=cex.pt, color=col_pts[rownames(geopoint)])+scale_radius(name=name.var, range=c(cexmin.pt,cex.pt),guide="none",breaks=mybreaks)
            }
        }
    }

if(is.numeric(geopoint$pop)){
  if(isFALSE(scalesize_size)&isTRUE(pie)){
    req(length(facpizza)>0)
    #vals<-readRDS("savepoint.rds")
    # facpizza<-attr(vals$saved_data$`nema_hellinger (1)`,"factors")[rownames(geopoint),'estacao']
    geopoint$fac<-facpizza
    require("scatterpie")
    li<-split(geopoint,geopoint$fac)
    x<-li[[7]]
    lis<-lapply(li,function(x){
      xx<-x$pop/length(x$pop)

      c(x=mean(x$x),y=mean(x$y),group=NA,xx)
    })
    #max(unlist(lapply(lis,length)))

    newdf<-data.frame(do.call(rbind,lis))
    newdf$group<-1:nrow(newdf)
    colnames(newdf)[4:ncol(newdf)]<-paste0("obs",1:length(4:ncol(newdf)))
    #newdf[is.na(newdf)]<-0


    p1<-p0+ geom_scatterpie(aes(x=x, y=y, group=group), data=newdf,
                            cols=colnames(newdf)[4:ncol(newdf)],
                            color=NA,
                            alpha=.8)

    p<-p1+ scale_fill_manual(name=leg,values =  newcolhabs[[col.palette]](length(4:ncol(newdf))),drop=F)
  }
}

  }
  names( p$layers)[length( p$layers)]<-paste0('points')

  #scale_color_viridis(name=name.var, limits= range(mybreaks)) +
  if(is.null(colored_by_factor)){
    if(isTRUE(as_factor)){p<- p +   geom_point( data=geopoint, aes(x=x, y=y), pch=symbol, color=colhabs[as.factor(prev)]) }else{
      if(!any(geopoint$pop<0)){
        #p<-p+scale_radius(name=name.var, range=c(cexmin.pt,cex.pt),guide="none",breaks=mybreaks)
      } else{
        p<-p+
          scale_radius(name=name.var,guide="none",breaks=mybreaks)
      }
      p<-p+

        scale_colour_gradientn (colours=getcolhabs(newcolhabs,col.palette,100), guide="none")+

        guides(size = guide_legend(override.aes = list(
          colour = as.list( scales::col_numeric(getcolhabs(newcolhabs,col.palette,100), domain = NULL)(mybreaks)),
          #range= range(geopoint$pop),
         # size=scales::rescale(mybreaks,c(cexmin.pt,cex.pt)),
          breaks=mybreaks
        )))

    }
  }




  #p<-p  +  scale_size(name=name.var, range=c(0,cex.pt), breaks=mybreaks)

if(isTRUE(showguides)){
  xcoords<-pretty(coords[,1])
  ycoords<-pretty(coords[,2])
  p<-p+geom_hline(yintercept=ycoords,color = gray(.5), linetype = "dashed", size = .15)+
    geom_vline(xintercept =xcoords,color = gray(.5), linetype = "dashed", size = .15)
}

  if(any(names(p$layers)%in%"layer_shape")){

    point_layer<-p$layers[ grep("layer_shape",  names(p$layers))]
    old_layer<-p$layers[-grep("layer_shape",  names(p$layers))]
    new_p <- append(old_layer, point_layer, after=length(p$layers))
    p$layers<-new_p

  }
  no<-north_arrow_fancy_orienteering(text_size=cex.axes)
p<-p+
  #guides( colour = guide_legend())+

  annotation_scale(location = "br", width_hint = width_hint,text_cex=cex_scabar,height  =unit(cex_scabar/4,"cm")) +
  annotation_north_arrow(location = "tl",
                         which_north = "true",
                         width = unit(keyscale, "pt"),
                         height  = unit(keyscale, "pt"),
                         pad_x = unit(.1, "in"),
                         pad_y = unit(0.1, "in"),
                         style = no) +
  coord_sf(xlim = c(bbox[1,]), ylim = c(bbox[2,]), expand = FALSE) +


  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle(main, subtitle = subtitle) +
  theme(panel.grid.major = element_blank(),
        panel.background=element_rect(fill=NA, color="white"),
        panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"),
        #legend.key.size = unit(key.height, 'pt'),
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




if(!is.null(extralayers)){
  if(!is.null(data_depth)){
    point_layer<-p$layers[ grep("points",  names(p$layers))]
    old_layer<-p$layers[-grep("points",  names(p$layers))]
    new_p <- append(old_layer, point_layer, after=data_depth-1)
    p$layers<-new_p

  }
  #saveRDS(p,"p.rds")






}





p}


#' @export
mylighten <- function(color, factor = 0.5) {
  if ((factor > 1) | (factor < 0)) stop("factor needs to be within [0,1]")
  col <- col2rgb(color)
  col <- col + (255 - col)*factor
  col <- rgb(t(col), maxColorValue=255)
  col
}
#' @export
to_spatial<-function(coords,  crs.info="+proj=longlat +datum=WGS84 +no_defs"){
  suppressWarnings({
    colnames(coords)[1:2]<-c("Long","Lat")
    coordinates(coords)<-~Long+Lat
    proj4string(coords) <-CRS(crs.info)
    return(coords)
  })
}
inline<-function (x) {
  tags$div(style="display:inline-block; margin: 0px", x)
}
#' @export
inline2<-function (x) {
  tags$div(style="display:inline-block; margin-top: -100px", x)
}
#' @export



scale_color_matlab<-function (..., alpha = 1, begin = 0, end = 1, direction = 1,discrete = FALSE, option = "D",palette=c(
  "turbo",'viridis', 'plasma',"Rushmore1","FantasticFox1",'Grays',"heat",'Purples',"Blues",'Greens',"black","gray","royalblue", "firebrick","forestGreen",'goldenrod3',"white"),
  newcolhabs)
{

  if (discrete) {
    discrete_scale("colour", palette,  newcolhabs[[palette]], ...)
  } else {
    scale_color_gradientn(colours = do.call( newcolhabs[[palette]],args=list(n=256)),...)

  }
}
#' @export
#depths=x
colsbyprof<-function(depths,cols=c('lightblue','darkblue'))
{
  if(is.null(names(depths))){ids<-1:length(depths)} else {ids<-names(depths)}
  rbPal <- colorRampPalette(cols)
  maxdepth<-max(depths)
  profcols <- rbPal(maxdepth)[as.numeric(cut(depths,breaks = length(cols)))]
  lev<-  pretty(depths)
  proflev <- rbPal(max(lev))[as.numeric(cut(lev,breaks = length(lev)))]
  attr(profcols, 'prettydepth')<-lev
  attr(profcols, 'prettycols')<-proflev
  return(profcols)}
#' @export


scale_color_2<-function (palette="viridis",newcolhabs)
{
  newcolhabs[[palette]](256)
}

