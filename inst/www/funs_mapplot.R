
#' @export
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

#' @export
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

#' @export
is_grob_like<-function (x)
{
  grid::is.grob(x) || inherits(x, "gList") || inherits(x, "gTree")
}


#' @export
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

#' @export
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

#' @export
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

#' @export
.torad<-function (deg) {
  deg * pi/180
}

#' @export
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

#' @export
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
  GeomNorthArrow<-readRDS("inst/www/GeomNorthArrow.rds")
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
  GeomScaleBar<-readRDS("inst/www/GeomScaleBar.rds")
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
scale_color_2<-function (palette="viridis",newcolhabs,fillOpacity=1, reverse_palette=F)
{
  cols<-newcolhabs[[palette]](256)
  if(isTRUE(reverse_palette)){
    cols<-rev(cols)
  }
  adjustcolor(cols,fillOpacity)
}
