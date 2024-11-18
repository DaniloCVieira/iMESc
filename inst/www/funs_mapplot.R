


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
getcolhabs<-function(newcolhabs,palette,n){
  newcolhabs[[palette]](n)
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
    sp::coordinates(coords)<-~Long+Lat
    sp::proj4string(coords) <-sp::CRS(crs.info)
    return(coords)
  })
}
inline<-function (x) {
  tags$div(style="display:inline-block; margin: 0px", x)
}



#' @export
scale_color_2<-function (palette="viridis",newcolhabs,fillOpacity=1, reverse_palette=F)
{
  cols<-newcolhabs[[palette]](256)
  if(isTRUE(reverse_palette)){
    cols<-rev(cols)
  }
  adjustcolor(cols,fillOpacity)
}
