# Used by the north arrow stored in inst/www/GeomNorthArrow.rds (do not remove)
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


north_arrow_cache<-new.env()
#' @export
annotation_north_arrow<-function (mapping = NULL, data = NULL, ..., height = unit(1.5,"cm"), width = unit(1.5, "cm"), pad_x = unit(0.25,"cm"), pad_y = unit(0.25, "cm"), rotation = NULL,style = north_arrow_orienteering)
{
  if (is.null(data)) {
    data <- data.frame(x = NA)
  }
  # read from disk only once per session
  if(is.null(north_arrow_cache$geom)) north_arrow_cache$geom<-readRDS("inst/www/GeomNorthArrow.rds")
  GeomNorthArrow<-north_arrow_cache$geom
  ggplot2::layer(data = data, mapping = mapping, stat = ggplot2::StatIdentity,
                 geom = GeomNorthArrow, position = ggplot2::PositionIdentity,
                 show.legend = FALSE, inherit.aes = FALSE,
                 params = list(...,height = height, width = width, pad_x = pad_x, pad_y = pad_y,rotation = rotation, style = style))
}





#' @export
getcolhabs<-function(newcolhabs,palette,n){
  if(!palette%in%names(newcolhabs)){
    if(n==1){
      return(palette)
    }
  }

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

# Screenshot of an htmlwidget (leaflet, plotly) as PNG or JPEG without extra packages: a headless
# Chrome/Edge when one is installed, otherwise PhantomJS through webshot.
# Chrome/Edge come first: their virtual time waits for the map tiles, while PhantomJS (kept by
# users of older iMESc versions) captured after 'delay' (0.2 s by default), before the tiles
# arrived, giving maps with points on a grey background (seen on macOS).
#' @export
ll_find_browser<-function(){
  opt<-getOption("imesc.browser")
  if(!is.null(opt)&&file.exists(opt)) return(opt)
  env<-Sys.getenv(c("CHROMOTE_CHROME","CHROME_PATH"))
  env<-env[nzchar(env)&file.exists(env)]
  if(length(env)) return(unname(env[1]))
  cands<-if(.Platform$OS.type=="windows"){
    pf<-c(Sys.getenv("PROGRAMFILES"),Sys.getenv("PROGRAMFILES(X86)"),Sys.getenv("LOCALAPPDATA"))
    pf<-pf[nzchar(pf)]
    c(file.path(pf,"Google/Chrome/Application/chrome.exe"),
      file.path(pf,"Microsoft/Edge/Application/msedge.exe"),
      file.path(pf,"Chromium/Application/chrome.exe"))
  } else if(Sys.info()[["sysname"]]=="Darwin"){
    apps<-c("/Applications",file.path(Sys.getenv("HOME"),"Applications"))
    c(file.path(apps,"Google Chrome.app/Contents/MacOS/Google Chrome"),
      file.path(apps,"Microsoft Edge.app/Contents/MacOS/Microsoft Edge"),
      file.path(apps,"Chromium.app/Contents/MacOS/Chromium"),
      file.path(apps,"Brave Browser.app/Contents/MacOS/Brave Browser"))
  } else{
    unname(Sys.which(c("google-chrome","google-chrome-stable","chromium","chromium-browser","microsoft-edge")))
  }
  cands<-cands[nzchar(cands)&file.exists(cands)]
  if(length(cands)) cands[1] else NULL
}

#' @export
# format: "png" (default) or "jpeg"; both engines choose the format by the file extension
ll_widget_png<-function(widget,file,vwidth=800,vheight=600,zoom=1,delay=0.2,format=c("png","jpeg")){
  format<-match.arg(format)
  dir<-tempfile("widget_")
  dir.create(dir)
  on.exit(unlink(dir,recursive=TRUE),add=TRUE)
  html<-file.path(dir,"map.html")
  htmlwidgets::saveWidget(widget,html,selfcontained=FALSE)
  browser<-ll_find_browser()
  if(is.null(browser)){
    if(isTRUE(tryCatch(webshot::is_phantomjs_installed(),error=function(e) FALSE))){
      # at least 3 s, so that the map tiles are downloaded before the capture
      shot<-file.path(dir,paste0("shot.",if(format=="jpeg") "jpg" else "png"))
      webshot::webshot(html,file=shot,delay=max(3,delay),zoom=zoom,vwidth=vwidth,vheight=vheight)
      file.copy(shot,file,overwrite=TRUE)
      return(invisible(file))
    }
    stop("No screenshot engine found. Install Google Chrome or Microsoft Edge, or PhantomJS with webshot::install_phantomjs().")
  }
  out<-file.path(dir,paste0("shot.",format))
  # file:///C:/... on Windows, file:///var/... on macOS/Linux (the path already starts with /)
  url<-paste0("file:///",sub("^/+","",normalizePath(html,winslash="/")))
  args<-c("--headless=new","--disable-gpu","--hide-scrollbars","--no-first-run","--no-default-browser-check",
          paste0("--user-data-dir=",shQuote(file.path(dir,"profile"))),
          paste0("--window-size=",round(vwidth),",",round(vheight)),
          paste0("--force-device-scale-factor=",zoom),
          # virtual time lets the tiles and the widget finish loading before the capture
          paste0("--virtual-time-budget=",round(max(3000,delay*1000+2000))),
          paste0("--screenshot=",shQuote(out)),
          shQuote(url))
  # the browser is not waited on (its helper processes can keep the console open);
  # the screenshot is ready when the file exists and its size stops changing
  suppressWarnings(system2(browser,args,stdout=FALSE,stderr=FALSE,wait=FALSE))
  t0<-Sys.time()
  last<- -1
  repeat{
    Sys.sleep(0.25)
    size<-if(file.exists(out)) file.info(out)$size else -1
    if(size>0&&size==last) break
    last<-size
    if(difftime(Sys.time(),t0,units="secs")>60) break
  }
  if(!file.exists(out)||file.info(out)$size==0) stop("The browser could not capture the map within 60 s.")
  file.copy(out,file,overwrite=TRUE)
  invisible(file)
}
