#' @export
gg_rst<-function(rst,limits=NULL,main="",subtitle="",axis.text_size=13,axis.title_size=13,
plot.title_size=13,plot.subtitle_size=13,legend.text_size=13,layer_shape=NULL,
extra_shape=NULL,breaks=NULL,pal="turbo",key.height=NULL,
newcolhabs=list(turbo=viridis::turbo),add_extra_shape=F,add_base_shape=T,
add_layer_shape=T,show_labels=F,labels=attr(rst,"factors")[1],
cex.fac=13,col.fac="red",show_coords=F,col.coords="black",cex.coords=13,
show_guides=T,layer_col="gray",lighten=0.4,layer_shape_border="gray",
keyscale=12,width_hint=0.15,cex_scabar=0.7) {
  crs.info<-crs(rst)
  rasterpoints <-  rasterToPoints(rst) |> data.frame()
  colnames(rasterpoints)<-c("x","y","z")
  coords<-coordinates(rst) |> data.frame()
  base_shape<-attr(rst,'base_shape')
  layer_shape<-attr(rst,'layer_shape')
  if(is.null(limits)){
    limits<-get_limits(NULL,base_shape,layer_shape,coords)
    limits<-list(x_min=limits[1,1],x_max=limits[2,1], y_min=limits[1,2],y_max=limits[2,2])
  }
  if(is.null(breaks)){breaks=pretty(rasterpoints$z)}
  base_shape0<-st_as_sf(coords,coords = colnames( coords),crs=crs.info)
  p<-ggplot(base_shape0)
  levs<-rst@data@attributes[[1]]$levels
  is_fac<-length(levs)>0
  name<-attr(rst,'z_name')
  if(is_fac){
    colhabs= newcolhabs[[pal]](length(levs))

    p<-p+ geom_tile(data = rasterpoints , aes(x = x, y = y, fill = factor(z,levels=rst@data@attributes[[1]][,1], labels=rst@data@attributes[[1]][,2]))) +   scale_fill_manual(values=c(colhabs), name=name, drop=F,breaks=breaks)
  } else{
    p=p+geom_tile(data = rasterpoints , aes(x = x, y = y, fill = z))+scale_fill_gradientn(colours =scale_color_2(pal,newcolhabs),name=name,breaks=breaks, limits=range(breaks))
  }
  extra_shape<-attr(rst,'extra_shape')
   p<-add_extra_shapes(p,extra_shape,add_extra_shape)
   xlim<-unlist(limits[1:2])
   ylim<-unlist(limits[3:4])
  p<-p+
    coord_sf(xlim = c(xlim), ylim = ylim, expand = FALSE)+
    xlab("Longitude") +
    ylab("Latitude") +
    ggtitle(main, subtitle = subtitle) +
    theme(panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill=NA,
                                      color="black",
                                      linewidth=0.5,
                                      linetype="solid"),
          axis.text=element_text(size=axis.text_size),
          axis.title=element_text(size=axis.title_size,face="bold"),
          plot.title=element_text(size=plot.title_size),
          plot.subtitle=element_text(size=plot.subtitle_size),
          legend.text=element_text(size=legend.text_size),
          legend.title=element_text(size=legend.text_size))
  if(!is.null(key.height)){
    p<-p+theme(legend.key.size=unit(key.height, 'pt'))
  }

  if(isTRUE(show_labels)){
    coords_labels<-attr(rst,"coords")
    labs<-labels[rownames(coords_labels),]
    df_labels<-cbind(coords_labels,labs)
    colnames(df_labels)<-c("x","y","label")
    p<-p+  geom_text( data=df_labels, aes(x=x, y=y, label=label),size=cex.fac,colour=col.fac)
  }
  if(isTRUE(show_coords)){
    coords_data<-attr(rst,"coords")
    colnames(coords_data)<-c("x","y")
    p<-p+geom_point( data=coords_data, aes(x=x, y=y),  size=cex.coords, pch=3, colour=col.coords)}
  if(isTRUE(show_guides)){
    coords_data<-attr(rst,"coords")
    colnames(coords_data)<-c("x","y")

    xcoords<-pretty(coords_data[,1])
    ycoords<-pretty(coords_data[,2])
    p<-p+geom_hline(yintercept=ycoords,color = gray(.5), linetype = "dashed", linewidth = .15)+
      geom_vline(xintercept =xcoords,color = gray(.5), linetype = "dashed", linewidth = .15)
  }
  if(isTRUE(add_layer_shape)){
    if(!is.null(layer_shape)){
      layer_shape<-st_transform(layer_shape,crs(rst))
      p<-p+geom_sf(data=layer_shape, fill=mylighten(layer_col,lighten), color=layer_shape_border,lty=1) + coord_sf(xlim = xlim, ylim = ylim, expand = FALSE)

    }}
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
