#' @export
validate_data<-function(data){
  res<-TRUE
  if(is.null(data)){
    res<-"Data not found"
  } else{
    coords<-attr(data,"coords")
    if(!length(coords)>0){
      res<-'The selected Datalist has no coordinates.'
    } else{
      if(ncol(coords)!=2){
        res<-"Error in Coordinates: Number of columns not equal 2"
      }
    }
  }
  res
}
#' @export
validate_data2<-function(data){
  coords<-attr(data,"coords")
  all(c(data_ok=length(data)>0,
        coords_ok=length(coords)>0,
        coords_ok2=ncol(coords)==2))


}
#' @export

get_data_map<-function(saved_data,name,attr,var,filter,filter_level){
  data<-saved_data[[name]]
  coords<-attr(data,"coords")
  factors<-attr(data,"factors")
  data_selected<-if(attr=="Factor-Attribute"){
    factors
  } else{
    data
  }
  filtered_rows<-if(filter=="None"){
    rownames(factors)
  } else{
    fac<-factors[,filter]
    rownames(factors)[fac==filter_level]}

  newdata<-data_selected[filtered_rows,var, drop=F]
  final_data<-data_migrate(data,newdata,name)
  args_list <- as.list(environment())
  args_list <- args_list[!names(args_list) %in% "get_data_map"]
  attr(final_data,"args")<-args_list
  return(final_data)
}
#' @export
breaks_interval<-function(z,n_breaks=5){
  n_breaks=n_breaks-1
  need(n_breaks!=0,"Breaks should be higher than 2") |> validate()
  if(n_breaks==1){
    return(range(z))
  }

  breaks=cut(z,n_breaks)
  break_intervals <- levels(breaks)
  unique(c(as.numeric( sub("\\((.+),.*", "\\1", break_intervals) ),
           as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", break_intervals) )))
}
#' @export
gg_circles<-function(p,rasterpoints,rst,newcolhabs,pal,fillOpacity,reverse_palette,name,breaks,min_radius,max_radius, scale_radius){
  if(is.factor(rst[,1])){
    colors0<-colors<-newcolhabs[[pal]](nlevels(rst[,1]))
    range<-c(max_radius,max_radius)
  } else{
    colors0<-colors<-newcolhabs[[pal]](256)
    range<-if(isTRUE(scale_radius)){c(min_radius,max_radius)} else {max_radius}
  }
  if(isTRUE(reverse_palette)){
    colors0<-colors<-rev(colors)
  }
  colors<-adjustcolor(colors,fillOpacity)

  if(is.factor(rst[,1])){
    p<-p+
      geom_point(aes(x,y, color=z),data=rasterpoints,size=max_radius)+scale_colour_manual(values=colors, name=colnames(rst))
  } else{
    p<-p+
      geom_point(aes(x,y,size=z, color=z),data=rasterpoints)+
      scale_colour_gradientn(guide="none",
                             colours=colors,
                             breaks =breaks,

                             limits = range(pretty(breaks)))+
      scale_size(range=range,
                 guide="none",
                 breaks =breaks,
                 name=colnames(rst[,1]),
                 limits = range(pretty(breaks)) )+
      guides(
        size = guide_legend(
          override.aes = list(
            colour = as.list(adjustcolor( scales::col_numeric(colors0, domain=NULL)(breaks),fillOpacity)),

            breaks=breaks
          )
        )
      )
  }


  p



}
#' @export
rst_tile<-function(p,rasterpoints,rst,newcolhabs,pal,fillOpacity,reverse_palette,name,breaks){


  levs<-rst@data@attributes[[1]]$levels
  is_fac<-length(levs)>0
  name<-attr(rst,'z_name')
  if(is_fac){
    colhabs= newcolhabs[[pal]](length(levs))
    colhabs=adjustcolor(colhabs,fillOpacity)
    if(isTRUE(reverse_palette)){
      colhabs<-rev(colhabs)
    }
    p<-p+ geom_tile(data = rasterpoints , aes(x = x, y = y, fill = factor(z,levels=rst@data@attributes[[1]][,1], labels=rst@data@attributes[[1]][,2]))) +   scale_fill_manual(values=c(colhabs), name=name, drop=F,breaks=breaks)
  } else{
    p=p+geom_tile(data = rasterpoints , aes(x = x, y = y, fill = z))+scale_fill_gradientn(colours =scale_color_2(pal,newcolhabs, fillOpacity,reverse_palette),name=name,breaks=breaks, limits=range(breaks))
  }
  return(p)
}
#' @export
get_pie_gg<-function(rst,factor_chart=1,buffer_zize=1,fun="sum",min_radius=1,max_radius=2,newcolhabs, pal){

  df0<-get_chart_data(rst,factor_chart, distance=buffer_zize, fun=fun)
  df<-data.frame(df0)
  colnames(df)<-colnames(df0)

  popup<-if(is.factor(rst[,1])){
    popupArgs(showTitle =F,showValues =F,supValues =get_popup_chart(df))
  } else{
    popupArgs()
  }

  facddf<-df[-c(1:2)]
  fac<-attr(rst,"factors")[factor_chart]

  if(is.factor(rst[,1])){
    colors <-    get_cols_factor_chart(df,rst,newcolhabs[[pal]](100))
    width=max_radius
  } else{


    colors <- newcolhabs[[pal]](ncol(facddf))
    width = scales::rescale(rowSums(facddf),c(min_radius,max_radius))
  }
  colnames(facddf)<-gsub("_split_",".",colnames(facddf))
  res<-cbind(df[,1:2],facddf)
  attr(res,"color")<-colors
  res
}
#' @export
gg_pie<-function(p,rst,factor_chart,buffer_zize,fun,min_radius,max_radius,newcolhabs,pal,reverse_palette, fillOpacity){
  pie_data<-get_pie_gg(rst,factor_chart,buffer_zize,fun,min_radius,max_radius,newcolhabs, pal)
  if(is.factor(rst[,1])){
    colpie<-attr(pie_data,"color")
  } else{
    colpie<-newcolhabs[[pal]](ncol(pie_data))
  }
  colpie<-adjustcolor(colpie,fillOpacity)


  if(isTRUE(reverse_palette)){
    colpie<-rev(colpie)
  }
  p<-p+ geom_scatterpie(aes(x=x, y=y), data=pie_data,
                     cols=colnames(pie_data)[-c(1:2)],
                     color=NA)+
    scale_fill_manual(name=colnames(rst[,1]),values = colpie,drop=F)
  p
}
#' @export

#' @export
add_extra_shapes<-function(p,extra_shape,args_extra){
  p2<-p
  ae<-data.frame(args_extra)
  ae[,1]<-as.logical(ae[,1])
  i=1
  for(i in 1:nrow(args_extra)){
    if(isTRUE(ae$layers[i])){
      p2<-p2+geom_sf(data=st_as_sf(extra_shape[[i]]), col=ae$colors[i], lty=1)
      names( p$layers)[length( p$layers)]<-paste0("extra",i)
      if(ae$labels[i]!='None') {
        p2<-p2+geom_sf_text(data=st_as_sf(extra_shape[[i]]),aes(label=get(ae$labels[i])), size=ae$sizes[i],check_overlap=T,col=colors)
        names( p$layers)[length( p$layers)]<-paste0("extra_lab",i)
      }
    }
  }

  p2
}
#' @export
add_gg_shape<-function(p,shape_args, shape,xlim,ylim,crs.info){
  if(isTRUE(shape_args$shape)){
    if(!is.null(shape)){
      shape<-st_as_sf(shape)
      weight=shape_args$weight/2
      stroke=shape_args$stroke
      fill=shape_args$fill
      base_col=shape_args$color
      if(isFALSE(fill)){
        base_col<-NA
      }
      base_shape_border=base_col
      if(isFALSE(stroke)){
        base_shape_border=NA
      }
      fillOpacity=shape_args$fillOpacity
      colfill<-adjustcolor(base_col,fillOpacity)

      suppressWarnings(st_crs(shape)<-crs.info)

      p<-p+geom_sf(data=shape, fill=colfill, color=base_shape_border,lty=1, lwd=weight)
    }}
  return(p)
}
