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
        res<-"Error in Coordinates: Number of columns not equal 2."
      }
      if(anyNA(coords)){
        res<-"Check your Coordinates: NAs not allowed."
      }
    }
  }
  res
}

#' @export
validate_data2<-function(data){
  coords<-attr(data,"coords")
  all(c(
    data_ok=length(data)>0,
    coords_ok=length(coords)>0,
    coords_ok2=ncol(coords)==2,
    coords_ok3=!anyNA(coords)

  ))



}
#' @export

get_data_map<-function(saved_data,name,attr,var,filter,filter_level){

  req(  all(sapply(list(name,  attr,  var,  filter),length)>0))
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
breaks_interval<-function(z,nbreaks=5){
  nbreaks=nbreaks-1
  need(nbreaks!=0,"Breaks should be higher than 2") |> validate()
  if(nbreaks==1){
    return(range(z))
  }

  seq(min(z),max(z),length.out=nbreaks)
}
col_factor_map<-function(newcolhabs,pal,data,reverse_palette){

  if(is.factor(data[,1])){
    colors0<-colors<-newcolhabs[[pal]](nlevels(data[,1]))
    if(isTRUE(reverse_palette)){
      colors0<-colors<-rev(colors)
    }
    colors0<- colors0[levels(data[,1])%in%data[,1]]
  } else {
    colors0<-newcolhabs[[pal]](256)
    if(isTRUE(reverse_palette)){
      colors0<-colors<-rev(colors)
    }
  }
  colors0
}


#' @export
breaks_label<-function(breaks,z){
  breaks<-as.numeric(breaks)
  res<-c()

  mi<-round(min(z),decimal_places(as.numeric(breaks)))
  bb<-c(mi,breaks)
  for(i in 1:c(length(breaks))){
    res[i]<-paste0("[",bb[[i]],", ",bb[[i+1]],"]")
  }
  res
}

gg_circles<-function(p,rasterpoints,rst,newcolhabs,pal,fillOpacity,reverse_palette,name,custom_breaks,min_radius,max_radius, scale_radius,num_breaks,light=0){
  breaks<-as.numeric(custom_breaks)
  colnames(rasterpoints)<-c('x','y','z')

  if(is.factor(rst[,1])){
    range<-c(max_radius,max_radius)
  } else{
    range<-if(isTRUE(scale_radius)){c(min_radius,max_radius*2)} else {max_radius}
  }
  colors<-colors0<-col_factor_map(newcolhabs,pal,rst,reverse_palette)
  colors<-adjustcolor(lighten(colors,light),fillOpacity)
  p0<-p
  if(is.factor(rst[,1])){
    p<-p+
      geom_point(aes(x,y, color=z),data=rasterpoints,size=max_radius)+scale_colour_manual(values=colors, name=colnames(rst))
  } else{
    dfb<-data.frame(a=breaks[2:length(breaks)-1],
                    b=breaks[2:length(breaks)]
    )

    break_labels<-apply(dfb,1,function(x){
      paste(x[1],"-",x[2])
    })




    #plot(1,col=colors[length(colors)/2],pch=16,cex=15)
    col_leg<-colorBin(colors,domain=rasterpoints$z)(breaks)

    if(length(breaks)==1){
      colors<-col_leg
      colors<-adjustcolor(lighten(colors,light),fillOpacity)
    }
    col_leg<-adjustcolor(lighten(col_leg,light),fillOpacity)
    p<-p+
      geom_point(aes(x,y,size=z, color=z),data=rasterpoints)+
      scale_colour_gradientn(
        guide="none",colours=colors,breaks =breaks, limits = range(c(breaks,rst[,1])))+
      scale_radius(range=range,
                   breaks,
                   name=colnames(rst[,1]),
                   labels=breaks_label(breaks,rasterpoints$z),
                   limits = range(c(breaks,rst[,1])),
                   )      +
      guides(size = guide_legend(
        override.aes = list(
          color=col_leg
        )
      ))
  }




  p



}

#' @export


decimal_places<-function(x){
  sp<-do.call(rbind,strsplit(as.character(format(x, scientific=F)),"\\."))
  if(ncol(sp)==2){
    max(sapply(sp[,2],nchar))

  } else{
    1
  }
}



rst_tile<-function(p,rasterpoints,rst,newcolhabs,pal,fillOpacity,reverse_palette,name,breaks,factor=F,data_o,light=0,custom_breaks){
  custom_breaks<-as.numeric(custom_breaks)
  rasterpoints$z<-scales::rescale(rasterpoints$z,c(min(custom_breaks),max(custom_breaks)))
  name<-attr(rst,'z_name')
  if(isTRUE(factor)){
    levs<-levels(data_o[,1])
    colhabs= lighten(newcolhabs[[pal]](length(levs)),light)
    colhabs<-colhabs[levs%in%rasterpoints$z]
    colhabs=adjustcolor(colhabs,fillOpacity)

    # cols<-cols[levels(data[,1])%in%data[,1]]
    # cols<-cols[as.factor(z_factor)]

    if(isTRUE(reverse_palette)){
      colhabs<-rev(colhabs)
    }
    p<- p+ geom_tile(data = rasterpoints , aes(x = x, y = y, fill =  z))+   scale_fill_manual(values=c(colhabs), name=name, drop=F,breaks=breaks)

  } else{
    round<-decimal_places(custom_breaks)
    p=p+geom_tile(data = rasterpoints , aes(x = x, y = y, fill = z))+scale_fill_gradientn(colours =scale_color_2(pal,newcolhabs, fillOpacity,reverse_palette),name=name,breaks=breaks, limits=range(breaks), labels=round(as.numeric(custom_breaks),round))

  }
  return(p)
}
#' @export
get_pie_gg<-function(rst,factor_chart=1,buffer_zize=1,fun="sum",min_radius=1,max_radius=2,newcolhabs, pal,fillOpacity, light){

  df0<-get_chart_data(rst,factor_chart, distance=buffer_zize, fun=fun)
  df<-data.frame(df0)
  colnames(df)<-colnames(df0)
  facddf<-df[-c(1:2)]
  fac<-attr(rst,"factors")[factor_chart]

  if(is.factor(rst[,1])){
    colors <-    get_cols_factor_chart(df,rst,newcolhabs[[pal]](100))
    width=max_radius
  } else{


    colors <- newcolhabs[[pal]](ncol(facddf))
    width = scales::rescale(rowSums(facddf),c(min_radius,max_radius))
  }
  colors<-adjustcolor(lighten(colors,light),fillOpacity)
  colnames(facddf)<-gsub("_split_",".",colnames(facddf))
  res<-cbind(df[,1:2],facddf)
  attr(res,"color")<-colors
  res
}
#' @export
gg_pie<-function(p,rst,factor_chart,buffer_zize,fun,min_radius,max_radius,newcolhabs,pal,reverse_palette, fillOpacity, light){
  pie_data<-get_pie_gg(rst,factor_chart,buffer_zize,fun,min_radius,max_radius,newcolhabs, pal,fillOpacity,light=light)
  if(is.factor(rst[,1])){
    colpie<-attr(pie_data,"color")
  } else{
    colpie<-newcolhabs[[pal]](ncol(pie_data))
  }
  colpie<-adjustcolor(lighten(colpie,light),fillOpacity)


  if(isTRUE(reverse_palette)){
    colpie<-rev(colpie)
  }
  y<-pie_data$y
  x<-pie_data$x
  library(scatterpie)
  p<-p+ geom_scatterpie(mapping=aes(x=x, y=y), data=pie_data,
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
#shape_args<-base_shape_args

add_gg_shape<-function(p,shape_args, shape,xlim,ylim,crs.info){
  if(isTRUE(shape_args$shape)){
    if(!is.null(shape)){
      shape<-st_as_sf(shape)
      weight=shape_args$weight/2
      stroke=shape_args$stroke
      fill=shape_args$fill
      border_col=shape_args$border_col

      base_col=shape_args$color
      if(isFALSE(fill)){
        base_col<-NA
      }

      if(is.null(border_col)){
        border_col=base_col
      }

      if(isFALSE(stroke)){
        border_col=NA
      }
      fillOpacity=shape_args$fillOpacity
      colfill<-adjustcolor(base_col,fillOpacity)

      suppressWarnings(st_crs(shape)<-crs.info)

      p<-p+geom_sf(data=shape, fill=colfill, color=border_col,lty=1, lwd=weight)
    }}
  return(p)
}
g_cross_results<-function(g, coki=T){

  div(
    div("Cross-Validation results",popify(icon("question-circle"),NULL,gcv_summary(g))),
    renderTable(gcv_df(g),spacing ="xs",rownames=T)
  )

}
