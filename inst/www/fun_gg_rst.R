#' @export
is_grob_like<-function (x)
{
  grid::is.grob(x) || inherits(x, "gList") || inherits(x, "gTree")
}


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
    req(filter%in%colnames(factors))
    fac<-factors[,filter]
    rownames(factors)[fac==filter_level]}

  req(var%in%colnames(data_selected))
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

gg_circles<-function(p,rasterpoints,rst,newcolhabs,pal,fillOpacity,reverse_palette,name,custom_breaks,min_radius,max_radius, scale_radius,num_breaks,light=0,show_legend=T,leg_title=NULL){

  colnames(rasterpoints)<-c('x','y','z')

  if(is.factor(rst[,1])){
    range<-c(max_radius,max_radius)
  } else{
    breaks<-as.numeric(custom_breaks)
    range<-if(isTRUE(scale_radius)){c(min_radius,max_radius*2)} else {max_radius}
  }
  colors<-colors0<-col_factor_map(newcolhabs,pal,rst,reverse_palette)
  colors<-adjustcolor(colorspace::lighten(colors,light),fillOpacity)
  p0<-p
  if(is.factor(rst[,1])){
    p<-p+
      geom_point(aes(x,y, color=z),data=rasterpoints,size=max_radius,show.legend =show_legend)+scale_colour_manual(values=colors, name=leg_title)
  } else{
    dfb<-data.frame(a=breaks[2:length(breaks)-1],
                    b=breaks[2:length(breaks)]
    )

    break_labels<-apply(dfb,1,function(x){
      paste(x[1],"-",x[2])
    })




    #plot(1,col=colors[length(colors)/2],pch=16,cex=15)
    col_leg<-leaflet::colorBin(colors,domain=rasterpoints$z)(breaks)

    if(length(breaks)==1){
      colors<-col_leg
      colors<-adjustcolor(colorspace::lighten(colors,light),fillOpacity)
    }
    col_leg<-adjustcolor(colorspace::lighten(col_leg,light),fillOpacity)
    p<-p+
      geom_point(aes(x,y,size=z, color=z),data=rasterpoints)+
      scale_colour_gradientn(
        name=leg_title,
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
  colors<-adjustcolor(colorspace::lighten(colors,light),fillOpacity)
  colnames(facddf)<-gsub("_split_",".",colnames(facddf))
  res<-cbind(df[,1:2],facddf)
  attr(res,"color")<-colors
  res
}
#' @export



#' @export
#shape_args<-base_shape_args

add_gg_shape<-function(p,shape_args, shape,xlim,ylim,crs.info){
  if(isTRUE(shape_args$shape)){
    if(!is.null(shape)){
      shape<-st_as_sf(shape)
      weight=shape_args$weight/2
      stroke=T
      fill=T
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
      fillOpacity=1
      colfill<-base_col

      suppressWarnings(st_crs(shape)<-crs.info)

      p<-p+geom_sf(data=shape, fill=colfill, color=border_col,lty=1, lwd=weight)
    }}
  return(p)
}
g_cross_results<-function(g, coki=T){

  div(
    div("Cross-Validation results",shinyBS::popify(icon("question-circle"),NULL,gcv_summary(g))),
    renderTable(gcv_df(g),spacing ="xs",rownames=T)
  )

}






gg_grid<-function(ps,ncol,nrow,main,padding){

  title_grob <- grid::textGrob(main, gp=grid::gpar(cex=2),vjust=0.3)
  title_gTree <- grid::gTree(children = grid::gList(title_grob))
  ml <-gridExtra::grid.arrange(grobs=ps,
                    ncol = ncol,nrow=ceiling(length(ps)/ncol),
                    top =title_gTree,
                    padding=unit(padding,"npc"))

  #graphics.off()
}
rst_tile<-function(p,rasterpoints,rst,newcolhabs,pal,fillOpacity,reverse_palette,name,breaks,factor=F,data_o,light=0,custom_breaks,show_legend=T, limits,breaks_on=T,leg_title=NULL){

  if(!isTRUE(factor)){
    custom_breaks<-as.numeric(custom_breaks)
    if(is.null(limits)){
      limits<-range(breaks)
    }
  }

  # rasterpoints$z<-scales::rescale(rasterpoints$z,c(min(custom_breaks),max(custom_breaks)))
  name<-leg_title
  if(isTRUE(factor)){
    levs<-levels(data_o[,1])
    colhabs= colorspace::lighten(newcolhabs[[pal]](length(levs)),light)
    # colhabs<-colhabs[levs%in%rasterpoints$z]
    colhabs=adjustcolor(colhabs,fillOpacity)
    #breaks<-breaks[breaks%in%rasterpoints$z]
    # cols<-cols[levels(data[,1])%in%data[,1]]
    # cols<-cols[as.factor(z_factor)]
    labels<-limits<- custom_breaks<-breaks<-as.character(custom_breaks)

    if(isTRUE(breaks_on)){
      limits<-custom_breaks[custom_breaks%in%rasterpoints$z]

    }



    if(isTRUE(reverse_palette)){
      colhabs<-rev(colhabs)
    }

    rasterpoints<-rbind(rasterpoints,data.frame(x=NA,y=NA,z=breaks))
    p<- p+ geom_tile(data = rasterpoints , aes(x = x, y = y, fill =  z))+   scale_fill_manual(values=c(colhabs), name=name,breaks=as.character(breaks), limits=as.character(limits), labels=as.character(labels),drop=FALSE)


  } else{
    round<-decimal_places(custom_breaks)

    p=p+geom_tile(data = rasterpoints , aes(x = x, y = y, fill = z))+scale_fill_gradientn(colours =scale_color_2(pal,newcolhabs, fillOpacity,reverse_palette),name=name,breaks=breaks, limits=range(rasterpoints$z), labels=round(as.numeric(custom_breaks),round))

  }
  return(p)
}


gg_grid_title<-function(main,grid_main_hadj,grid_main_size){
  list(
    ggplot()+geom_text(data=data.frame(x=grid_main_hadj,y=0,label=main),aes(x,y,label=label,hjust=grid_main_hadj),size=grid_main_size)+theme_void()+scale_x_continuous(limits=c(0,1))


  )
}
gg_grid2<-function(ps,m,heights,main="Title",label,padding=0,grid_main_hadj=0,grid_main_size=12){

  ps_title<-gg_grid_title(main,grid_main_hadj,grid_main_size)
  ps1<-c(ps_title,ps)
  layout_matrix = m
  ml <-gridExtra::grid.arrange(grobs=ps1,layout_matrix=layout_matrix,top=NULL,heights =heights )
  ml
  #graphics.off()
}
gg_grid_preview_list<-function(levs,breaks,titles,grid_sub_hadj){
  ps_e<-list()
  for(i in seq_along(levs)) {
    p<-ggplot()+
      geom_text(data=data.frame(x=0,y=0,label=paste("Level:",levs[i])),aes(x,y,label=label))+
      ggtitle(titles[[i]])+
      geom_text(data=data.frame(x=0,y=c(-.2,-.4),label=c("breaks",breaks[[i]])),aes(x,y,label=label))+
      theme_bw()+xlab('')+ylab('')+
      scale_y_continuous(limits=c(-1,1))
    ps_e[[i]]<-p +theme(
      axis.text=element_blank(),
      axis.ticks=element_blank(),
      panel.grid=element_blank(),
      plot.title = element_text(hjust=grid_sub_hadj)

    )
  }
  ps_e

}



scatter_3dplotly<-function(points,text,shape_args=NULL,xlab="x",ylab="y",zlab="z
",plot.title_size=12,axis.text_size=12,axis.title_size=12,main="",...){

  p <- plotly::plot_ly( points,    showlegend = T)
  lpoints<-split(points,points$col_factor)
  p1<-p


  for(i in seq_along(lpoints)) {
    pts<-lpoints[[i]]
    hover_text <- paste0(
      paste(pts$z_name[1],pts$z,sep=":"),
      "\n",
      paste(pts$size_var[1],pts$size_value,sep=":"),
      "\n",
      paste(pts$color_var,pts$col_zvalue,sep=":")
    )
    p1<-plotly::add_trace(
      p1,
      data = pts,
      x = pts$x,
      y =pts$y,
      z=pts$z,
      type = 'scatter3d',
      mode = 'markers',
      name= names(lpoints)[[i]],
      marker=list(

        size=pts$size*5,
        color = pts$color ,
        line = list(
          width = 0
        )
      ),
      text = hover_text, # Add custom hover text
      hoverinfo = 'text' # Specify that hover info should include custom text


    )


    #shape_args$base_shape_args$z<-min(pts$z)
    # shape_args$layer_shape_args$z<-min(pts$z)
    # p1<-add_shape_plotly(p1,vals$data_map,shape_args$base_shape_args,which_shape="base_shape")
    # p1<-add_shape_plotly(p1,vals$data_map,shape_args$layer_shape_args,which_shape="layer_shape")
  }


  if(!is.null(text)) {
    p1<-plotly::add_trace(
      p1,
      data = text,
      x = text$x,
      y =text$y,
      z=text$z,
      type = 'scatter3d',
      mode = 'text',
      text=text$labels,
      showlegend = F,
      textfont =list(
        size=text$size,
        color = text$color
      )

    )
  }
  p1<-plotly::layout(
    p1,
    title = list(
      text=main,
      font=list(
        size=plot.title_size
      )
    ),

    scene = list(
      zaxis = list(
        title=list(
          text=zlab,
          font=list(
            size=axis.title_size
          )
        ),
        tickfont=list(
          size=axis.text_size
        )

      ),
      xaxis=list(
        title=list(
          text=xlab,
          font=list(
            size=axis.title_size
          )
        ),
        tickfont=list(
          size=axis.text_size
        )
      ),
      yaxis=list(
        title=list(
          text=ylab,
          font=list(
            size=axis.title_size
          )
        ),
        tickfont=list(
          size=axis.text_size
        )
      )
    )
  )
  p1

}



crop_raters<-function(my_rst1,my_rst2){
  r2 <- raster::crop(my_rst1,extent(my_rst2))
  r3<-raster::crop(my_rst2,extent(my_rst1))
  extent(r2)<-extent(r3)
  r3e<-r3@ncols*r3@nrows
  r2e<-r2@ncols*r2@nrows
  if(r3e!=r2e){

    r23<-list(r2=r2,r3=r3)

    min_res<-which.min(c(r2e,r3e))
    max_res<-which.max(c(r2e,r3e))
    r_change<-r23[[max_res]]
    r_base<-r23[[min_res]]
    coarser=min_res
    e <- extent(r_base)
    r_new <- raster::raster(e, ncol=r_base@ncols, nrow=r_base@nrows)
    r_tochange<-raster::rasterToPoints(r_change)

    r_new2 <- raster::rasterize( r_tochange[,1:2], r_new, r_tochange[,3], fun=mean)
    r23[[max_res]]<-r_new2
    r2<-r23$r2
    r3<-r23$r3
  }
  s<-raster::stack(r2,r3)

  s

}



stack_plotly<-function(maps,pals,surface=F,sep_factor=1,xlab="x",ylab="y",zlab="z",main="",plot.title_size=12,axis.text_size=12,axis.title_size=12,shape_args=NULL,z_names=NULL,surf_exp=0.5,...){
  if(isFALSE(surface)){
    surf_exp<-1
  }


  if(is.null(z_names)){
    z_names<-rep("",length(maps))
  }
  fac_maps<-sapply(maps,function(x){
    ncol( x@data@attributes[[1]])==2
  })
  maps2<-resize_rasters(maps)

  p <- plotly::plot_ly(showscale = FALSE)
  i=2
  init=0

  init_i<-1
  init_m<-c(init_i+sep_factor)
  r_ranges<-list()
  r_ranges[[1]]<-c(init_i,init_m)
  for(i in 2:length(maps)){
    newmin<-(max(r_ranges[[i-1]])+sep_factor)
    newmax<-(newmin+surf_exp)
    r_ranges[[i]]<-c(newmin,newmax)}




  y_colorbar<-seq(0.05,.8,len=length(maps))
  for(i in seq_along(maps2)){
    map<-maps2[[i]]
    xyz<-get_rst_xyz_plotly(map)
    z_color<-xyz$z
    xyz$z<-z1<-scales::rescale(xyz$z,c(r_ranges[[i]][[1]],r_ranges[[i]][[2]]))


    zmap<-xyz$z

    if(isTRUE(fac_maps[i])|isFALSE(surface)){
      zmap<-scales::rescale(xyz$z,c(r_ranges[[i]][[1]],r_ranges[[i]][[1]]))
    }

    color_pal<-pals[[i]]
    p<-plotly::add_trace(
      p,z=zmap,
      x=xyz$x,
      y=xyz$y,
      surfacecolor =z_color,
      type = 'surface',
      showscale = T,
      colorscale = list(c(seq(0,1,len=length(color_pal))),color_pal),
      colorbar=list(
        title=list(
          text=z_names[[i]],
          font=list(
            size=8
          )
        ),
        tickfont=list(
          size=8
        ),
        orientation="v",
        y=y_colorbar[[i]],
        yanchor="bottom"
      ),
      contours =list(
        z=list(
          usecolormap=T
        ),
        y=list(
          usecolormap=T
        ),
        x=list(
          usecolormap=T,
          color="red",
          highlight=T
        )
      )


    )
    shape_args$base_shape_args$z<-r_ranges[[i]][[1]]
    shape_args$layer_shape_args$z<-r_ranges[[i]][[1]]
    # p<-add_shape_plotly(p,maps[[i]],shape_args$base_shape_args,which_shape="base_shape")
    p<-add_shape_plotly(p,maps[[i]],shape_args$layer_shape_args,which_shape="layer_shape")


  }
  xyz<-get_rst_xyz_plotly(maps2[[1]])
  x<-xyz$x
  y<-xyz$y
  z<-c(1,max(unlist(r_ranges)))
  if(isTRUE(surface)){
    z<-c(1,max(unlist(r_ranges)))
  }

  p<-plotly::layout(
    p,
    title = list(
      text=main,
      font=list(
        size=plot.title_size
      )
    ),

    scene = list(
      zaxis = list(
        #range=range(z),

        title=list(
          text=zlab,
          font=list(
            size=axis.title_size
          )
        ),
        tickfont=list(
          size=axis.text_size
        )

      ),
      xaxis=list(
        range=range(x),
        title=list(
          text=xlab,
          font=list(
            size=axis.title_size
          )
        ),
        tickfont=list(
          size=axis.text_size
        )
      ),
      yaxis=list(
        range=range(y),
        title=list(
          text=ylab,
          font=list(
            size=axis.title_size
          )
        ),
        tickfont=list(
          size=axis.text_size
        )

      )
      #spectratio = list(z = exp)
    )
  )
  return( p)

}
# my_rst1<-args$my_rst1
# my_rst2<-args$my_rst2
surface_plotly<-function(
    my_rst1,my_rst2,addpoints,colors,exp=0.5,theta,phi,r,custom_breaks,
    xlab='x',
    ylab="y",
    zlab="z",
    axis.title_size=1,
    axis.text_size=1,
    main="",
    plot.title_size=1,
    shape_args=NULL,
    addtext=NULL,

    ...){

  my_rst1_0<-my_rst1
  stack_rasters<-crop_raters(my_rst1,my_rst2)
  my_rst1<-stack_rasters[[1]]
  my_rst2=stack_rasters[[2]]

  rst<-my_rst1

  zval<-rst@data@values
  z=matrix(zval,ncol=rst@nrows,nrow=rst@ncols)
  z<-apply(z,1,rev)
  x<-seq(rst@extent[1],rst@extent[2],len=ncol(z))
  y<-seq(rst@extent[3],rst@extent[4],len=nrow(z))
  if(!is.null(addpoints))
    colnames(addpoints)[1:4]<-c("x",'y',"z","col")



  rst2<-my_rst2
  #rst2<-crop_raters(my_rst1,my_rst2)
  z2val<-rst2@data@values
  z2=matrix(z2val,ncol=rst2@nrows,nrow=rst2@ncols)
  z2<-apply(z2,1,rev)

  color_pal<-colorRampPalette(colors)(100)


  p<-plotly::plot_ly(x = x, y = y, z =z)

  p<-add_shape_plotly(p,my_rst1_0,shape_args$base_shape_args,which_shape="base_shape")
  p<-add_shape_plotly(p,my_rst1_0,shape_args$layer_shape_args,which_shape="layer_shape")
  p<-plotly::add_surface(p,
                         surfacecolor =z2,
                         colorscale = list(c(seq(0,1,len=100)),color_pal))
  p

  #p<-plotly::add_trace(p,x = x, y = y,z=z2,colorscale = list(c(seq(0,1,len=100)),rev(color_pal)))

  p<-plotly::add_trace(p,
                       data = addpoints,
                       x = addpoints$x,
                       y =addpoints$y,
                       z=addpoints$z,
                       type = 'scatter3d',
                       mode = 'markers',
                       marker=list(
                         size=addpoints$size*5,
                         color = addpoints$col,
                         line = list(
                           width = 0
                         )

                       )

  )
  text=addtext
  if(!is.null(text)) {
    p<-plotly::add_trace(
      p,
      data = text,
      x = text$x,
      y =text$y,
      z=text$z,
      type = 'scatter3d',
      mode = 'text',
      text=text$label,
      showlegend = F,
      textfont =list(
        size=text$size,
        color = text$color
      )

    )
  }


  cam<-get_camera(theta-80,
                  phi+25,
                  r)
  p<-plotly::layout(
    p,
    title = list(
      text=main,
      font=list(
        size=plot.title_size
      )
    ),

    scene = list(
      zaxis = list(
        range=range(z),
        tickvals = custom_breaks,
        title=list(
          text=zlab,
          font=list(
            size=axis.title_size
          )
        ),
        tickfont=list(
          size=axis.text_size
        )

      ),
      xaxis=list(
        range=range(x),
        title=list(
          text=xlab,
          font=list(
            size=axis.title_size
          )
        ),
        tickfont=list(
          size=axis.text_size
        )
      ),
      yaxis=list(
        range=range(y),
        title=list(
          text=ylab,
          font=list(
            size=axis.title_size
          )
        ),
        tickfont=list(
          size=axis.text_size
        )

      ),
      camera = list(eye = list(x = cam$x, y = cam$y, z = cam$z)),
      aspectratio = list(z = exp))
  )
  p
}


get_camera<- function(theta, phi, r) {

  # Convertendo os ângulos de graus para radianos
  theta_rad <- theta * pi / 180
  phi_rad <- phi * pi / 180

  # Calculando as coordenadas cartesianas
  x <- r * sin(phi_rad) * cos(theta_rad)
  y <- r * sin(phi_rad) * sin(theta_rad)
  z <- r * cos(phi_rad)

  # Retornando uma lista com as coordenadas
  list(x = x, y = y, z = z)
}
resize_rasters<-function(maps){

  names(maps)<-paste0("map_",1:length(maps))
  min_res<-which.min(
    sapply(maps,function(x){
      nrow(x)*ncol(x)
    }))
  fine_res<-maps[[min_res]]


  to_decrease<-maps[-min_res]


  to_decrease<-lapply(to_decrease,function(x){
    r2 <- raster::crop(x,extent(fine_res))
    extent(x)<-extent(fine_res)
    x
  })

  r_new <- raster::raster(extent(fine_res), ncol=fine_res@ncols, nrow=fine_res@nrows)

  map_decreased<-lapply(to_decrease,function(x){

    if(ncol(x@data@attributes[[1]])==2){
      fun=function(x,...){
        as.numeric(names(which.max(table(x))))
      }
    } else{
      fun=mean
    }
    r_tochange<-raster::rasterToPoints(x)
    r_new2 <- raster::rasterize( r_tochange[,1:2], r_new, r_tochange[,3], fun=fun)
    r_new2
  })
  names(map_decreased)<-names(to_decrease)
  new_maps<-c(map_decreased,list(fine_res))

  names(new_maps)[length(new_maps)]<-names(maps)[min_res]

  new_maps<-new_maps[names(maps)]
  new_maps
}
get_rst_xyz_plotly<-function(rst){
  zval<-rst@data@values
  z=matrix(zval,ncol=rst@nrows,nrow=rst@ncols)
  z<-apply(z,1,rev)
  x<-seq(rst@extent[1],rst@extent[2],len=ncol(z))
  y<-seq(rst@extent[3],rst@extent[4],len=nrow(z))
  list(x=x,y=y,z=z)
}


get_xyz_persp3D<-function(my_rst1){
  r1<-raster::as.matrix(my_rst1,transpose=F)
  ex<-extent(my_rst1)
  z<-t(apply(r1,2,rev))
  x<-seq(ex[1],ex[2],len=nrow(z))
  y<-seq(ex[3],ex[4],len=ncol(z))
  return(list(
    x=x,y=y,z=z
  ))
}
stack_plot3D<-function(maps,z_names=NULL,pals,  sep_factor=2,surf_exp=3,shape_args=NULL,surface=F,main="",
                       exp=0.2,tictype='detailed',theta=0,phi=40,r=1.73,d=1,xlab="",ylab="",zlab="",plot.title_size=1,axis.title_size=1,axis.text_size=1,...){



  if(is.null(z_names)){
    z_names<-rep("",length(maps))
  }
  fac_maps<-sapply(maps,function(x){
    ncol( x@data@attributes[[1]])==2
  })
  maps2<-resize_rasters(maps)
  names(args)
  xyz1<-get_xyz_persp3D(maps[[1]])

  n<-length(maps)

  init_i<-1
  init_m<-c(init_i+sep_factor)
  r_ranges<-list()
  r_ranges[[1]]<-c(init_i,init_m)
  for(i in 2:length(maps)){
    newmin<-max(r_ranges[[i-1]])+sep_factor
    newmax<-newmin+surf_exp
    r_ranges[[i]]<-c(newmin,newmax)}



  len_colkeys=1/(n*2)

  zlim<-c(1,max(unlist(r_ranges))-1)
  if(isTRUE(surface)){
    zlim<-c(1,max(unlist(r_ranges))+1)
  }
  pmat<-plot3D::perspbox(x=xyz1$x,y=xyz1$y,z=1, exp=exp,ticktype=tictype,theta=theta,phi=phi,r=r,d=d,xlab=xlab,ylab=ylab,zlim=zlim,
                         zlab=zlab, main=main,cex.main=plot.title_size/12,cex.axis=axis.text_size/12,cex.lab=axis.title_size/12)



  leg_positions<-  seq(-.2,.2,len=n)
  clab_positions<-  seq(-.2,.2,len=n)
  for(i in 1:n){
    xyz<-get_xyz_persp3D(maps2[[i]])
    z2<-xyz$z
    if(isTRUE(fac_maps[i])|isFALSE(surface)){
      z1<-r_ranges[[i]][[1]]
    } else{
      z1<-scales::rescale(xyz$z,c(r_ranges[[i]][[1]],r_ranges[[i]][[2]]))
    }
    colfunc<-colorRampPalette(pals[[i]])
    layer_args<-shape_args$layer_shape_args
    layer_args$z<-min(z1)
    add_3dpolygon(maps2[[i]],layer_args, pmat,which_shape="layer_shape")

    plot3D::persp3D(x=xyz$x,y=xyz$y,z=z1, colvar=z2, col=colfunc(length(z2)),border=NA,exp=exp,ticktype=tictype,theta=theta,phi=phi,r=r,d=d,xlab=xlab,ylab=ylab,add=T,clab=attr(maps[[i]],'z_name'),NAcol=NA,
                    colkey=list(
                      line.clab=0.2,
                      length=len_colkeys,
                      shift=leg_positions[[i]],
                      cex.clab=0.5,
                      lwd=0,
                      font=1,
                      #col.axis = "white",
                      col.box = "white",
                      tcl =1,
                      lwd.ticks=0,
                      cex.axis=0.5
                    ))

  }
}

get_polygon_coordinates<-function(shape){
  result<-lapply(shape$geometry,function(x) {
    lapply(x,function(xx){
      xx[[1]]
    })
  })
  result<-unlist(result,recursive=F)
  result
}

add_shape_plotly<-function(p,my_rst1,shape_args,which_shape="layer_shape"){
  if(isFALSE(shape_args$shape)){
    return(p)
  }

  shape<-attr(my_rst1,which_shape)
  if(is.null(shape)){
    return(p)
  }
  ext<-extent(shape)
  rpoly<-raster::raster(ext,  nrows=360,ncols=360)
  r_shape<-rasterize(to_spatial(data.frame(raster::coordinates(rpoly))),rpoly,shape_args$z)
  rp0<-mask( raster::crop(r_shape, ext), shape)


  zp0=matrix(rp0@data@values,ncol=rp0@nrows,nrow=rp0@ncols)
  np_new<-zp0
  np_new[]
  zp<-apply(zp0,1,rev)
  color_matrix <- matrix(1, nrow = nrow(zp), ncol = ncol(zp))
  xp=seq(rp0@extent[1],rp0@extent[2],len=ncol(zp))
  yp=seq(rp0@extent[3],rp0@extent[4],len=nrow(zp))
  color<-shape_args$color
  p1<-plotly::add_trace(p,x=xp,y=yp,
                        type = 'surface', z = zp,
                        surfacecolor = color_matrix,
                        colorscale = list(c(0, 1), c(color, color)),
                        showscale = FALSE
  )

  p1
}

add_3dpolygon<-function(my_rst1,args_shape, pmat,which_shape="base_shape"){
  if(isTRUE(args_shape$shape)) {
    shape<-attr(my_rst1,which_shape)
    fill<-args_shape$fill
    if(isTRUE(fill)){
      fill_color<-args_shape$color
    } else{
      fill_color<-NA
    }

    if(!is.null(shape)){
      poly_list<-get_polygon_coordinates(shape)
      poly_list<-poly_list[ sapply(sapply(poly_list,dim),length)>0]
      poly_list<-poly_list[sapply(poly_list,length)>0]
      lapply(poly_list, function(poly){

        polygon(trans3d(poly[,1] , y=poly[,2], z=args_shape$z,  pmat=pmat),col=fill_color, border=args_shape$border_col, lwd=args_shape$weight)
      })

    }


  }
}



get_4D<-function(my_rst1,my_rst2=NULL,colors,exp=0.2,wlegend=20,hlegend=20,tictype='detailed',shape_args=NULL,addpoints=NULL,addtext=NULL,legend=T,slices=F,bg_points=T,theta=0,phi=40,r=1.73,d=1,xlab="",ylab="",
                 zlab="",main=NULL,plot.title_size=1,
                 axis.title_size=1,
                 axis.text_size=1,
                 y.intersp=1,
                 inset_x=-0.25,
                 inset_y=0,
                 x.intersp,
                 ...)
{


  #graphics.off()
  col0<-colors
  my_rst2_0<-my_rst2
  stack_rasters<-crop_raters(my_rst1,my_rst2)
  my_rst1<-stack_rasters[[1]]
  my_rst2=stack_rasters[[2]]
  ex<-extent(my_rst1)
  r1<-raster::as.matrix(my_rst1,transpose=F)
  z1<-t(apply(r1,2,rev))
  r2<-raster::as.matrix(my_rst2,transpose=F)
  z2<-t(apply(r2,2,rev))
  x<-seq(ex[1],ex[2],len=nrow(z1))
  y<-seq(ex[3],ex[4],len=ncol(z1))
  colfunc = colorRampPalette(colors)
  color_apl<-colfunc(100)
  z.facet.center <- (z2[-1, -1] + z2[-1, -ncol(z2)] + z2[-nrow(z2), -1] + z2[-nrow(z2), -ncol(z2)])/4
  # Range of the facet center on a 100-scale (number of colors)
  z.facet.range<-cut(z.facet.center, 100, include.lowest=T)


  pmat<-plot3D::perspbox(x=x,y=y,z=z1, exp=exp,ticktype=tictype,theta=theta,phi=phi,r=r,d=d,xlab=xlab,ylab=ylab,zlim=range(z1[!is.na(z1)]),
                         zlab=zlab, main=main,cex.main=plot.title_size/12,cex.axis=axis.text_size/12,cex.lab=axis.title_size/12)


  base_args<-shape_args$base_shape_args
  layer_args<-shape_args$layer_shape_args
  add_3dpolygon(my_rst1,base_args, pmat,which_shape="base_shape")
  add_3dpolygon(my_rst1,layer_args, pmat,which_shape="layer_shape")



  plot3D::persp3D(x=x,y=y,z=z1, colvar=z2, col=colfunc(length(z2)),border=NA,exp=exp,ticktype=tictype,theta=theta,phi=phi,r=r,d=d,xlab=xlab,ylab=ylab,add=T,clab=attr(my_rst2_0,'z_name'))

  if(!is.null(addpoints)){
    if(isFALSE(bg_points)){
      points(trans3d(addpoints[,1] , y=addpoints[,2], z=addpoints[,3],  pmat=pmat), col=addpoints[,4], pch=16,cex=addpoints[,"size"])
    } else{
      pal<-colfunc(length(unique(addpoints[,3])))[cut(addpoints[,3],length(unique(addpoints[,3])))]
      points(trans3d(addpoints[,1] , y=addpoints[,2], z=addpoints[,3],  pmat=pmat),col=pal ,bg=addpoints[,4], pch=21,cex=addpoints[,"size"])
    }

  }

  if(!is.null(addtext)){
    text(trans3d(addtext$x , y=addtext$y, z=addtext$z,  pmat=pmat),col=addtext$color , cex=addtext$size/5, labels=addtext$label)

  }
  if(!is.null(addpoints)){
    zsize<-addpoints$size
    lev_legend<-levels(cut(zsize,length(unique(addpoints$color))))
    lev_legend<-gsub("\\(","",lev_legend)
    lev_legend<-gsub("\\]","",lev_legend)
    x<-lev_legend[1]
    lev_legend<-sapply(lev_legend, function(x) {
      paste0(rev(strsplit(x,",")[[1]]),collapse=" - ")
    })
    par(xpd=T)
    usr<-par("usr")

    y_legend<-seq(usr[2],usr[3],len=10)[1:length(lev_legend)]
    x_legend<-rep(usr[2],length(lev_legend))

    zval<-addpoints$size_value

    br<-unique(seq(min(zval),max(zval),len=5))

    cut_var<-cut(zval,breaks=br,include.lowest =T)

    libr<-split(zval,cut_var)
    libr<-libr[sapply(libr,length)>0]
    zval_legend<-as.character(sapply(libr,function(x) {
      paste0(paste0(rev(round(range(x,na.rm=T),2)),collapse=" - "))
    }))
    pt.cex=unique(seq(min(zsize,na.rm=T),max(zsize,na.rm=T),len=length(libr)))
    col=rev(unique(addpoints$color))
    legend=rev(zval_legend)
    legend("topright",legend=legend,pch=16,pt.cex=pt.cex,col=col,y.intersp=y.intersp,inset=c(inset_x,inset_y),bty="n",x.intersp=x.intersp)

  }



  return(pmat)

}

get_breaks_input <- function(z, n) {

  options(scipen = 999)  # Try to avoid scientific notation globally

  bbreaks <- unique(seq(min(z), max(z), length.out = n))

  if (length(bbreaks) == 1) {
    values <- bbreaks
  } else {
    cutted <- cut(na.omit(bbreaks), breaks = bbreaks, include.lowest = TRUE)

    # Convert cutted to numeric
    numeric_values <- as.numeric(cutted_to_numeric(cutted))

    # Apply formatting individually to each numeric value
    formatted_values <- sapply(numeric_values, function(val) {
      format(val, scientific = FALSE, trim = TRUE)
    })

    # Adjust first and last values
    formatted_values[1] <- format(floor_decimal(min(z, na.rm = TRUE), decimal_places(numeric_values)),
                                  scientific = FALSE, trim = TRUE)
    formatted_values[length(formatted_values)] <- format(ceiling_decimal(max(z, na.rm = TRUE), decimal_places(numeric_values)),
                                                         scientific = FALSE, trim = TRUE)

    values <- paste(formatted_values, collapse = ", ")
  }

  return(values)
}
ceiling_decimal <- function(x, decimal_places) {
  result <- ceiling(x * 10^decimal_places) / 10^decimal_places
  result
}

floor_decimal<-function(x,decimal_places){
  result <- floor(x * 10^decimal_places) / 10^decimal_places
  result
}

resize_with_aspect_ratio <- function(new_width = 250, new_height = NULL, width = 200, height = 300) {
  aspect_ratio <- width / height

  if (is.null(new_height)) {
    new_height <- new_width / aspect_ratio
  } else {
    new_width <- new_height * aspect_ratio
  }

  return(list(new_width = new_width, new_height = new_height))
}
Pixel_to_cm <- function(pixels = 250, dpi = 96) {
  # Convert DPI to DPCM (dots per centimeter)
  dpcm <- dpi / 2.54

  # Convert pixels to centimeters
  cm <- pixels / dpcm

  return(cm)
}




##






#p<-readRDS("p.rds")
#data<-readRDS("savepoint.rds")$saved_data$nema_araca
#axis_width=0.01




add_pie_chart<-function(map,data,factor_chart,buffer_zize,fun, min_radius,max_radius, pal,light=0){
  max_radius<-max_radius*2
  min_radius=min_radius*2
  df0<-get_chart_data(data,factor_chart, distance=buffer_zize, fun=fun)
  df<-data.frame(df0)
  colnames(df)<-colnames(df0)

  popup<-if(is.factor(data[,1])){
    leaflet.minicharts::popupArgs(showTitle =F,showValues =F,supValues =get_popup_chart(df))
  } else{
    leaflet.minicharts::popupArgs()
  }
  facddf<-df[-c(1:2)]
  fac<-attr(data,"factors")[factor_chart]
  colors <- colorspace::lighten(colorRampPalette(pal)(ncol(facddf)),light)
  if(is.factor(data[,1])){
    colors=   colorspace::lighten(get_cols_factor_chart(df,data, pal),light)
    width=max_radius
  } else{
    width = scales::rescale(rowSums(facddf),c(min_radius,max_radius))
  }
  colnames(facddf)<-gsub("_split_",".",colnames(facddf))
  map |> leaflet.minicharts::addMinicharts(
    lng = df[,1],
    lat = df[,2] ,
    type="pie",
    chartdata = facddf,
    #layerId=fac[,1],
    colorPalette =colors,
    popup = popup,
    legend =T,
    width=width
  )

}

map_discrete<-function(data, pal=viridis(100),nbreaks=5,min_radius=1,max_radius=5,scale_radius=F,fillOpacity=0.8, providers="Esri.WorldTopoMap", addCircles=T,addMinicharts=F,factor_chart=2,buffer_zize=50, fun="sum",base_shape_args=NULL,layer_shape_args=NULL, rst=NULL,args_extra_shape=NULL,args_labels=NULL,newcolhabs=NULL,palette=NULL,custom_breaks=NULL,light=0,zoomSnap=0.25,...){
  palette0<-palette
  colors00<-colorspace::lighten(newcolhabs[[palette]](256),light)
  newcolhabs[[palette]]<-colorRampPalette(colors00)
  pal<-colorspace::lighten(pal,light)


  if(is.factor(data[,1])){
    max_radius<-max_radius-2
  } else{
    if(!is.null(rst)){
      var<-colnames(data)
      data<-get_data_rst(rst)
      colnames(data)<-var
    }

  }


  max_radius<-max_radius*3
  min_radius=min_radius*3
  radius<-max_radius
  coords<-attr(data,"coords")
  if(class(coords)[1]=="matrix"){
    coords<-data.frame(coords)
  }
  colnames(coords)<-c("x","y")
  var<-colnames(data)
  df<-na.omit(cbind(coords,z=data[,var]))
  coords<-df[,1:2]
  data<-data[rownames(coords),,drop=F]
  if(is.factor(df[,3])){
    cols<-newcolhabs[[palette]](nlevels(df[,3]))
    cols<-cols[levels(df[,3])%in%df[,3]]
    palette <- leaflet::colorFactor(cols,domain = factor(df[,3]))
    radius<-radius
  } else{
    radius<-if(isTRUE(scale_radius)){scales::rescale(df$z,c(min_radius,radius))}else{radius}

    custom_breaks<-as.numeric(custom_breaks)
    if(length(custom_breaks)<=2){
      custom_breaks=2
      palette <- leaflet::colorFactor(pal,domain = factor(df[,3]))
    } else{
      palette <- leaflet::colorBin(pal,domain = df[,3], bins = custom_breaks)
    }

  }

  map <- leaflet::leaflet(df, options =leaflet::leafletOptions(
    zoomSnap=zoomSnap,
    zoomControl =T,
    zoomDelta = 1
  )  )

  map<-add_shapes1(map,data,layer_shape_args,base_shape_args)

  #map<-add_extraLL(map,data,args_extra_shape)

  if(isTRUE(addCircles)){
    map <- map |> leaflet::addCircleMarkers(
      lng = ~x,
      lat = ~y ,
      fillOpacity = fillOpacity,
      stroke =F,
      radius =radius,
      color = ~palette(z),
      fillColor = ~palette(z),
      popup = ~paste(var, z),
      group="circles"
    )
  }

  if(isTRUE(addMinicharts)){
    map<-add_pie_chart(map,data,factor_chart,buffer_zize,fun, min_radius,max_radius, pal,light)
  }
  map <- map %>% leaflet::addTiles() %>%
    leaflet::addProviderTiles(providers)


  map0<-map
  if(!is.null(rst)){
    if(!is.factor(df[,3])){

      pal <- leaflet::colorNumeric(pal,domain = df[,3], na.color ='#00000000')


    } else{
      colors_factor<-newcolhabs[[palette0]](nlevels(df[,3]))

      pal<-colors_factor[levels(df[,3])%in%
                           rst@data@attributes[[1]][,2]]
    }


    map <- map |> leaflet::addRasterImage(rst, colors = pal, opacity = fillOpacity, group="raster")
  }



  if(!is.null(args_labels)){
    if(isTRUE(args_labels$show_labels)){
      if(!is.null(args_labels$labels)){
        labels<-attr(data,"factors")[,args_labels$labels]
        map<- map |>
          leaflet::addLabelOnlyMarkers(
            lng = df$x,
            lat = df$y, label =  ~as.character(labels),
            group="labels",
            labelOptions = leaflet::labelOptions(
              style =list('color'=args_labels$col.fac),
              direction = 'center',

              noHide = T,
              textOnly = T,
              textsize=paste0(args_labels$cex.fac*2,"px"))
          )}

    }}


  if(!isTRUE(addMinicharts)){
    map <- map |> leaflet::addLegend("bottomright",
                                     pal = palette,
                                     values = ~z,
                                     title = var,
                                     opacity = 1)
  }
  overlayGroups<-control_layers(addCircles,addMinicharts,rst,base_shape_args,layer_shape_args,args_labels)
  map <- map |> leaflet::addLayersControl(overlayGroups = overlayGroups)
  map
}
gg_add_extra_shape<-function(p, rst,extra_shape_args  ){
  if(is.null(extra_shape_args)){
    return(p)
  }
  extralayers<-data.frame(extra_shape_args$args_extra)


  extralayers$layers<-as.logical(extralayers$layers)

  extras<-attr(rst,"extra_shape")
  if(is.null(extras)){
    return(p)
  }
  if(!any(extralayers$layers)){
    return(p)
  }
  if(any(extralayers$layers)){
    extralayers<-subset(extralayers,isTRUE(layers))

    for(i in 1:length(  extralayers$layers)){
      col_extra<-extralayers$colors[i]
      p<-p+geom_sf(data=st_as_sf(extras[[i]]), col=mylighten(col_extra,as.numeric(extralayers$alphas[i])), lty=1)
      names( p$layers)[length( p$layers)]<-paste0("extra",i)
    }
    return(p)
  }
}

extra_coords<-function(args_extra_shape,extra_shape){
  if(is.null(extra_shape)){
    return(NULL)
  }
  if(is.null(args_extra_shape)){
    return(NULL)
  }
  if(is.null(args_extra_shape$args_extra)){
    return(NULL)
  }


  # args_extra_shape<-args$args_extra_shape
  args_extra_shape<-data.frame(args_extra_shape$args_extra)
  res<-lapply(1:nrow(args_extra_shape), function(i){

    if(args_extra_shape$layers[i]=="TRUE"){
      sf::st_coordinates(extra_shape[[i]])[,1:2]
    } else{
      NULL
    }
  })
  do.call(rbind,res)
}
get_all_coords<-function(data,base_shape_args,layer_shape_args,args_extra_shape){
  coords0<-coords<-attr(data,"coords")
  req(coords)
  coords<-st_as_sf(coords,coords=colnames(coords))
  base_shape<-attr(data,"base_shape")
  layer_shape<-attr(data,"layer_shape")
  extra_shape<-attr(data,"extra_shape")

  ec<-extra_coords(args_extra_shape,extra_shape)
  shapes<-list(coords=coords,
               base_shape=base_shape,
               layer_shape=layer_shape)
  shapes<-shapes[which(c(
    TRUE,
    base_shape_args$shape,
    layer_shape_args$shape
  ))]
  shapes<-c(shapes[-1],extra_shape)
  shapes<-shapes[sapply(shapes,length)>0]
  if(!length(shapes)>0){
    return(coords0)
  }
  shs<-lapply(shapes, st_union)
  shs_sf <- lapply(shs, st_as_sf)
  shs_sf <- lapply(shs_sf, st_convex_hull)
  shs_sf <- lapply(shs_sf, function(x) sf::st_coordinates(x)[,1:2])
  shs_sf<-do.call(rbind,shs_sf)
  colnames(shs_sf)<-colnames(coords0)
  shs_sf

}



get_mean_resolution<-function(coords){


  dx<-sqrt(diff(unique(coords[,1]))^2)
  res_x<-mean(dx)

  dy<-sqrt(diff(unique(coords[,2]))^2)
  res_y<-mean(dy)

  min_res<-min(res_x,res_y)
  max_res<-max(res_x,res_y)

  res <-  max(min_res,max_res)
  round(res,3)
}
map_rst<-function(data,base_shape_args,
                  layer_shape_args,
                  args_extra_shape, crs="+proj=longlat +datum=WGS84 +no_defs",resolution){

  coords<-attr(data,"coords")
  my_rst<-rst0(data,resolution)
  req(raster::nrow(my_rst)>1)
  req(raster::ncol(my_rst)>1)
  if(is.factor(data[,1])){
    my_rst<-rst_factor(my_rst,data)
  } else{
    my_rst<-rst_num(my_rst,data)
  }

  base_shape0<-st_as_sf(x =  coords,coords = c(colnames( coords)))
  base_shape0<-st_set_crs(base_shape0,crs)
  if(isTRUE(base_shape_args$shape)){
    base_shape<-attr(data,'base_shape')
    if(!is.null(base_shape)) {
      lims<-get_limits2(data,base_shape_args,
                        layer_shape_args,
                        args_extra_shape)




      my_rst<-mask( raster::crop(my_rst, lims_to_extend(lims)), base_shape)

    }
  }

  crs(my_rst) <- sp::CRS(crs)
  attr(my_rst,"data_levels")<-levels(data[,1])
  # my_rst<-  raster::ratify(my_rst)
  my_rst
}
raster_gg<-function(data,raster_resolution=0.1,down_grade_raster=NULL,down_grade=NULL,base_shape_args,
                    layer_shape_args,
                    args_extra_shape){



  args<-list(data=data, cut_shape = cut_shape, resolution=raster_resolution,base_shape_args=base_shape_args,
             layer_shape_args=layer_shape_args,
             args_extra_shape=args_extra_shape)
  rst<-do.call(map_rst,args)
  rst<-migrate_rst(rst, data)
  if(isTRUE(down_grade_raster)){
    rst <- raster::aggregate(rst, fact=down_grade)


  }
  rst
}
get_limits<-function(limits,base_shape,layer_shape,coords){
  shapes<-get_shapes(base_shape, layer_shape, coords)
  base_shape<-shapes$base_shape
  crs.info=shapes$crs.info
  if(is.null(limits)){
    limits<-st_bbox(base_shape)}
  xlimits<-limits[c(1,3)]
  ylimits<-limits[c(2,4)]
  limits<-cbind(xlimits,ylimits)
  colnames(limits)<-colnames(coords)
  attr(limits,"crs.info")<-crs.info
  return(limits)
}
lims_to_extend<-function(lims,as_extent=T){
  dflim<-rbind(c(lims$xmin,lims$ymin),c(lims$xmin,lims$ymax),c(lims$xmax,lims$ymax),c(lims$xmax,lims$ymin))

  dflim<-data.frame(matrix(dflim,dimnames = list(1:4,c("x","y")),ncol=2))
  if(isTRUE(as_extent)){
    limits<-extent(raster(to_spatial(dflim)))
  } else{
    xlimits<-lims[c(1,3)]
    ylimits<-lims[c(2,4)]
    limits<-cbind(xlimits,ylimits)
    limits<-data.frame(matrix(unlist(limits),ncol=2))

    colnames(limits)<-c("x","y")
  }
  limits


}

get_idw_leaf<-function(data, base_shape_args,
                       layer_shape_args,
                       args_extra_shape, resolution=3000,crs.info="+proj=longlat +datum=WGS84 +no_defs",...){
  base_shape<-attr(data,"base_shape")
  layer_shape<-attr(data,"layer_shape")


  coords<-attr(data,"coords")
  data<-na.omit(data)
  limits<-get_limits2(data,base_shape_args,
                      layer_shape_args,
                      args_extra_shape)

  limits<-lims_to_extend(limits,F)
  colnames(limits)<-colnames(coords)
  coords<-coords[rownames(data),]
  colnames(coords)<-c("x","y")
  newgrid<-get_grid(coords,limits,crs.info,resolution)
  data_spat<-to_spatial(data.frame(cbind(coords,z=data[,1])))
  interp_model=df_idw = gstat::idw(z~1, data_spat, newgrid)
  df_raster <- raster::raster(df_idw)
  if(isTRUE(base_shape_args$shape)){
    if(!is.null(base_shape)){
      df_raster<-  mask(df_raster, base_shape)
    }
  }
  df_raster<-  raster::ratify(df_raster)
  return(df_raster)
}
#' @export
get_knn_leaf<-function(data,k=5,base_shape_args,
                       layer_shape_args,
                       args_extra_shape,resolution){
  coords<-attr(data,"coords")
  base_shape<-attr(data,"base_shape")
  layer_shape<-attr(data,"layer_shape")
  limits<-get_limits2(data,base_shape_args,
                      layer_shape_args,
                      args_extra_shape)

  limits<-lims_to_extend(limits,F)
  colnames(limits)<-colnames(coords)
  coords<-attr(data,"coords")
  data<-na.omit(data)
  coords<-coords[rownames(data),]
  newgrid<-get_grid(coords,limits,crs.info,resolution)
  z=data[,1]
  m<-caret::train(x=coords,y=z,method="knn",trControl =caret::trainControl(method="cv"),kmax =k)
  newdata<-data.frame(newgrid)
  colnames(newdata)<-colnames(coords)
  pred<-predict(m,newdata=newdata)
  dfs<-data.frame(newdata,pred)
  colnames(dfs)[3]<-colnames(data)
  pred_spat<-to_spatial(dfs)
  knn_raster <- raster::rasterFromXYZ(dfs)
  rst<-cut_shape_fun(knn_raster,T,base_shape)
  rst<-raster::ratify(rst)
  raster::crs(rst)<-crs.info

  return(rst)

}


get_caret_leaf<-function(data,base_shape_args,
                         layer_shape_args,
                         args_extra_shape,resolution,tuneLength=5,method="knn",seed=NA,crs.info){
  coords<-attr(data,"coords")
  base_shape<-attr(data,"base_shape")
  layer_shape<-attr(data,"layer_shape")

  limits<-get_limits2(data,base_shape_args,
                      layer_shape_args,
                      args_extra_shape)

  limits<-lims_to_extend(limits,F)
  colnames(limits)<-colnames(coords)

  data<-na.omit(data)
  coords<-coords[rownames(data),]
  newgrid<-get_grid(coords,limits,crs.info,resolution)
  z=data[,1]
  if(is.na(seed)){
    set.seed(NULL)
  } else{
    set.seed(seed)}
  if(is.factor(data[,1])){
    y<-as.character(z)
  } else{
    y<-z
  }
  m<-caret::train(x=coords,y=y,method=method,trControl =caret::trainControl(method="cv"),tuneLength  =tuneLength )
  newdata<-data.frame(newgrid)
  colnames(newdata)<-colnames(coords)
  if(method=="gaussprRadial"){
    if(is.factor(data[,1])){
      out <-predict(m, newdata, type="prob")
      pred<-kohonen::classmat2classvec(out)
    } else{
      pred<-predict(m,newdata=newdata)
    }
  } else{
    pred<-predict(m,newdata=newdata)
  }


  dfs<-data.frame(newdata,pred)
  colnames(dfs)[3]<-colnames(data)
  pred_spat<-to_spatial(dfs)
  if(is.factor(data[,1])){
    levs<-levels(dfs[,3])
    dfs[,3]<-as.numeric(dfs[,3])

  }

  knn_raster <- raster::rasterFromXYZ(dfs)


  knn_raster<-raster::ratify(knn_raster)
  if(is.factor(data[,1])){



    knn_raster@data@values<-factor(knn_raster@data@values, labels=levels(pred), levels=1:nlevels(pred))
    knn_raster@data@attributes[[1]]$levels<-levs[  knn_raster@data@attributes[[1]]$ID]
  }



  rst<-cut_shape_fun(knn_raster,base_shape_args$shape,base_shape)
  raster::crs(rst)<-crs.info
  attr(rst,"m")<-m

  return(rst)

}
#' @export
predict_cokrige2<-function(g,newdata=NULL, crs.info="+proj=longlat +datum=WGS84 +no_defs",resolution=500,base_shape_args,
                           layer_shape_args,
                           args_extra_shape){
  coords<-attr(newdata,"coords")
  base_shape<-attr(newdata,"base_shape")
  layer_shape<-attr(newdata,"layer_shape")
  limits<-get_limits2(newdata,base_shape_args,
                      layer_shape_args,
                      args_extra_shape)

  limits<-lims_to_extend(limits,F)
  colnames(limits)<-colnames(coords)
  newgrid<-get_grid(coords,limits,crs.info,res=resolution)
  grd<-to_spatial(data.frame(newgrid),crs.info=crs.info)
  k.c <- predict(g, grd)
  #pred1<-predict(g, newdata =newgrid )
  result<-data.frame(k.c)
  xyz<-result[1:3]
  colnames(xyz)[3]<-colnames(newdata)
  rst<-raster::rasterFromXYZ(xyz)
  rst<-cut_shape_fun(rst,base_shape_args$shape,base_shape)
  rst<-raster::ratify(rst)
  raster::crs(rst)<-crs.info
  rst
}
gstat_predictions<-function(g,data,resolution, crs.info="+proj=longlat +datum=WGS84 +no_defs",base_shape_args,layer_shape_args,args_extra_shape){
  req(g)
  newdata<-data
  args_pred<-list(g=g,newdata=newdata, crs.info=crs.info,resolution=resolution,base_shape_args,layer_shape_args,args_extra_shape)
  pred<-do.call(predict_cokrige2,args_pred)
  pred
}

interp_leaflet2<-function(data, base_shape_args,
                          layer_shape_args,
                          args_extra_shape, resolution=3000,crs.info="+proj=longlat +datum=WGS84 +no_defs",k=5,interp_type="knn",g=NULL,seed=NA,tuneLength=NULL,...){



  if(!interp_type%in%c("krige","idw")){
    method=interp_type
    rst_result<-get_caret_leaf(data,base_shape_args=base_shape_args,
                               layer_shape_args=layer_shape_args,
                               args_extra_shape=args_extra_shape,resolution,tuneLength,method,seed,crs.info)
  } else if(interp_type=="idw"){
    rst_result<- get_idw_leaf(data, base_shape_args=base_shape_args,
                              layer_shape_args=layer_shape_args,
                              args_extra_shape=args_extra_shape, resolution,crs.info="+proj=longlat +datum=WGS84 +no_defs",...)
  } else if(interp_type=="krige"){
    rst_result<-gstat_predictions(g,data,resolution, crs.info="+proj=longlat +datum=WGS84 +no_defs",base_shape_args=base_shape_args,
                                  layer_shape_args=layer_shape_args,
                                  args_extra_shape=args_extra_shape)

  }

  rst<-migrate_rst(rst_result, data)
  attr(rst,"data_levels")<-levels(data[,1])
  rst




}
interp_gg<-function(data,interp_args,base_shape_args,
                    layer_shape_args,
                    args_extra_shape,interp_gstat,custom_breaks=NULL) {
  req(interp_args)
  args<-interp_args
  req(args)
  args$data<-data


  args$g<-interp_gstat
  args$base_shape_args<-base_shape_args
  args$layer_shape_args<-layer_shape_args
  args$args_extra_shape<-args_extra_shape

  rst<- do.call(capture_log2(interp_leaflet2),args)
  if(is.numeric(data[,1])){
    custom_breaks=custom_breaks
    rst@data@values<- scales::rescale( rst@data@values,c(min(custom_breaks),max(custom_breaks)))
  }

  req(!inherits(rst,"error"))
  attr(rst,"method")<-args$interp_type

  rst


}
get_limits2<-function(data,base_shape_args,
                      layer_shape_args,
                      args_extra_shape){

  coords<-attr(data,"coords")
  req(coords)
  coords<-st_as_sf(coords,coords=colnames(coords))
  base_shape<-attr(data,"base_shape")
  layer_shape<-attr(data,"layer_shape")
  extra_shape<-attr(data,"extra_shape")

  extra_limits<-function(args_extra_shape,extra_shape){
    if(is.null(extra_shape)){
      return(NULL)
    }
    if(is.null(args_extra_shape)){
      return(NULL)
    }
    if(is.null(args_extra_shape$args_extra)){
      return(NULL)
    }


    args_extra_shape<-data.frame(args_extra_shape$args_extra)
    res<-lapply(1:nrow(args_extra_shape), function(i){

      if(args_extra_shape$layers[i]=="TRUE"){
        st_bbox(extra_shape[[i]])
      } else{
        NULL
      }
    })
    do.call(rbind,res)
  }
  el<-extra_limits(args_extra_shape,extra_shape)
  shapes<-list(coords=coords,
               base_shape=base_shape,
               layer_shape=layer_shape)
  shapes<-shapes[which(c(
    TRUE,
    base_shape_args$shape,
    layer_shape_args$shape
  ))]
  shapes<-shapes[sapply(shapes,length)>0]
  limits<-data.frame(do.call(rbind,lapply(shapes,st_bbox)))
  limits<-rbind(limits,el)
  c(lapply(limits[1:2],min),lapply(limits[3:4],max))

}

gg_pie<-function(p,rst,factor_chart,buffer_zize,fun,min_radius,max_radius,newcolhabs,pal,reverse_palette, fillOpacity, light,leg_title=NULL){
  pie_data<-get_pie_gg(rst,factor_chart,buffer_zize,fun,min_radius,max_radius,newcolhabs, pal,fillOpacity,light=light)

  size<-rowSums(pie_data[,-c(1:2)])
  size<-scales::rescale(scales::rescale(size,c(min_radius,max_radius)),c(0,2))/5
  if(is.factor(rst[,1])){
    colpie<-attr(pie_data,"color")
  } else{
    colpie<-newcolhabs[[pal]](ncol(pie_data))
  }
  colpie<-adjustcolor(colorspace::lighten(colpie,light),fillOpacity)


  if(isTRUE(reverse_palette)){
    colpie<-rev(colpie)
  }
  pie_data$size<-size
  y<-pie_data$y
  x<-pie_data$x

  p<-p+ scatterpie::geom_scatterpie(mapping=aes(x=x, y=y
                                    #,r=size
  ), data=pie_data,
  cols=colnames(pie_data)[-c(1:2,ncol(pie_data))],
  color=NA)

  p<-p+
    scale_fill_manual(name=leg_title,values = colpie,drop=F)
  p
}
#' @export
position_barscale<-function(bar_scale,position,pxrange,pyrange,n_bins,pad_x,pax_y){
  xs<-c(unlist(bar_scale[,c('xmin','xmax')]))
  ys<-c(unlist(bar_scale[,c('ymin','ymax')]))
  move_x<-(max(pxrange)-min(pxrange))*pad_x
  move_y<-(max(pyrange)-min(pyrange))*pax_y
  bs<-bar_scale
  if(grepl("bottom",position)){
    yss<-ys*min(pyrange)/min(ys)
    bs[,c('ymin','ymax')]<-  matrix(yss,nrow=n_bins)
    bs$ymin<-bs$ymin+move_y
    bs$ymax<-bs$ymax+move_y
  }
  if(grepl("top",position)){
    yss<-ys*max(pyrange)/max(ys)
    bs[,c('ymin','ymax')]<-  matrix(yss,nrow=n_bins)
    bs$ymin<-bs$ymin-move_y
    bs$ymax<-bs$ymax-move_y
  }
  if(grepl("left",position)){
    xss<-xs*min(pxrange)/min(xs)
    bs[,c('xmin','xmax')]<-  matrix(xss,nrow=n_bins)
    bs$xmin<-bs$xmin+move_x
    bs$xmax<-bs$xmax+move_x
  }
  if(grepl("right",position)){
    xss<-xs*max(pxrange)/max(xs)
    bs[,c('xmin','xmax')]<-  matrix(xss,nrow=n_bins)

    bs$xmin<-bs$xmin-move_x
    bs$xmax<-bs$xmax-move_x
  }
  bs
}

format_distance <- function(distance_meters) {
  if (distance_meters >= 1000) {
    step<-round(distance_meters / 1000, 2)
    attr(step,"unit")<-"km"
  } else if (distance_meters >= 1) {
    step<-round(distance_meters, 2)
    attr(step,"unit")<-"m"
  } else {
    step<-round(distance_meters * 100, 2)
    attr(step,"unit")<-"cm"
  }
  step
}
# Função para converter entre unidades
format_distance_to <- function(auto_formated_distance, to = "km") {
  from_unit <- attr(auto_formated_distance, "unit")

  if (from_unit == to) {
    return(auto_formated_distance)
  }

  # Conversão para a unidade desejada
  if (to == "km") {
    if (from_unit == "m") {
      converted_value <- auto_formated_distance / 1000
    } else if (from_unit == "cm") {
      converted_value <- auto_formated_distance / 100000
    }
    attr(converted_value, "unit") <- "km"

  } else if (to == "m") {
    if (from_unit == "km") {
      converted_value <- auto_formated_distance * 1000
    } else if (from_unit == "cm") {
      converted_value <- auto_formated_distance / 100
    }
    attr(converted_value, "unit") <- "m"

  } else if (to == "cm") {
    if (from_unit == "km") {
      converted_value <- auto_formated_distance * 100000
    } else if (from_unit == "m") {
      converted_value <- auto_formated_distance * 100
    }
    attr(converted_value, "unit") <- "cm"
  } else {
    stop("Unidade de destino não reconhecida")
  }

  return(converted_value)
}

step_scale_bar<-function(data,base_shape_args,
                         layer_shape_args,
                         args_extra_shape){

  args<-list(
    data=data,base_shape_args=base_shape_args,
    layer_shape_args=layer_shape_args,
    args_extra_shape=args_extra_shape
  )

  coords<-attr(data,"coords")
  base_shape<-attr(data,"base_shape")
  layer_shape<-attr(data,"layer_shape")
  extra_shape<-attr(data,"extra_shape")

  a<-get_limits2(data,base_shape_args,
                 layer_shape_args,
                 args_extra_shape)
  xlimits<-a[c(1,3)]
  ylimits<-a[c(2,4)]
  a<-cbind(xlimits,ylimits)
  a[1,2]<-a[2,2]
  range_meters<-max(geodist::geodist(a,measure = "geodesic"))
  step<-pretty(seq(0,range_meters,len=5))[2]/2
  format_distance(step)
}

bar_scale_coordinates <- function(n_bins = 4, bins_km = 100, pos_y = -27, pos_x = -44, bar_height = 10, crs, move_bin = 0,unit="km") {

  # Conversão de quilômetros para graus de longitude, ajustando pela latitude
  km_per_degree_lat <- 111  # Aproximadamente 111 km por grau de latitude
  km_per_degree_lon <- 111 * cos(pi * abs(pos_y) / 180)  # Ajustado pela latitude

  # Calcular quantos graus de longitude o bin vai mover
  degree_per_bin <- bins_km / km_per_degree_lon

  # Inicializar um data frame para armazenar as coordenadas dos retângulos
  coords <- data.frame(
    xmin = numeric(n_bins),
    xmax = numeric(n_bins),
    ymin = pos_y,
    ymax = pos_y
  )

  # Ajustar pos_x conforme move_bin
  pos_x <- pos_x - move_bin * degree_per_bin

  # Calcular as coordenadas para cada bin, movendo para a esquerda (oeste)
  for (i in 1:n_bins) {
    # O xmin será o ponto calculado agora, e o xmax será o ponto atual
    coords$xmax[i] <- pos_x
    coords$xmin[i] <- pos_x - degree_per_bin

    # A altura da barra de escala será constante em todas as iterações
    coords$ymax[i] <- coords$ymax[i] + bar_height  # Altura convertida para graus de latitude

    # Atualizar o ponto atual para o próximo bin (à esquerda, movendo oeste)
    pos_x <- coords$xmin[i]
  }

  # Definir as cores alternadas para a barra de escala
  coords$color <- c("black", "white")[rep(c(1, 2), len = nrow(coords))]

  # Manter as coordenadas ymin e ymax constantes para todos os bins
  coords$ymin <- coords$ymin[1]
  coords$ymax <- coords$ymax[1]

  # Criar as posições dos rótulos para a barra de escala
  posx_label_bar <- data.frame(
    x = sort(unique(unlist(coords[,1:2]))),
    y = coords$ymax[1],
    label = sort(c(0, cumsum(rep(bins_km, n_bins))))
  )

  posx_label_bar$label[nrow(posx_label_bar)] <- paste(posx_label_bar$label[nrow(posx_label_bar)], unit)

  # Atribuir os rótulos como atributo
  attr(coords, 'posx_label_bar') <- posx_label_bar

  return(coords)
}

add_bar_scale<-function(p,data,position="bottomright",
                        unit="km",
                        position_label='above',
                        n_bins=2,
                        bins_km=100,bar_height=10,
                        pad_x=0.01,
                        pad_y=0.01,
                        size_scalebar_text=3,
                        crs.info="+proj=longlat +datum=WGS84 +no_defs",...){
  attr(bins_km,"unit")<-unit
  bins_km<-format_distance_to(bins_km,"km")
  if(position=="none"){
    return(p)
  }

  base_shape<-attr(data,"base_shape")
  layer_shape<-attr(data,"layer_shape")

  if(all(!sapply(list(base_shape,layer_shape),is.null))){
    crs.info<-st_crs(suppressWarnings(st_union(base_shape,layer_shape)))
  } else{
    if(!is.null(base_shape)){
      crs.info<-st_crs(base_shape)
    }
    if(!is.null(layer_shape)){
      crs.info<-st_crs(layer_shape)
    }
  }

  pscales<-ggplot2:::layer_scales(p)
  pyrange<-pscales$y$range$range
  pxrange<-pscales$x$range$range




  {
    pos_y =   min(pyrange)
    pos_x = max(pxrange)


    bar_height=((max(pyrange)-min(pyrange))*bar_height)/10

    bar_scale<-bar_scale_coordinates(n_bins = n_bins, bins_km = bins_km, pos_y =   pos_y, pos_x =pos_x, bar_height = bar_height,crs=crs.info,move_bin=0,unit=unit)
    bar_lab<-attr(bar_scale,"posx_label_bar")
    bar_scale<-position_barscale(bar_scale,position,pxrange,pyrange,n_bins,pad_x,pad_y)
    newranges<-bar_scale
    bar_lab2<-add_label_bar(newranges,n_bins,bins_km,unit,position_label,padding_bar=2)


    p<-p+ggnewscale::new_scale_fill()
    nudge_y=0
    p<-p+
      geom_rect(data = newranges, mapping=aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=color),color="black",linewidth=0.3,show.legend=F)+geom_text(data=bar_lab2,aes(x,y,label=label),  nudge_y=nudge_y,size=size_scalebar_text)

    p<-p+scale_fill_manual(values=c("black","white"))+guides(fill="none")
    p
  }

}
add_label_bar<-function(coords,n_bins,bins_km,unit,position_label="above",padding_bar=0.1){
  pyrange<-range(c( coords$ymin , coords$ymax))
  pad_y<-(max(pyrange)-min(pyrange))*padding_bar

  pos_y<-switch(position_label,
                "below"={
                  pos_y =   mean(pyrange)
                  pos_y=pos_y-pad_y
                  pos_y

                },
                "above"={
                  pos_y = mean(pyrange)
                  pos_y=pos_y+pad_y
                  pos_y

                }
  )
  coords$ymin <- pos_y
  coords$ymax <- pos_y
  # Criar as posições dos rótulos para a barra de escala
  posx_label_bar <- data.frame(
    x = sort(unique(unlist(coords[,1:2]))),
    y = coords$ymax[1],
    label = sort(c(0, cumsum(rep(bins_km, n_bins))))
  )

  posx_label_bar$label[nrow(posx_label_bar)] <- paste(posx_label_bar$label[nrow(posx_label_bar)], unit)
  posx_label_bar
}


gg_add_titles<-function(p,main="",plot.title_size=12,title.face="plain",subtitle="",plot.subtitle_size=10,subtitle.face='plain',grid_sub_hadj=0,...){
  p+ggtitle(main, subtitle = subtitle) +
    theme(
      plot.title=element_text(size=plot.title_size,
                              hjust=grid_sub_hadj,
                              face=title.face),
      plot.subtitle=element_text(size=plot.subtitle_size,
                                 face=subtitle.face))
}





gg_plotList<-function(cur_data,
                      cur_choices_map,
                      cur_var_map,
                      filter,
                      filter_level,
                      saved_data,args,factor_filter,padding=0.1,main=colnames(data),round=round,raster=T,raster_resolution=0.036,down_grade_raster=F,down_grade=2,base_shape_args,
                      layer_shape_args,
                      args_extra_shape,interp_args,interp_gstat=NULL,interp=F,session=MockShinySession$new(),custom_breaks,circles=circles,pie=pie,breaks_on=T,titles,grid_sub_hadj,args_barscale,...) {


  data=get_data_map(name=cur_data,
                    attr=cur_choices_map,
                    var=cur_var_map,
                    filter="None",
                    filter_level=NULL,
                    saved_data=saved_data)

  args$breaks_on<-breaks_on
  j=1
  # args$data<-data
  vars<-colnames(data)
  args$data_o<-data


  data<-data[rownames(data),,drop=F]
  dd<-data[vars[j]]
  i=3
  rs<-split(dd,factor_filter)
  ps<-list()


  args$grid_sub_hadj=grid_sub_hadj
  withProgress(min=1,max=length(rs),message="Running...",session=session,{
    for(i in 1:length(rs)) {

      rsi<-rs[[i]]
      ddd<-data_migrate(data,rsi)
      args_temp<-args
      if(is.factor(ddd[,1])){
        args_temp$custom_breaks<-breaks<-levels(ddd[,1])
      } else{
        args_temp$custom_breaks<-breaks<-custom_breaks[[i]]
      }

      args_temp$data<-ddd
      #args_temp$main<-colnames(ddd)
      args_temp$main<-titles[[i]]
      incProgress(1,session=session)
      if(isTRUE(interp)|isTRUE(raster)){
        if(isTRUE(raster)) {
          rst<-raster_gg(data=ddd,
                         raster_resolution=raster_resolution,
                         down_grade_raster=down_grade_raster,
                         down_grade=down_grade,
                         base_shape_args=base_shape_args,
                         layer_shape_args=layer_shape_args,
                         args_extra_shape=args_extra_shape
          )

          rst<-migrate_rst(rst,data)
          args_temp$rst<-rst
          args_temp$args
          p<-do.call(gg_rst,args_temp)
        } else{
          rst<-interp_gg(data=ddd,interp_args,shape_args,interp_gstat,custom_breaks=breaks,base_shape_args=base_shape_args,
                         layer_shape_args=layer_shape_args,
                         args_extra_shape=args_extra_shape)
          rst<-migrate_rst(rst,data)
          args_temp$rst<-rst
          p<-do.call(gg_rst,args_temp)
        }

      }
      if(isTRUE(circles)|isTRUE(pie)){
        p<-do.call(gg_rst,args_temp)

      }

      args_temp$p<-p
      p<-do.call(gg_add_titles,args_temp)
      args_temp$p<-p
      p<-do.call(gg_style_axes,args_temp)
      args_barscale$p<-p
      args_barscale$data<-args_temp$data
      p<-do.call(add_bar_scale,args_barscale)
      args_temp$p<-p
      p<-do.call(gg_add_north,args_temp)

      ps[[i]]<-p
    }

  })




  ps
}

gg_add_north<-function(p,n_location,n_which_north,n_width,n_height,n_pad_x,n_pad_y,n_cex.text,...){
  if(n_location=="none"){
    return(p)
  }
  p<-p+ annotation_north_arrow(location = n_location,
                               which_north = n_which_north,
                               width = unit(n_width, "pt"),
                               height  = unit(n_height, "pt"),
                               pad_x = unit(n_pad_x, "in"),
                               pad_y = unit(n_pad_y, "in"),
                               style = north_arrow_fancy_orienteering(text_size =n_cex.text))
  p
}


variogram_parameters<-function(df,model="Gau",var=1, log=F, g=NULL){

  log_params=NULL
  if(isTRUE(log)){
    log_var<-vegan::decostand(df[,var],"log")
    log_params<-attr(log_var,"parameters")
    df[,var]<-log_var
  }
  coords<-attr(df,"coords")[rownames(df),]
  req(coords)
  req(!anyNA(coords))

  colnames(coords)<-c("x","y")
  z<-df[var]
  if(!is.null(g)){
    if(length(g$data)>0){
      newnames<-make.unique(c(names(g$data),var))
      var<-newnames[length(newnames)]
      colnames(z)<-var
    }
  }
  dd<-cbind(coords,z)
  req(nrow(na.omit(dd))>0)
  formula<-as.formula(paste(var,"~ 1"))
  dsp<-to_spatial(dd)



  if(nrow(sp::zerodist(dsp))>0){
    dsp <- dsp[-sp::zerodist(dsp)[,1],]
  }
  TheVariogram=NULL

  auto_log<-capture_log1(automap::autofitVariogram)(formula, dsp,model=model)
  auto_z1<-auto_log[[1]]
  vgm_z1<-auto_z1[[1]]
  model_z1<-auto_z1[[2]]
  result<-list(variogram=vgm_z1,auto_z1=auto_z1, formula=formula,dsp=dsp, id=var, log=log,log_params=log_params)
  attr(result,"logs")<-auto_log[[2]]
  result


  return(result)
}
plot_variogram <- function(v, m, sized=F, show_vgm_lines=T) {
  preds = gstat::variogramLine(m, maxdist = max(v$dist))
  g<-ggplot()
  if(isTRUE(sized)){
    g<-g+geom_point(data = v, aes(x = dist, y = gamma, size=np))
  } else{
    g<-g+geom_point(data = v, aes(x = dist, y = gamma))
  }

  g<-g+geom_line(data = preds, aes(x = dist, y = gamma))+
    xlab("Distance")+ ylab("Semivariance")



  g
}
gg_rst<-function(rst=NULL,data=NULL,limits=NULL,legend.text_size=13,layer_shape=NULL,extra_shape=NULL,breaks=T,nbreaks=5,pal="turbo",key.height=NULL,newcolhabs=list(turbo=viridis::turbo),add_extra_shape=F,add_base_shape=T,add_layer_shape=F,show_labels=F,labels=attr(rst,"factors")[1],cex.fac=13,col.fac="red",show_coords=F,col.coords="black",cex.coords=13,custom_breaks=NULL,layer_col="gray",lighten=0.4,layer_shape_border="gray",width_hint=0.15,cex_scabar=0.7,fillOpacity=1,reverse_palette=F,scale_radius=T,min_radius=1,max_radius=5,addCircles=T,addMinicharts=F,buffer_zize=1,
                 fun="sum",factor_chart=1,args_extra_shape=NULL,data_depth=NULL,data_o=NULL,base_shape_args,layer_shape_args,factor=F,light=0,legend.position="none",show_scale_bar=F,breaks_on=T,leg_title=NULL,bg='white',...) {

  if(!isTRUE(factor)){
    custom_breaks00<-as.numeric(custom_breaks)
  } else{
    custom_breaks00<-custom_breaks

  }

  {

    crs.info="+proj=longlat +datum=WGS84 +no_defs"
    req(!is.null(rst)|!is.null(data))

    if(class(rst)[1]=="RasterLayer"){
      crs.info<-raster::crs(rst)
      rasterpoints <-  raster::rasterToPoints(rst) |> data.frame()

      if(isTRUE(factor)){
        # labels=rst@data@attributes[[1]][,2][ rst@data@attributes[[1]][,2]%in%unique(rasterpoints[,3])]
        labels<-rst@data@attributes[[1]][,2]


        fac0<-factor(rasterpoints[,3],levels=1:length(rst@data@attributes[[1]][,2]),labels=rst@data@attributes[[1]][,2])

        rasterpoints[,3]<-factor(fac0,levels= attr(rst,"data_levels"),labels=attr(rst,"data_levels"))

        # rasterpoints[,3]<-factor(rasterpoints[,3], labels=rst@data@attributes[[1]][,2])
        colnames(rasterpoints)[3]<-"z"
      }



      coords<-raster::coordinates(rst) |> data.frame()
    } else{
      coords<-attr(data,"coords")
      rasterpoints<-data.frame(cbind(coords,z=data[,1]))
      rst<-data
    }

    validate(need(nrow(rasterpoints)>0,"Error"))


    if(!is.factor(rasterpoints$z)){
      custom_breaks0<-as.numeric(custom_breaks)
      colnames(rasterpoints)<-c("x","y","z")
      custom_breaks<-as.numeric(custom_breaks)

    }



    base_shape<-attr(rst,'base_shape')
    layer_shape<-attr(rst,'layer_shape')
    if(!exists('limits')){
      limits<-NULL
    }

    if(is.null(limits)){
      limits<-get_limits2(
        rst,base_shape_args,
        layer_shape_args,
        args_extra_shape
      )
      limits<-list(x_min=limits$xmin,x_max=limits$xmax, y_min=limits$ymin,y_max=limits$ymax)
    }




    if(!exists('breaks')){
      breaks<-NULL
    }



    if(!is.factor(rasterpoints$z)){
      #breaks=    breaks_interval(z=rasterpoints$z,nbreaks)
      breaks= as.numeric(custom_breaks)
    } else{ breaks=    levels(rasterpoints$z)}



    xlim<-unlist(limits[1:2])
    ylim<-unlist(limits[3:4])



    base_shape0<-st_as_sf(coords,coords = colnames( coords),crs=crs.info)


    p<-ggplot()

    if(class(rst)[1]!="RasterLayer"){
      p<-ggplot(data=base_shape0)
    }





    p<-add_gg_shape(p,base_shape_args,base_shape,xlim,ylim,crs.info)



  }

  if(class(rst)=="RasterLayer"){
    breaks=NULL

    if(isFALSE(factor)){
      breaks<-as.numeric(custom_breaks)
      custom_breaks0=as.numeric(custom_breaks0)
    } else{
      custom_breaks0=custom_breaks
    }

    #rst@data@values<-scales::rescale(rst@data@values,range(custom_breaks0))
    breaks<-custom_breaks0
    p<-rst_tile(p,rasterpoints,rst,newcolhabs,pal,fillOpacity,reverse_palette,name,breaks,factor, data_o=data_o,light, custom_breaks=custom_breaks0,show_legend=T,limits=NULL,breaks_on=breaks_on,leg_title=leg_title)

  } else{


    if(isTRUE(addCircles)){

      p<-gg_circles(p,rasterpoints,rst,newcolhabs,pal,fillOpacity,reverse_palette,name,custom_breaks=sort(custom_breaks00),min_radius,max_radius, scale_radius, light,show_legend=T,leg_title=leg_title)}


    if(isTRUE(addMinicharts)){
      p<-gg_pie(p,rst,factor_chart,buffer_zize,fun,min_radius,max_radius,newcolhabs,pal,reverse_palette, fillOpacity,light,leg_title=leg_title)

    }
  }
  names( p$layers)[length( p$layers)]<-paste0('points')



  p0<-p


  p<-p+

    theme(panel.grid.major = element_blank(),
          panel.background = element_rect(fill = bg),
          panel.border = element_rect(fill=NA,
                                      color="black",
                                      linewidth=0.5,
                                      linetype="solid"),

          #legend.text=element_text(size=legend.text_size),
          #legend.title=element_text(size=legend.text_size)

    )
  if(!is.null(key.height)){
    p<-p+theme(legend.key.size=unit(key.height, 'pt'))
  }

  extra_shape<-attr(rst,'extra_shape')

  if(!is.null(args_extra_shape)){
    p<-gg_add_extra_shape(p,rst,args_extra_shape)
  }

  if(show_coords!="None"){
    coords_data<-attr(rst,"coords")
    colnames(coords_data)<-c("x","y")
    p<-p+geom_point( data=coords_data, aes(x=x, y=y),  size=cex.coords, pch=as.numeric(show_coords), colour=col.coords)}
  p<-add_gg_shape(p,layer_shape_args,layer_shape,xlim,ylim,crs.info)

  #  point_layer<-p$layers[ grep("points",  names(p$layers))]
  #  old_layer<-p$layers[-grep("points",  names(p$layers))]
  #  new_p <- append(old_layer, point_layer, after=0)
  # p$layers<-new_p


  if(!is.null(data_depth)) {
    point_layer<-p$layers[ grep("points",  names(p$layers))]
    old_layer<-p$layers[-grep("points",  names(p$layers))]
    new_p <- append(old_layer, point_layer, after=data_depth-1)
    p$layers<-new_p

  }
  if(isTRUE(show_labels)){
    labels=attr(data,"factors")[labels]
    coords_labels<-attr(data,"coords")
    labs<-labels[rownames(coords_labels),]
    df_labels<-cbind(coords_labels,labs)
    colnames(df_labels)<-c("x","y","label")
    p<-p+  geom_text( data=df_labels, aes(x=x, y=y, label=label),size=cex.fac,colour=col.fac)
  }



  if(legend.position=="none"){
    p<-p+guides(fill="none",color="none",size="none")
  } else{
    p<-p+
      theme(legend.position=legend.position)
  }




  p<-p+    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE)
  return(p)

}



gg_bwaxes0<-function(xb,yb,yh){
  {axis1<-data.frame(
    xmin=xb[1:(length(xb)-1)],
    xmax=xb[2:length(xb)],
    ymin=min(yb)-yh,
    ymax=min(yb),
    color=rep(c("black","white"),len=length(xb)-1),
    axis="axis1"
  )
  axis2<-data.frame(
    xmin=max(xb),
    xmax=max(xb)+yh,
    ymin=yb[1:(length(yb)-1)],
    ymax=yb[2:length(yb)],
    color=rep(c("black","white"),len=length(yb)-1),
    axis="axis2"
  )
  axis3<-data.frame(
    xmin=xb[1:(length(xb)-1)],
    xmax=xb[2:length(xb)],
    ymin=max(yb),
    ymax=max(yb)+yh,
    color=rep(c("black","white"),len=length(xb)-1),
    axis="axis3"

  )

  axis4<-data.frame(
    xmin=min(xb)-yh,
    xmax=min(xb),
    ymin=yb[1:(length(yb)-1)],
    ymax=yb[2:length(yb)],
    color=rep(c("black","white"),len=length(yb)-1),
    axis="axis4"
  )
  }

  axis1$xmin[1]<-min(axis4$xmin)
  axis1$xmax[length(axis1$xmax)]<-max(axis2$xmax)
  axis3$xmin[1]<-min(axis4$xmin)
  axis3$xmax[length(axis1$xmax)]<-max(axis2$xmax)
  df<-data.frame(rbind(axis1,axis2,axis3,axis4))
  df
}
any_shape<-function(data){
  res<-c(
    length(attr(data,"base_shape"))>0,
    length(attr(data,"layer_shape"))>0,
    length(attr(data,"extra_shape"))>0
  )
  any(res)
}
adaptative_rounding<-function(vetor){
  # Calcula as diferenças absolutas entre os números do vetor
  diferencas <- diff(sort(vetor))

  # Encontra a menor diferença
  min_diferenca <- min(diferencas)

  # Calcula o número de casas decimais necessário para preservar essa diferença
  num_casas <- ceiling(-log10(min_diferenca))

  # Arredonda o vetor com o número de casas decimais calculado
  vetor_arredondado <- round(vetor, num_casas)
  vetor_arredondado=format(vetor_arredondado, nsmall = num_casas)
  # Exibe o resultado
  vetor_arredondado
}
gg_bwaxes<-function(axis_width,x.n.breaks,y.n.breaks,data,base_shape_args,p,layer_shape_args,args_extra_shape){
  lims<-get_limits2(data,base_shape_args,layer_shape_args,args_extra_shape)
  axis_width=axis_width/10
  #x.n.breaks=x.n.breaks-5
  #y.n.breaks=y.n.breaks-5
  scales<-ggplot2::layer_scales(p)
  pyrange<-do.call(c,c(lims[c("ymin","ymax")]))
  pxrange<-do.call(c,c(lims[c("xmin","xmax")]))
  yh<-(max(pyrange)-min(pyrange))*axis_width
  x.nb=x.n.breaks
  y.nb=y.n.breaks
  if(!x.nb%%2==0){
    x.nb<-x.nb+1
  }
  if(!y.nb%%2==0){
    y.nb<-y.nb+1
  }


  xb<-seq(pxrange['xmin'],pxrange['xmax'],len=x.nb)
  yb<-seq(pyrange['ymin'],pyrange['ymax'],len=y.nb)

  #xb<-pretty(attr(data,"coords")[,1],x.nb)
  #yb<-pretty(attr(data,"coords")[,],y.nb)

  # xb<-scales::breaks_pretty(x.nb)(seq(lims$xmin,lims$xmax,len=100))
  # yb<-scales::breaks_pretty(y.nb)(seq(lims$ymin,lims$ymax,len=100))
  #xb<-round(xb,max(sapply(pretty(xb),decimal_places)))
  #yb<-round(yb,max(sapply(pretty(yb),decimal_places)))
  #xb<-c(min(xb),xb,max(xb))
  #yb<-c(min(yb),yb,max(yb))
  print(any_shape(data))
  if(!any_shape(data)){
    #  xb<-c(min(xb),xb,max(xb)+(yh*4))
    # yb<-c(min(yb),yb,max(yb)+(yh*4))
  }
  adf<-gg_bwaxes0(xb,yb,yh)
  adf
}
gg_style_axes<-function(p,axis_style="default",xlab='Longitude',ylab='Latitude',axis.text_size=13,axis.title_size=13,x.n.breaks=5,y.n.breaks=5,axis_width=0.1,show_guides=F,guides_color="gray",guides_linewidth=0.15,guides_linetype= "dashed",data=NULL,base_shape_args=NULL,layer_shape_args=NULL,args_extra_shape=NULL,...){

  p<-p+xlab(xlab)+ylab(ylab)+theme(
    axis.title=element_text(size=axis.title_size),
    axis.text=element_text(size=axis.text_size),
  )

  if(axis_style=="default"){
    return(p)
  }
  adf<-gg_bwaxes(axis_width,x.n.breaks, y.n.breaks, data,base_shape_args,p,layer_shape_args,args_extra_shape)
  labelx<-subset(adf,axis=='axis1')$xmin
  labelx<-c(labelx[-c(1)])
  labely<-subset(adf,axis=='axis4')$ymin
  labely<-labely[-c(1)]
  labelsx<-adaptative_rounding(labelx)
  # labelsx<-format(labelsx, nsmall = max(sapply(labelsx,decimalplaces)))
  labelsy<-adaptative_rounding(labely)
  # labelsy<-format(labelsy, nsmall = max(sapply(labely,decimalplaces)))

  xmin=min(adf$xmin)
  xmax=max(adf$xmin)
  ymin=min(adf$ymin)
  ymax=max(adf$ymax)
  xxyy<-data.frame(x=c(xmin,xmin,xmax,xmax),
                   y=c(ymin,ymax,ymax,ymin))

  p<-p+coord_sf(
    xlim=range(c(adf$xmin,adf$xmax)),
    ylim=range(c(adf$ymin,adf$ymax)),
    #label_axes=list(bottom=labelx),
    expand=F
  )

  p<-p+
    geom_rect(data=adf,aes(ymin=ymin,ymax=ymax,xmin=xmin,xmax=xmax),color="black",linewidth=0.3,fill=adf$color)


  p<-p+
    scale_x_continuous(breaks =labelx,
                       labels=as.character(labelsx) )+
    scale_y_continuous(breaks =labely,labels=labelsy )+
    guides(y= guide_axis(angle=90,cap="upper",minor.ticks=F))+
    theme(
      axis.ticks=element_blank()
    )
  if(isTRUE(show_guides)){
    p<-p+geom_hline(yintercept=labely,color =guides_color, linetype =guides_linetype, linewidth = guides_linewidth)+
      geom_vline(xintercept =labelx,color =guides_color, linetype = guides_linetype, linewidth = guides_linewidth)
  }
  p

}
# Function to restore all inputs
restoreInputs<-function(session, input_ids, state) {
  print("RESTORING")

  withProgress(message="Loading...",min=1,max=length(input_ids),{
    for (id in input_ids) {
      incProgress(1, #detail = paste("Updating", id)
                  )
     # runjs(paste0("Shiny.setInputValue('",id,"', '",state[[id]],"');"))
      if (!is.null(state[[id]])) {
        # Verifique se o input já existe no session$input
        if (!is.null(session$input[[id]])) {
          # Atualizar para inputs de texto
          if (is.character(session$input[[id]])) {
            #updateRadioButtons(session, id, selected = state[[id]])

            updateTextInput(session, id, value = state[[id]])
            updateRadioGroupButtons(session, id, selected = state[[id]])
            # Atualizar para inputs numéricos
          } else if (is.numeric(session$input[[id]])) {
            updateNumericInput(session, id, value = state[[id]])
          }


          if (is.logical(session$input[[id]])) {
            updateCheckboxInput(session, id, value = state[[id]])
          }


        }
      }

    }
  })

}
restoreInputs2<-function(session, id, state) {
  updated=F
  if (!is.null(state)) {
    if (!is.null(session$input[[id]])) {
      if (is.character(session$input[[id]])) {
        updateTextInput(session, id, value = state)
        updateRadioGroupButtons(session,id,selected = state)

        updated=T
      } else if (is.numeric(session$input[[id]])) {
        updateNumericInput(session, id, value = state)
        updated=T
      }
      if (is.logical(session$input[[id]])) {
        updateCheckboxInput(session, id, value = state)
        updated=T
      }
    }
  }
  updated
}




