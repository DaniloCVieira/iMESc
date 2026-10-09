


is_installed <- function(package) {
  # Check if the package is installed
  installed_packages <- rownames(installed.packages())
  return(package %in% installed_packages)
}
install_packages <- function(package) {
  install.packages(package, dependencies = TRUE)
}

formals_from_sring<-function(string){
  fun_name <- strsplit(string, "::")[[1]][2]
  package_name <- strsplit(string, "::")[[1]][1]
  function_name2<-paste0(fun_name,".default")

  fun <- try(get(function_name2, envir = asNamespace(package_name)),silent =T)
  if(inherits(fun,"try-error")){
    fun<-get(fun_name, envir = asNamespace(package_name))
  }
  formals(fun)
}
run_gridcaret<-function(x,y, model, len=5){
  fun<-model_grid[[model]]
  args<- formals(fun)
  args$len<-len
  args$x<-x
  args$y<-y
  args$y<-args$y[,1]
  args<-as.list(args)
  res<-data.frame(do.call(fun,args))
  classes<-sapply(res,class)
  int<-classes%in%'integer'
  classes[int]<-"numeric"
  return(list(
    classes=classes,
    param=res
  ))
}
strong_forest<-function(text){
  strong(text,style="color: darkgreen")
}
emforest<-function(text){
  em(text,style="color: darkgreen")
}
box_caret<-function(id,content=NULL,inline=T, class="train_box",click=T,title=NULL,button_title=NULL,tip=NULL,auto_overflow=F, color=NULL,show_tittle=T,hide_content=F,button_title2=NULL,ab=NULL){
  ns<-NS(id)

  id0<-strsplit(id,"-")[[1]]
  id0<-id0[length(id0)]
  if(is.null(color)){
    color=getcolorbox(id0)
  }
  background=paste0("background: ",color,";")
  # border=paste0("box-shadow: 0 0px 2px ","#303030ff","; ")
  color=paste0("color: ",color,";")
  style<-paste0(background)

  if(is.null(title))
    title<-getboxtitle(id0)
  if(isTRUE(inline)){
    class=paste("inline_pickers",class)
  }
  if(is.null(tip)){
    tip<-getboxhelp(id0,ns,click)
  }
  style_over='padding-top: 5px;'
  if(auto_overflow){
    style_over='padding-top: 5px; overflow:auto;'
  }
  if(isTRUE(hide_content)){
    hide_content<-"+"
  } else{
    hide_content="-"
  }
  div_title<-NULL
  if(is.null(ab)){
    ab<-actionButton(ns("show_hide"),hide_content,style=style)
  }
  if(isTRUE(show_tittle)){
    div_title<-div(
      style="display: flex",class="box_title",
      div(
        ab,
        title,tip),
      div(
        class="btn-title2",
        button_title2
      )

    )
  } else{
    class='train_box ptop0'
  }

  div(class=class,
      #style=border,
      div_title,div(button_title,style="position: absolute; top: -1px;right: 0px; padding: 3px"),
      div(id=ns('content'),style=style_over,
          content
      )
  )
}

box_caret_server<-function(id, hide_content=F){
  moduleServer(id,function(input,output,session){
    val_box<-reactiveVal("-")
    if(isTRUE(hide_content)){
      val_box<-reactiveVal("+")
    }
    observeEvent(input$show_hide,ignoreInit = T,{
      shinyjs::toggle("content")
      if(val_box()=="-"){
        val_box("+")
      } else{
        val_box("-")

      }
    })

    observeEvent(val_box(),ignoreInit = T,{
      updateActionLink(session,'show_hide',val_box())
    })

    once_hide<-reactiveVal(F)
    observe({
      req(isFALSE(once_hide()))
      req(length(input$show_hide)>0)
      if(isTRUE(hide_content))
      shinyjs::hide("content")
      once_hide(TRUE)
    })

  })
}
getboxhelp<-function(id=1,ns,click=T){
  content_id<-paste0('box',id,'_help_content')
  if(exists(content_id)){
    content<-get(content_id)
    get_drop_help(ns,content, id,click)
  } else{
    NULL
  }
}
getcolorbox<-function(id=1,ns){

  if(any(grepl(id,c(as.character(0:6))))){
    switch(id,
           "1"="#748cccff",
           "2"="#74c7ccff",
           "3"="#c3cc74ff",
           "4"="#e6af83ff",
           "5"="#7bcc7dff",
           "0"="#374061ff",
           "6"="#e68f83ff")} else{
             '#7bcc7dff'
           }
}
getboxtitle<-function(id=""){
  res<-switch(id,
              "1"="Tunning",
              "2"="Custom-grid parameters",
              "3"='Model Parameters',
              "4"= "Resampling parameters",
              "5"="Summary",
              "6"="Current tunning"  )
  span(res,style="padding-left: 5px;padding-right:5px;font-weight: bold")
}
get_drop_help<-function(ns,content, id,click=T){
  link=paste0("box_caret",id)
  color0<-getcolorbox(id,ns)
  st<-paste(paste0('.tip_box_caret',id), '.tippy-tooltip.material-theme {background:', colorspace::lighten(color0,0.95),";box-shadow: 0 0px 2px",color0,";}")
  if(!isTRUE(click)){
    return(tipright(content))
  }
  div(tags$style(
    HTML(st)  ),style="display: inline-block",
    class=paste0("tip_panel tip_",link),
    shinyWidgets::dropMenu(placement="right",
                           theme ="material",
                           actionLink(ns(paste0(link,"_help")),icon("fas fa-question-circle")),
                           div(
                             tiphelp_icon(ns(paste0(link,"_help")),"Click for details", "right"),
                             content
                           )
    )
  )
}







#' @export

gg_pairplot<-function(x,y,cols,xlab="",ylab="",title="",include.y=F,cor.size=1,varnames.size=1,points.size=1,axis.text.size=1,axis.title.size=1,plot.title.size=1,plot.subtitle.size=1,legend.text.size=1,legend.title.size=1,alpha.curve=.5, alpha.points=1, method = "pearson",round=3,title_corr=paste(method,"corr"),subtitle=paste("*** p< 0.001,","** p< 0.05 and","* p< 0.1"),switch = NULL,upper="cor", pch=16){
isfac<-is.factor(y[,1])
  if(!is.null(y)){
    df=data.frame(x,y=y[,1])
    li<-split(x,y[,1])
    row1<-F
    if(any(sapply(li,nrow)==1)){
      row1=T
    }
  } else{
    df<-x
    row1<-F
  }



  col.curve<-adjustcolor(cols,alpha.curve)
  col.points<-adjustcolor(cols,alpha.points)

  cor.size=cor.size*3
  points.size=points.size*3
  axis.text.size=axis.text.size*10
  axis.title.size=axis.title.size*3
  plot.title.size=plot.title.size*10
  plot.subtitle.size=plot.subtitle.size*10
  legend.text.size=legend.text.size*10
  legend.title.size=legend.title.size*10
  varnames.size=varnames.size*10

  if(isTRUE(include.y)){
    columns=1:ncol(df)
    columnLabels=colnames(cbind(x,y))
  } else{
    columns=1:(ncol(df)-1)
    columnLabels=colnames(x)
  }
  if(is.null(y)){
    columns=1:ncol(x)
  }
  scalecolor<-if(isfac){
    scale_colour_manual(values = col.points)
  } else{
    scale_color_gradientn(colours =  col.points)
  }
  scalediag<-if(isfac){
    scale_fill_manual(values = col.curve)
  } else{
    NULL
  }
  scaleupper<-if(isfac){
    scale_colour_manual(values = cols)
  } else{
    NULL
  }
  aes_color<-if(isfac){
    aes(colour = if(!is.null(y)){y} else{NULL})
  } else{
    NULL
  }


  if(upper=="cor"){
    fun_upper<-list(continuous = function(data, mapping, ...) {
      if(isTRUE(row1)){
        mapping$colour<-NULL
      }
      if(is.null(y)){
        mapping$colour<-NULL
      }

      GGally::ggally_cor(data = df,
                         mapping = mapping,
                         size=cor.size,

                         method=method,
                         digits = round,
                         title = title_corr)      + scaleupper

      })

  } else{
    fun_upper<-"blank"
  }


  p<-GGally::ggpairs(df,
             columns =columns ,
             switch = switch,

             columnLabels=columnLabels,
             aes_color,
             upper =fun_upper,
             lower = list(continuous = function(data, mapping, ...) {
               if(is.null(y)){
                 mapping$colour<-NULL
               }
               GGally::ggally_points(data = df, mapping = mapping, size=points.size, shape=16)+ scalecolor
               }),
             diag = list(continuous = function(data, mapping, ...) {
               if(is.null(y)){
                 mapping$colour<-NULL
               }
               GGally::ggally_densityDiag(data = df, mapping = mapping) + scalediag
               }))

  p<-p+labs(title = title,
            tag = subtitle)
  p<-p+xlab(xlab)+ylab(ylab)

  p<- p+theme(
    plot.tag.position = "bottom",
    panel.grid.major = element_blank(),
    panel.background=element_rect(fill=NA, color="white"),
    panel.border = element_rect(fill=NA,color="black", linewidth=0.5, linetype="solid"),
    axis.line=element_line(),
    axis.text=element_text(size=axis.text.size),
    axis.title=element_text(size=axis.title.size),
    plot.title=element_text(size=plot.title.size),
    plot.tag=element_text(size=plot.subtitle.size,face="italic"),
    plot.subtitle=element_text(size=plot.subtitle.size,face="italic"),
    legend.text=element_text(size=legend.text.size),
    legend.title=element_text(size=legend.title.size),
    strip.text.x = element_text(size = varnames.size),
    strip.text.y = element_text(size = varnames.size)
  )



  if(fun_upper!="blank"){
    if(isTRUE(row1)){
      attr(p,"row1")<-"Warning: Due to insufficient data points in certain groups (fewer than two), the correlation per group could not be calculated. Only the overall correlation will be displayed"
    }

  }

  if(isTRUE(include.y)){
    for(i in 1:ncol(df)){
      p[i,ncol(df)] <- p[i,ncol(df)] +
        scale_fill_manual(values=col.points)

    }
    for(i in 1:ncol(df)){
      p[ncol(df),i] <- p[ncol(df),i] +
        scale_fill_manual(values=col.points)

    }
  }


  return(p)

}



#dnn plot
circle_from_center <- function(center = c(0,0), radius = 1, resolution = 100) {
  angles <- seq(0, 2 * pi, length.out = resolution)
  circle_coords <- sapply(angles, function(angle) {
    c(center[1] + radius * cos(angle), center[2] + radius * sin(angle))
  })
  return(t(circle_coords))
}
#modelo<-readRDS("dnn.rds")$finalModel
get_neulist<-function(modelo){
  wt<-modelo$W

  size<-modelo$size
  names(size)<-paste0("layer",1:length(size))
  names(wt)<-paste0("layer",1:length(wt))
  i=1
  neulist<-lapply(seq_along(size),function(i){
    name<-names(size)[i]
    weight<-as.vector(wt[[name]])
    if(is.null(weight)){
      weight<-NA
    }
    res<-data.frame(Layer=name,neuron=paste0("Neu",seq_len(size[name])),weight,y=seq_len(size[name]),x=i)


    res
  })
  mean_neu<-max(sapply(neulist,function(x){
    mean(x$y)
  }))
  neulist2<-lapply(neulist,function(x){
    x$y<-rescale_to_mean(x$y,mean_neu)
    x
  })
  neulist2
}
rescale_to_mean <- function(vec, mean) {
  # Calcula a média atual do vetor
  current_mean <- mean(vec)

  # Calcula o fator de escala necessário para atingir a média desejada
  scale_factor <- mean / current_mean

  # Multiplica cada valor do vetor pelo fator de escala
  scaled_vec <- vec * scale_factor

  return(scaled_vec)
}
plot_ggnet<-function(modelo,
                     neuron_fill="Grey",
                     neuron_border="Grey",
                     weight_palette,
                     neu_radius=0.05,
                     xlim=c(-.5,1.5),
                     text_size=1,
                     linewidth=1,
                     size.layers.text=1){
  neu<-get_neulist(modelo)
  dft<-do.call(rbind,neu)
  dft$x<-scales::rescale(dft$x,c(0,1))
  dft$y<-scales::rescale(dft$y,c(0,1))
  cutted<-  cut(dft$weight,5)
  #dft$weight<-cutted
  if(missing(weight_palette)){
    weight_palette=viridis::turbo
  }
  dft$color<-  weight_palette(nlevels(cutted))[cutted]
  neu<-split(dft,dft$Layer)
  data<-lapply(seq_along(neu),function(i){
    n0<-neu[[i]]
    data.frame(n0[!duplicated(n0$neuron),])
  })
  df<-do.call(rbind,data)

  i=1
  lis<-split(df,df$Layer)
  p<-ggplot(df)


  pol<- data.frame(do.call(rbind,lapply(1:nrow(df),function(i) {
    data.frame(circle_from_center(c(df$x[i],df$y[i]),neu_radius),group=i)
  })))
  colnames(pol)[1:2]<-c('x','y')

  p<-ggplot(pol)
  i=1
  for(i in ((1:(length(lis)-1)))){
    print(i)
    x0=data[[i]]$x+(neu_radius)
    y0=data[[i]]$y
    x1=data[[i+1]]$x-(neu_radius)
    y1=data[[i+1]]$y
    xx<-expand.grid(x0=x0,x1=x1)
    xx$weights<-data[[i]]$weight

    yy<-expand.grid(y0=y0,y1=y1)

    p<-p+geom_segment(aes(x=x0,y=y0,xend=x1,yend=y1,color=weights),linewidth=linewidth,
                      data=cbind(xx,yy)

    )

  }

  p
  dfinput<- data.frame(x=data[[1]]$x-0.15,
                       y=data[[1]]$y,
                       xend=data[[1]]$x-(neu_radius),
                       yend=data[[1]]$y)
  p<-p+geom_segment(aes(x=x,y=y,xend=xend,yend=yend),data=dfinput,arrow=arrow(length = unit(0.1,"inches")))+
    geom_text(data=dfinput,aes(x,y),
              label=colnames(data.frame(modelo$W[[1]])),
              hjust=1,size=text_size) + xlim(xlim)
  n<-length(data)
  dfoutput<- data.frame(x=data[[n]]$x+((neu_radius)*1.2),
                        y=data[[n]]$y,
                        xend=data[[n]]$x+0.1,
                        yend=data[[n]]$y)
  p
  p<-p+ geom_text(data=dfoutput,aes(x,y),
                  label=rownames(data.frame(modelo$W[[length(modelo$W)]])),
                  hjust=0,size=text_size)
  p<-p+geom_polygon(aes(x,y,group=group),data=pol,colour=neuron_border,fill=neuron_fill)+scale_color_gradientn(colors=weight_palette(100))
  size<-modelo$size
  for(i in seq_along(size)){
    if(i==1){
      names(size)[i]<-"Input"
    }else if(i==length(size)){
      names(size)[i]<-"Output"
    } else{
      names(size)[i]<-"Hidden"
    }
  }

  names(size)[names(size)=="Hidden"]<-paste0(names(size)[names(size)=="Hidden"],1:length(names(size)[names(size)=="Hidden"]))
  y<-max(df$y)+
    1/max(size)
  x<-unique(df$x)

  p<-p+geom_text(data= data.frame(x,y),aes(x,y),label=names(size),size=size.layers.text)
  names(modelo$size)
  p+theme_void()


}


neurons_avNNet<-function(mod1){
  size<-mod1$n
  mean_neu<-max(sapply(size,function(x) mean(seq_len(x))))
  dfneu<-do.call(rbind,lapply(seq_along(size),function(i){
    data.frame(neu=seq_len(size[[i]]),x=i,y=rescale_to_mean(seq_len(size[[i]]),mean_neu))
  }))
  dfneu$neuron<-1:nrow(dfneu)

  dfb<- data.frame(neu=seq_along(size[-1]),
                   y=max(dfneu$y)+1,
                   x=seq_along(size[-1])+0.5,
                   neuron=seq_along(size[-1])+max(dfneu$neuron))

  dfneu<-rbind(dfneu,dfb)

  dfneu$x<-scales::rescale(dfneu$x,c(0,1))
  dfneu$y<-scales::rescale(dfneu$y,c(0,1))
  dfneu
}
pol_avNNet<-function(dfneu,neu_radius){
  pol<- data.frame(do.call(rbind,lapply(1:nrow(dfneu),function(i) {
    data.frame(circle_from_center(c(dfneu$x[i],dfneu$y[i]),neu_radius),group=i)
  })))
  colnames(pol)[1:2]<-c('x','y')
  pol
}
dfinput_avNNet<-function(dfneu,neu_radius){
  dfinput<-subset(dfneu,x==0)
  dfinput$xend<-dfinput$x-neu_radius
  dfinput$x<-dfinput$x-0.15
  dfinput
}
dfoutput_avNNet<-function(dfneu,neu_radius){
  dfoutput<-subset(dfneu,x==max(dfneu$x))
  dfoutput<- data.frame(x=dfoutput$x+((neu_radius)*1.2),
                        y=dfoutput$y,
                        xend=dfoutput$x+0.1,
                        yend=dfoutput$y)
  dfoutput
}
weights_B_avNNet<-function(mod1){
  size<-mod1$n
  w<-mod1$wts
  xlayer<-unlist(sapply(2:(length(size)),function(i){
    rep(i,  size[i])
  }))
  df1<-data.frame(weights=mod1$wts[which(mod1$conn==0)],conn=mod1$conn[which(mod1$conn==0)],x=  xlayer-0.5)
  start=0
  res=c()
  for(i in seq_along(mod1$conn)){
    conn<-mod1$conn[[i]]
    if(conn==0){
      start<-start+1
    }
    res[i]<-start
  }
  df1$conn<-unlist(lapply(split(res,res),function(x) x[1]))
  df1$conn<-df1$conn+size[1]
  df1<-rbind(df1)

  bconlis<-split(df1,df1$x)
  bids<-seq_along(size[-1])+max(sum(size))
  do.call(rbind,lapply(seq_along(bconlis),function(i){
    bconlis[[i]]$neuron<-bids[[i]]
    bconlis[[i]]
  }))

}
weights_avNNet<-function(mod1){
  size<-mod1$n
  w<-mod1$wts

  xlayer<-unlist(sapply(1:(length(size)-1),function(i){
    rep(i,  size[i]*size[i+1])
  }))
  xlayer<-xlayer+1

  df1<-data.frame(weights=mod1$wts[-which(mod1$conn==0)],conn=mod1$conn[-which(mod1$conn==0)],x=xlayer)
  start=0
  res=c()
  for(i in seq_along(mod1$conn)){
    conn<-mod1$conn[[i]]
    if(conn==0){
      start<-start+1
    }
    res[i]<-start
  }
  df1$neuron<-unlist(lapply(split(res,res),function(x) x[-1]))
  df1$neuron<-df1$neuron+size[1]
  df1<-rbind(df1)
  df1
}

segments_avNNet<-function(df1,dfneu){
  do.call(rbind,lapply(1:nrow(df1),function(i){
    result<-df1[i,]
    res<-dfneu[dfneu$neuron==df1$conn[i],c("x","y","neuron")]

    colnames(res)<-c("x0",'y0',"conn")

    result2<-cbind(result,res)
    result2$y<-dfneu$y[dfneu$neuron== df1$neuron[i]]
    result2$x<-dfneu$x[dfneu$neuron== df1$neuron[i]]
    result2[c("neuron","x0","y0","x","y","weights","conn")]
  }))

}
dfs_avNNet<-function(m,mod1,var_names){


  if(missing(m)){
    if(missing(mod1)){
      return("requires nnet object or train object from caret")
    } else{
      if(missing(var_names)){
        return("require var_names")
      }
    }
  } else{
    mod1<-m$model[[1]]
    var_names<-m$xNames

  }

  dfneu<-neurons_avNNet(mod1)

  wb<-weights_B_avNNet(mod1)
  w<-weights_avNNet(mod1)
  df1<-rbind(w,wb)
  df_segments<-segments_avNNet(df1,dfneu)

  dfneu$neu_type<-"neuron"

  dfneu$neu_type[dfneu$neuron>  sum(mod1$n)]<-"Bias"
  attr(dfneu,"var_names")<-var_names
  if(!missing(m)){
    all_weights<-sapply(m$model,function(mod1){
      dfneu<-neurons_avNNet(mod1)
      wb<-weights_B_avNNet(mod1)
      w<-weights_avNNet(mod1)
      df1<-rbind(w,wb)
      df_segments<-segments_avNNet(df1,dfneu)
      df_segments$weights
    })
    mean_wei<-apply(all_weights,1,mean)
    df_segments$weights<-mean_wei
    df_segments
  }
  return(list(
    dfneu=dfneu,
    df_segments=df_segments
  ))
}
get_dfneu<-function(size, bias=T){
  mean_neu<-max(sapply(size,function(x) mean(seq_len(x))))
  dfneu<-do.call(rbind,lapply(seq_along(size),function(i){
    data.frame(neu=seq_len(size[[i]]),x=i,y=rescale_to_mean(seq_len(size[[i]]),mean_neu))
  }))
  dfneu$neuron<-1:nrow(dfneu)

  if(isTRUE(bias)){
    dfb<- data.frame(neu=seq_along(size[-1]),
                     y=max(dfneu$y)+1,
                     x=seq_along(size[-1])+0.5,
                     neuron=seq_along(size[-1])+max(dfneu$neuron))
    dfneu<-rbind(dfneu,dfb)
  }

  dfneu$x<-scales::rescale(dfneu$x,c(0,1))
  dfneu$y<-scales::rescale(dfneu$y,c(0,1))
  dfneu

}

dfs_mlpML<-function(model){
  weis<-NeuralNetTools::neuralweights(model$finalModel)
  size<-weis$struct
  dfneu<-get_dfneu(weis$struct,bias=F)
  xx<-unlist(sapply(seq_along(size),function(i){
    rep(i,size[[i]])
  } )[-1])-1
  xs<-unlist(sapply(seq_along(size),function(i){
    rep(i,size[[i]])
  } )[-1])
  cize<-
    sapply(seq_along(size),function(i){
      sum(size[1:i])
    })
  cize<-c(0,cize)
  dfseg<-do.call(rbind,lapply(seq_along(weis$wts),function(i){
    x<-na.omit(weis$wts[[i]])
    data.frame(weights=x,neuron=i,conn=(1:length(x))+cize[xx][i],x=xs[i])
  }))

  dfseg$neuron<-dfseg$neuron+size[1]
  df_segments<-segments_avNNet(dfseg,dfneu)
  attr(dfneu,'var_names')<-model$finalModel$xNames
  return(list(df_segments=df_segments,dfneu=dfneu))
}
dfs_monMLP<-function(m){

  w1<-m$model
  wei<-w1[[1]]





  size<-as.numeric(c(sapply(wei, function(x){
    nrow(x)-1
  }),ncol(wei[[length(wei)]])))
  dfneu<-get_dfneu(size)


  res_w<-list()
  start<-size[[1]]
  for(i in seq_along(wei)){
    conn_start=sum(size[i:1])-size[i]
    start<-sum(size[i:1])
    re<-reshape2::melt(wei[[i]])
    colnames(re)[1:3]<-c("conn","neuron",'weights')
    re$conn<-re$conn+conn_start
    re$neuron<-re$neuron+start
    re$x<-i
    re$neu_type<-"neuron"
    re$neu_type[re$conn>start]<-"constant"
    res_w[[i]]<-re
    start<-size[[i+1]]
  }
  df2<-do.call(rbind,res_w)
  df2$conn[df2$neu_type=="constant"]<-as.numeric(as.character(factor(df2$conn[df2$neu_type=="constant"], labels=(sum(size)+1):(sum(size)+(length(size)-1)))))
  attr(dfneu,"var_names")<-colnames(attr(w1,'x'))


  df_segments<-segments_avNNet(df2,dfneu)
  return(
    list(
      dfneu=dfneu,
      df_segments=df_segments
    )
  )
}
ggplot_avNNet<-function(m=NULL,mod1,df_segments,dfneu,
                        neu_radius=0.05,
                        neuron_fill="Grey",neuron_border="Grey",weight_palette,bias_lab="B",
                        xlim=c(-.5,1.5),text_size=5,
                        linewidth=1,size.layers.text=5,legname="Weight average") {
  if(!is.null(m)){
    mod1<-m$model[[1]]

  }
  if(inherits(mod1,"rsnns")){
    weis<-NeuralNetTools::neuralweights(mod1)
    mod1$n<-weis$struct
    mod1$obsLevels
    if(length(mod1$obsLevels)>1){
      mod1$residuals<-matrix(mod1$obsLevels,nrow=1,dimnames=list("1",as.character(mod1$obsLevels)))
    }
  }

  out_names<-colnames(mod1$residuals)
  size<-mod1$n
  if(is.null(out_names)){
    out_names<-colnames(attr(mod1$model,"y"))
      }
  if(is.null(size)){
    size<-as.numeric(c(sapply(mod1$model[[1]], function(x){
      nrow(x)-1
    }),ncol(mod1$model[[1]][[length(mod1$model[[1]])]])))
  }

  if(is.null(out_names)){
    if(m$problemType=="Regression"){
      out_names<-attr(m,"supervisor")
    }
  }
  var_names<-attr(dfneu,'var_names')
  pol<-pol_avNNet(dfneu,neu_radius)
  dfinput<-dfinput_avNNet(dfneu,neu_radius)
  dfoutput<-dfoutput_avNNet(dfneu,neu_radius)

  df_segments$lwd<-scales::rescale(abs(df_segments$weights),c(linewidth-0.5,linewidth+0.5))

  if(missing(weight_palette)){
    weight_palette=viridis::viridis
  }
  p<-ggplot(pol)+
    geom_segment(aes(x=x0,y=y0,xend=x,yend=y,color=weights),data=df_segments,linewidth=df_segments$lwd)+
    geom_polygon(aes(x,y,group=group),data=pol,colour=neuron_border,fill=neuron_fill)+
    scale_color_gradientn(name=legname,colors=weight_palette(100))+
    geom_segment(aes(x=x,y=y,xend=xend,yend=y),data=dfinput,arrow=arrow(length = unit(0.1,"inches")))+
    geom_text(data=dfinput,aes(x,y),
              label=var_names,
              hjust=1,size=text_size) + xlim(xlim)+
    geom_text(data=dfoutput,aes(x,y),label=out_names,hjust=0,size=text_size)


  for(i in seq_along(size)){
    if(i==1){
      names(size)[i]<-"Input"
    }else if(i==length(size)){
      names(size)[i]<-"Output"
    } else{
      names(size)[i]<-"Hidden"
    }
  }
  names(size)[names(size)=="Hidden"]<-paste0(names(size)[names(size)=="Hidden"],1:length(names(size)[names(size)=="Hidden"]))
  neus<-dfneu[dfneu$neuron<=sum(size),]
  y<-max(dfneu$y)+
    1/max(size)
  x<-unique(neus$x)

  p<-p+geom_text(data= data.frame(x,y)[1:length(size),],aes(x,y),label=names(size),size=size.layers.text)
  bs<-dfneu[dfneu$neuron>sum(size),]
  p+geom_text(data= bs,aes(x,y),label=paste0(bias_lab,1:nrow(bs)),size=size.layers.text*.8)+theme_void()

}





predict_cforest_tree<-function(tree,newdata, model){
  res<-c()
  for(i in 1:nrow(newdata)){
    node<-tree
    newrow<-newdata[i,]
    repeat({
      if(!is.null(node$psplit$variableName)){
        go_left<-newrow[[node$psplit$variableName]]<=node$psplit$splitpoint
      } else{
        go_left<-F
      }
      if(!is.null(node$psplit$variableName)){
        go_right<-newrow[[node$psplit$variableName]]>node$psplit$splitpoint
      } else{
        go_right<-F
      }
      if(go_left){
        node<-node$left
      } else if(go_right) {
        node<-node$right
      }
      if(node$terminal){
        res[length(res)+1]<- which.max(node$prediction)
        break()
      }

    })
  }
  factor(model$levels[res],levels=model$levels)
}


gety_datalist_model<-function(m){
  ya<-gsub(" ","",attr(m,"Y"))
  strsplit(ya,'::')[[1]][[1]]
}
get_metric_model<-function(m){
  metrics<-metrics_default<-m$results[rownames(m$bestTune),]
  metrics_default[colnames(m$bestTune)]<-NULL
  metrics_tunning<-metrics[colnames(m$bestTune)]
  colnames(metrics_tunning)<-paste0("tun_",colnames(metrics_tunning))
  metrics_final<-cbind(metrics_default,metrics_tunning)
  metrics_final
}

table_training_datalist<-function(saved_data,data_x){
  data<-saved_data[[data_x]]
  res0<-lapply(seq_along(available_models),function(i){

    cmodel<-as.character(available_models)[i]
    models_in<-attr(data,cmodel)

    lapply(seq_along(models_in),function(j){

      model_name<-names(models_in)[j]
      m<-models_in[[model_name]][[1]]
      cbind(data.frame(
        modelid=paste0(i,j),
        Model=cmodel,
        Model_name=model_name,
        Model_type=m$modelType,
        Datalist_x=data_x,
        Datalist_y=gety_datalist_model(m),
        Y=attr(m,"supervisor"),
        nobs=nrow(m$training)
      ),
      get_metric_model(m))





    })
  })

  res<-res0[sapply(res0,length)>0]
  tts<- unlist(res,recursive = F)


  isclass<-unlist(sapply(tts,function(x){
    if(inherits(x,"data.frame")){
      x$Model_type=="Classification"} else{F}
  }))
  isreg<-unlist(sapply(tts,function(x){
    if(inherits(x,"data.frame")){
      x$Model_type=="Regression"} else{F}
  }))
  class=NULL
  reg=NULL
  if(length(isclass)>0){
    class<- data.frame(data.table::rbindlist(tts[isclass],fill=T))
  }
  if(length(isreg)>0){
    reg<- data.frame(data.table::rbindlist(tts[isreg],fill=T))
  }



  return(list(class=class,reg=reg))
}
table_test_datalist<-function(saved_data,data_x){
  data<-saved_data[[data_x]]

  res0<-lapply(seq_along(available_models),function(i){
    cmodel<-as.character(available_models)[[i]]
    models_in<-attr(data,cmodel)
    results<-list()
    for(j in seq_along(models_in)){
      model_name<-names(models_in)[[j]]

      #model_name="rf model"
      m<-models_in[[model_name]][[1]]
      if(inherits(m,"train")){
        if(inherits(attr( m,"test"),"data.frame")){


          test_data<-as.matrix(attr(m,"test"))
          colnames(test_data)<-colnames(getdata_model(m))
          pred<-suppressWarnings(predict(m,test_data))
          if(inherits(attr(m,"sup_test"),"data.frame")){
            obs<-attr(m,"sup_test")[,1]
          } else{
            obs<-attr(m,"sup_test")
          }
          if(length(obs)!=length(pred)){
            df<-"None"
          } else{
            df<-cbind(data.frame(modelid=paste0(i,j),Datalist=data_x,Model=cmodel,Model_name=model_name,Model_type=m$modelType,nobs=length(pred),rbind(caret::postResample(pred,obs))
            ))
          }


        } else{df<-"None"}
        results[[model_name]]<-df
      }

    }
    if(length(results)>0)
      results
  })
  res<-res0[sapply(res0,length)>0]
  tts<-unlist(res,recursive = F)

  isclass<-unlist(sapply(tts,function(x){
    if(inherits(x,"data.frame")){
      x$Model_type=="Classification"} else{F}
  }))
  isreg<-unlist(sapply(tts,function(x){
    if(inherits(x,"data.frame")){
      x$Model_type=="Regression"} else{F}
  }))
  class=NULL
  reg=NULL
  if(any(isclass)){
    class<-do.call(rbind,tts[isclass])
  }
  if(any(isreg)){
    reg<-do.call(rbind,tts[isreg])
  }

  return(list(class=class,reg=reg))

}



get_datalist_model_metrics<-function(saved_data,data_x){



  ttr<-table_training_datalist(saved_data,data_x)

  if(sum(sapply(ttr,length))==0){
    return(NULL)
  }
  tts<-table_test_datalist(saved_data,data_x)
  show_tunning_params<-F
  modelType="class"
  result<-lapply(c("class","reg"),function(modelType){
    train<-ttr[[modelType]]

    if(!is.null(train)){
      rownames(train)<-train$modelid
      test<-tts[[modelType]]
      rownames(test)<-test$modelid
      train$Datalist_x<-NULL
      train$modelid<-NULL
      test$modelid<-NULL
      tuncols<-which(grepl("tun_",colnames(train)))
      tun<-NULL
      if(length(tuncols)>0){
        colnames(train)[tuncols]<-gsub("tun_","",colnames(train)[tuncols])
        tun<-as.character(apply(train[tuncols],1,function(x){
          x<-unlist(x)
          params<-x[!is.na(x)]
          parms<-sapply(names(params),function(i){
            paste(i,params[i], sep="=")
          })
          paste0(parms, collapse="<br>")
        }))
      }


      train_df<-train
      train_df[tuncols]<-NULL
      train_df$tunning<-tun
      #if()
      ncol_train=ncol(train_df)
      attr(train_df,"ncol_train")<-ncol_train
      if(length(test)>0){
        test$Datalist<-NULL
        test$Model<-NULL
        test$Model_name<-NULL
        test$Model_type<-NULL
        colnames(test)<-paste0("partition_",colnames(test))

        train_df[,colnames(test)]<-NA
        train_df[rownames(test),colnames(test)]<-test
        attr(train_df,"ncol_train")<-ncol_train
      }

      train_df
    }})
  names(result)<-c("class","reg")
  pic<-sapply(result,function(x) nrow(x)>0)
  if(length(pic)>0){
    result[pic]}

}

# ---- Forecast horizon curves (Prequential temporal / spatiotemporal CV) ---------
# The folds are built with the largest horizon H. Each test row gets its lead
# (blocks after the fold's last training block), so the metrics for the cumulative
# window t+1...t+h (h <= H) come from the same origins and models. With a gap of g
# blocks the windows are t+g+1...t+g+h.

#' @export
parse_horizon_blocks<-function(x){
  if(is.null(x)) return(integer(0))
  h<-suppressWarnings(as.numeric(trimws(unlist(strsplit(as.character(x),"[,; ]+")))))
  h<-h[!is.na(h)&h>=1&h==round(h)]
  sort(unique(as.integer(h)))
}

# list (one data.frame per caret fold) with rowIndex and lead of each test row
#' @export
make_horizon_map<-function(caret_folds){
  dat<-caret_folds$data
  if(is.null(dat)||!"fold_time"%in%names(dat)) return(NULL)
  block<-as.integer(dat$fold_time)
  rows<-if(".rowid_original"%in%names(dat)) dat$.rowid_original else seq_len(nrow(dat))
  block_of<-rep(NA_integer_,max(rows))
  block_of[rows]<-block
  maps<-mapply(function(tr,te){
    data.frame(rowIndex=te,lead=block_of[te]-max(block_of[tr],na.rm=TRUE))
  },caret_folds$index,caret_folds$indexOut,SIMPLIFY=FALSE)
  names(maps)<-names(caret_folds$index)
  maps
}

# window = "cumulative" (t+1...t+h) or "exact" (block t+h only). Models evaluated with the
# recursive strategy use their recursive predictions (attr(m,"recursive")); otherwise the
# caret test predictions (observed predictors), with the horizons beyond the leakage-free
# limit (attr(m,"cvt")$leak$h_valid) flagged.
#' @export
horizon_curve_data<-function(m,window="cumulative"){
  cvt<-attr(m,"cvt")
  if(is.null(cvt$horizon_map)) cvt<-attr(m,"cvst")
  hmap<-cvt$horizon_map
  horizons<-cvt$horizons
  gap<-suppressWarnings(as.integer(cvt$gap_blocks))
  if(!length(gap)||is.na(gap[1])) gap<-0L
  gap<-gap[1]
  if(is.null(hmap)||!length(horizons)||is.null(m$pred)) return(NULL)
  rec<-attr(m,"recursive")
  mode<-"observed"
  if(!is.null(rec$pred)&&nrow(rec$pred)){
    pred<-rec$pred[!is.na(rec$pred$pred),,drop=FALSE]
    mode<-"recursive"
  } else{
    pred<-m$pred
    tune_names<-intersect(colnames(m$bestTune),colnames(pred))
    for(nm in tune_names){
      pred<-pred[pred[[nm]]==m$bestTune[[nm]],,drop=FALSE]
    }
    leads<-do.call(rbind,lapply(names(hmap),function(f) data.frame(Resample=f,hmap[[f]],stringsAsFactors=FALSE)))
    pred<-merge(pred,leads,by=c("Resample","rowIndex"))
  }
  if(!nrow(pred)) return(NULL)
  # position inside the test window (1 = first block after the gap)
  pred$lead<-pred$lead-gap
  metrics<-function(p,o) suppressWarnings(caret::postResample(p,o))
  in_window<-function(lead,h) if(identical(window,"exact")) lead==h else lead<=h

  by_fold<-do.call(rbind,lapply(horizons,function(h){
    do.call(rbind,lapply(sort(unique(pred$Resample)),function(f){
      sub<-pred[pred$Resample==f&in_window(pred$lead,h),,drop=FALSE]
      if(!nrow(sub)) return(NULL)
      data.frame(Horizon=h,Fold=f,n=nrow(sub),t(metrics(sub$pred,sub$obs)),check.names=FALSE)
    }))
  }))
  if(is.null(by_fold)) return(NULL)
  metric_names<-setdiff(colnames(by_fold),c("Horizon","Fold","n"))
  h_valid<-if(identical(mode,"observed")&&length(cvt$leak$h_valid)&&is.finite(cvt$leak$h_valid)) cvt$leak$h_valid else NULL

  summary<-do.call(rbind,lapply(horizons,function(h){
    bf<-by_fold[by_fold$Horizon==h,,drop=FALSE]
    pooled_rows<-pred[in_window(pred$lead,h),,drop=FALSE]
    pooled<-metrics(pooled_rows$pred,pooled_rows$obs)
    out<-data.frame(Horizon=h,Window=if(identical(window,"exact")) paste0("t+",gap+h) else paste0("t+",gap+1,"...t+",gap+h),
                    Folds=nrow(bf),Predictions=nrow(pooled_rows),check.names=FALSE)
    if(!is.null(h_valid)) out$Leakage<-if(h>h_valid) "yes" else "no"
    for(mt in metric_names){
      out[[paste0(mt,"_mean")]]<-mean(bf[[mt]],na.rm=TRUE)
      out[[paste0(mt,"_sd")]]<-stats::sd(bf[[mt]],na.rm=TRUE)
      out[[paste0(mt,"_pooled")]]<-unname(pooled[mt])
    }
    out
  }))
  list(summary=summary,by_fold=by_fold,metrics=metric_names,horizons=horizons,gap=gap,mode=mode,window=window,h_valid=h_valid)
}

#' @export
gg_horizon_curve<-function(hz,metric,show_sd=TRUE,show_pooled=FALSE,color="#05668D",base_size=12,title="Performance by forecast horizon"){
  df<-hz$summary
  df$mean<-df[[paste0(metric,"_mean")]]
  df$sd<-df[[paste0(metric,"_sd")]]
  df$pooled<-df[[paste0(metric,"_pooled")]]
  p<-ggplot2::ggplot(df,ggplot2::aes(x=Horizon,y=mean))
  if(isTRUE(show_sd)){
    p<-p+ggplot2::geom_errorbar(ggplot2::aes(ymin=mean-sd,ymax=mean+sd),width=0.15,color=color,na.rm=TRUE)
  }
  p<-p+ggplot2::geom_line(color=color)+ggplot2::geom_point(color=color,size=2.5)
  leaky<-"Leakage"%in%names(df)&&any(df$Leakage=="yes")
  if(leaky){
    p<-p+ggplot2::geom_point(data=df[df$Leakage=="yes",,drop=FALSE],color="#B71C1C",size=3.2,shape=21,fill="#FDECEA",stroke=1)
  }
  if(isTRUE(show_pooled)){
    p<-p+ggplot2::geom_point(ggplot2::aes(y=pooled),shape=4,size=3,color="gray30",na.rm=TRUE)
  }
  gap<-if(is.null(hz$gap)) 0L else hz$gap
  exact<-identical(hz$window,"exact")
  cap<-c(if(identical(hz$mode,"recursive")) "Recursive forecasts from each origin (predicted response values feed the lags and windows of the response).",
         if(leaky) "Red: horizons with data leakage (the predictors derived from the response use values observed after the origin).",
         if(isTRUE(show_pooled)) "x = pooled over all test predictions")
  p+ggplot2::scale_x_continuous(breaks=df$Horizon,labels=paste0("t+",gap+df$Horizon))+
    ggplot2::labs(
      x=if(exact) paste0("Forecast horizon (test block t+h",if(gap>0) paste0("; gap = ",gap," blocks") else "",")") else
        if(gap>0) paste0("Forecast horizon (cumulative test window t+",gap+1,"...t+h; gap = ",gap," blocks)") else "Forecast horizon (cumulative test window t+1...t+h)",
      y=paste0(metric,if(isTRUE(show_sd)) " (mean +/- SD across folds)" else " (mean across folds)"),
      title=title,
      caption=if(length(cap)) paste(cap,collapse="\n") else NULL
    )+
    ggplot2::theme_bw(base_size=base_size)
}

# ---- Recursive (iterated) horizon evaluation ---------------------------------------------
# From each validation origin the response is forecast one time step at a time: the
# predictors derived from the response (Temporal Features: lags, rolling windows, changes,
# anomalies, LED, cumulative) are recomputed from the response history in which the values
# after the origin are the model's own predictions. The other predictors are taken as known
# or kept at their last value observed at the origin.

# time values as sortable numbers (same reading as the Temporal Features builder)
#' @export
sl_time_numeric<-function(tt){
  if(inherits(tt,"Date")||inherits(tt,"POSIXt")||is.numeric(tt)) return(as.numeric(tt))
  d<-tryCatch({
    g<-guess_time_settings(tt)
    if(g$type%in%c("date","datetime")) as.numeric(convert_time_column(tt,g$type,g$format,g$custom)) else NULL
  },error=function(e) NULL)
  if(!is.null(d)&&!all(is.na(d))) return(d)
  x<-as.character(tt)
  as.numeric(match(x,sort(unique(x))))
}

# series of the Temporal Features builder (coordinates, rounded to 6 decimals)
#' @export
sl_series_groups<-function(dat,grouped=TRUE){
  coords<-attr(dat,"coords")
  if(!isTRUE(grouped)||is.null(coords)) return(factor(rep("all",nrow(dat))))
  coords<-as.data.frame(coords)
  if(nrow(coords)!=nrow(dat)||!ncol(coords)) return(factor(rep("all",nrow(dat))))
  key<-data.frame(lapply(coords,function(x) if(is.numeric(x)) round(x,6) else x),check.names=FALSE)
  interaction(key,drop=TRUE,lex.order=TRUE)
}

# rows of each series in time order, and the position of each row in its series
sl_series_index<-function(groups,tt_num){
  o<-order(tt_num,seq_along(tt_num),na.last=TRUE)
  idx<-lapply(levels(groups),function(g) o[groups[o]==g])
  idx<-idx[lengths(idx)>0]
  series<-integer(length(tt_num))
  pos<-integer(length(tt_num))
  for(s in seq_along(idx)){
    series[idx[[s]]]<-s
    pos[idx[[s]]]<-seq_along(idx[[s]])
  }
  list(idx=idx,series=series,pos=pos)
}

# recipe of a derived predictor, from its metadata (the name gives the summary for older
# Datalists whose metadata do not record it)
#' @export
sl_tf_spec<-function(mr){
  col<-function(nm,default=NA) if(nm%in%names(mr)&&length(mr[[nm]])&&!is.na(mr[[nm]])) mr[[nm]] else default
  f<-mr$feature
  esc<-gsub("([][{}()+*^$|\\\\?.])","\\\\\\1",mr$source)
  suf<-sub("\\.[0-9]+$","",sub(paste0("^(.*_)?",esc,"_"),"",f))
  detail<-col("detail",switch(mr$type,
                             rolling=sub("^roll[0-9]+_","",suf),
                             change=if(grepl("^pct_change",suf)) "pct" else "diff",
                             anomaly=sub("^anom_[0-9]+_","",suf),
                             cumulative=sub("^cum_","",suf),
                             NA))
  list(feature=f,source=mr$source,type=mr$type,k=as.numeric(mr$k),detail=detail,past_only=!isTRUE(mr$includes_current),
       grouped=isTRUE(mr$grouped),alpha=as.numeric(col("alpha",0.3)),initial=as.numeric(col("initial",NA)),center=col("center",NA))
}

# one series (time ordered): the same formulas as the Temporal Features builder
#' @export
sl_tf_series<-function(spec,z){
  z<-as.numeric(z)
  n<-length(z)
  lagv<-function(x,l){ if(l>=length(x)) return(rep(NA_real_,length(x))); c(rep(NA_real_,l),utils::head(x,-l)) }
  past<-function(v) if(isTRUE(spec$past_only)) lagv(v,1) else v
  safe<-function(x,fun){
    x<-x[!is.na(x)]
    if(!length(x)) return(NA_real_)
    v<-suppressWarnings(fun(x))
    if(!length(v)) return(NA_real_)
    v<-as.numeric(v[1])
    if(is.na(v)||is.nan(v)||is.infinite(v)) NA_real_ else v
  }
  roll<-function(x,k,fun){
    out<-rep(NA_real_,length(x))
    if(k<1||!length(x)) return(out)
    for(i in seq_along(x)) out[i]<-fun(x[seq(max(1,i-k+1),i)])
    out
  }
  stat_fun<-function(s) switch(s,
                               sd=function(y) safe(y,stats::sd),
                               min=function(y) safe(y,min),
                               max=function(y) safe(y,max),
                               median=function(y) safe(y,stats::median),
                               q25=function(y) safe(y,function(v) stats::quantile(v,.25,names=FALSE)),
                               q75=function(y) safe(y,function(v) stats::quantile(v,.75,names=FALSE)),
                               iqr=function(y) safe(y,stats::IQR),
                               function(y) safe(y,mean))
  k<-spec$k
  switch(spec$type,
         lag=lagv(z,k),
         rolling={
           v<-if(identical(spec$detail,"slope")) roll(z,k,function(w){ ok<-!is.na(w); if(sum(ok)<2) return(NA_real_); stats::coef(stats::lm(w[ok]~seq_along(w)[ok]))[2] }) else roll(z,k,stat_fun(spec$detail))
           past(v)
         },
         change={
           lagged<-lagv(z,k)
           v<-if(identical(spec$detail,"pct")){ p<-(z-lagged)/lagged; p[is.nan(p)|is.infinite(p)]<-NA_real_; p } else z-lagged
           past(v)
         },
         anomaly={
           v<-switch(spec$detail,
                     median_dev=z-roll(z,k,stat_fun("median")),
                     zscore={ s<-(z-roll(z,k,stat_fun("mean")))/roll(z,k,stat_fun("sd")); s[is.nan(s)|is.infinite(s)]<-NA_real_; s },
                     z-roll(z,k,stat_fun("mean")))
           past(v)
         },
         led={
           alpha<-min(max(spec$alpha,.Machine$double.eps),1)
           state<-spec$initial
           has<-!is.na(state)
           out<-rep(NA_real_,n)
           for(i in seq_len(n)){
             out[i]<-if(has) state else NA_real_
             if(!is.na(z[i])){ state<-if(has) alpha*z[i]+(1-alpha)*state else z[i]; has<-TRUE }
           }
           if(isTRUE(spec$center)){
             ok<-!is.na(out)
             cm<-ifelse(cumsum(ok)>0,cumsum(ifelse(ok,out,0))/cumsum(ok),NA_real_)
             out<-out-cm
           }
           out
         },
         cumulative={
           ok<-!is.na(z)
           z0<-ifelse(ok,z,0)
           v<-if(identical(spec$detail,"mean")) ifelse(cumsum(ok)>0,cumsum(z0)/cumsum(ok),NA_real_) else if(identical(spec$detail,"sum")) cumsum(z0) else rep(NA_real_,n)
           past(v)
         },
         rep(NA_real_,n))
}

# derived predictor for all rows (x: source values; si: sl_series_index)
sl_tf_compute<-function(spec,x,si){
  out<-rep(NA_real_,length(x))
  for(idx in si$idx){
    r<-sl_tf_series(spec,x[idx])
    if(length(r)==length(idx)) out[idx]<-r
  }
  out
}

# derived predictor for some rows only, from the recent history of their series
sl_tf_rows<-function(spec,x,si,rows){
  need<-switch(spec$type,lag=spec$k+1,rolling=spec$k+1,change=spec$k+2,anomaly=spec$k+2,NA)
  vapply(rows,function(r){
    idx<-si$idx[[si$series[r]]][seq_len(si$pos[r])]
    if(!is.na(need)&&length(idx)>need) idx<-utils::tail(idx,need)
    v<-sl_tf_series(spec,x[idx])
    v[length(v)]
  },numeric(1))
}

# share of the rows where the recomputed values (a) match the stored ones (b). The first rows
# of each series are skipped: they are often removed (missing lags) after the features were
# created, which shortens the history. LED and cumulative summaries keep a small effect of
# the removed history, so they are compared with a tolerance.
sl_tf_match<-function(a,b,pos,spec){
  a<-as.numeric(a)
  b<-as.numeric(b)
  long<-spec$type%in%c("led","cumulative")
  burn<-if(long) 20 else if(is.na(spec$k)) 2 else spec$k+2
  ok<-!is.na(a)&!is.na(b)&pos>burn
  if(sum(ok)<5) ok<-!is.na(a)&!is.na(b)
  if(!any(ok)) return(1)
  if(long){
    # same variable up to the level shift left by the lost history (corrected by the anchor
    # of the recursive forecasts)
    if(sum(ok)<3||stats::sd(b[ok])==0) return(as.numeric(isTRUE(all.equal(a[ok],b[ok]))))
    return(as.numeric(isTRUE(stats::cor(a[ok],b[ok])>0.95)))
  }
  tol<-1e-6*max(1,max(abs(b[ok])))
  mean(abs(a[ok]-b[ok])<=tol+1e-12)
}

# everything the recursive evaluation needs, checked once per model: the derived predictors
# of the response reproduced from its values, the time order and the series of the rows.
# data_x: X Datalist (all rows); predictors: columns used by the model; response: its name;
# y_all: response for the rows of data_x (NA when unknown)
#' @export
sl_recursive_setup<-function(data_x,predictors,response,y_all){
  fail<-function(reason) list(ok=FALSE,reason=reason)
  if(!is.numeric(y_all)) return(fail("The recursive evaluation is available for regression models (numeric response)."))
  meta<-attr(data_x,"temporal_feature_meta")
  if(is.null(meta)||!nrow(meta)) return(fail("No predictor was created with Temporal Features."))
  m<-meta[meta$feature%in%predictors&meta$source%in%response&!meta$uses_future,,drop=FALSE]
  if(!nrow(m)) return(fail("No predictor is derived from the response (lags, windows, changes...): the recursive evaluation would give the same results as the observed predictors."))
  if(any(m$includes_current)) return(fail(paste0("Predictors derived from the response include its value at time t (",paste(utils::head(m$feature[m$includes_current],3),collapse=", "),"): recreate them with Past values only.")))
  if(!all(m$type%in%c("lag","rolling","change","anomaly","led","cumulative"))) return(fail("Some predictors derived from the response cannot be recomputed."))
  specs<-lapply(seq_len(nrow(m)),function(i) sl_tf_spec(m[i,,drop=FALSE]))
  # response history: the source column of the X Datalist (all its rows) when present
  hist0<-as.numeric(y_all)
  src<-unique(m$source)[1]
  if(src%in%colnames(data_x)&&is.numeric(data_x[[src]])){
    xs<-as.numeric(data_x[[src]])
    both<-!is.na(xs)&!is.na(hist0)
    if(any(both)&&max(abs(xs[both]-hist0[both]))>1e-8*max(1,max(abs(hist0[both])))) return(fail(paste0("The response differs from the variable '",src,"' of the X Datalist used to create the derived predictors.")))
    hist0[is.na(hist0)]<-xs[is.na(hist0)]
  }
  # time order: the time column recorded with the features, or the one that reproduces them
  tattr<-attr(data_x,"time")
  cands<-unique(c(if("time_col"%in%names(m)) stats::na.omit(m$time_col),if(!is.null(tattr)) colnames(tattr),NA))
  ids<-rownames(data_x)
  for(tc in cands){
    tt<-if(is.na(tc)) seq_len(nrow(data_x)) else{
      if(is.null(tattr)||!tc%in%colnames(tattr)||!all(ids%in%rownames(tattr))) next
      sl_time_numeric(tattr[ids,tc,drop=TRUE])
    }
    si<-list(sl_series_index(sl_series_groups(data_x,FALSE),tt),sl_series_index(sl_series_groups(data_x,TRUE),tt))
    match_of<-function(v){
      s<-si[[if(v$grouped) 2 else 1]]
      sl_tf_match(sl_tf_compute(v,hist0,s),data_x[[v$feature]],s$pos,v)
    }
    for(i in seq_along(specs)){
      sp<-specs[[i]]
      # LED: the centring is not recorded in older metadata; both are tried
      if(identical(sp$type,"led")&&is.na(sp$center)){
        specs[[i]]$center<-match_of(utils::modifyList(sp,list(center=TRUE)))>match_of(utils::modifyList(sp,list(center=FALSE)))
      }
    }
    ok<-vapply(specs,function(sp) match_of(sp)>=0.9,logical(1))
    bad_feats<-m$feature[!ok]
    if(all(ok)){
      grp<-sl_series_groups(data_x,any(vapply(specs,function(s) s$grouped,logical(1))))
      if(anyDuplicated(paste(as.character(grp),tt))) return(fail("Several observations share the same time step within a series: the recursive forecasts need one observation per time step and series."))
      return(list(ok=TRUE,specs=specs,features=m$feature,hist0=hist0,tt=tt,ids=ids,si=si,
                  site=sl_series_groups(data_x,TRUE),time_col=tc,data_x=data_x))
    }
  }
  fail(paste0("The predictors derived from the response could not be reproduced from its values (",paste(utils::head(bad_feats,3),collapse=", "),"). Recreate them with Temporal Features on this Datalist."))
}

# recursive forecasts of one fold. fit: model trained on the fold; X: numeric matrix of the
# predictors for all rows of data_x (rownames = ctx$ids); known: predictors known in the future
#' @export
sl_recursive_fold<-function(fit,ctx,X,train_ids,test_ids,known=character(0),exo="last"){
  tt<-ctx$tt
  pos_tr<-match(train_ids,ctx$ids)
  pos_te<-match(test_ids,ctx$ids)
  t0<-max(tt[pos_tr],na.rm=TRUE)
  t1<-max(tt[pos_te],na.rm=TRUE)
  fut<-!is.na(tt)&tt>t0
  hist<-ctx$hist0
  hist[fut]<-NA
  # the other predictors: last value observed at the origin in each site, or known
  if(identical(exo,"last")){
    cols<-setdiff(colnames(X),c(ctx$features,known))
    if(length(cols)){
      for(s in levels(ctx$site)){
        rows<-which(ctx$site==s)
        past<-rows[!is.na(tt[rows])&tt[rows]<=t0]
        futr<-rows[fut[rows]]
        if(!length(past)||!length(futr)) next
        last<-past[which.max(tt[past])]
        X[futr,cols]<-matrix(X[last,cols],nrow=length(futr),ncol=length(cols),byrow=TRUE)
      }
    }
  }
  # anchor of each series: the stored value at its first time after the origin (known at the
  # origin) minus the recomputed one, which differ only when early rows were removed after the
  # features were created (the history before them is lost); 0 otherwise
  offs<-lapply(ctx$specs,function(sp){
    si<-ctx$si[[if(sp$grouped) 2 else 1]]
    d<-numeric(length(si$idx))
    for(s in seq_along(si$idx)){
      idx<-si$idx[[s]]
      f1<-idx[fut[idx]][1]
      if(is.na(f1)) next
      r<-sl_tf_rows(sp,hist,si,f1)
      st<-X[f1,sp$feature]
      if(!is.na(r)&&!is.na(st)) d[s]<-st-r
    }
    d
  })
  steps<-sort(unique(tt[fut&tt<=t1]))
  pred<-rep(NA_real_,length(tt))
  filled<-0L
  for(s in steps){
    R<-which(tt==s)
    for(j in seq_along(ctx$specs)){
      sp<-ctx$specs[[j]]
      si<-ctx$si[[if(sp$grouped) 2 else 1]]
      v<-sl_tf_rows(sp,hist,si,R)+offs[[j]][si$series[R]]
      bad<-is.na(v)
      filled<-filled+sum(bad)
      v[bad]<-X[R[bad],sp$feature]
      X[R,sp$feature]<-v
    }
    p<-tryCatch(as.numeric(stats::predict(fit,newdata=X[R,,drop=FALSE])),error=function(e) rep(NA_real_,length(R)))
    if(length(p)!=length(R)) p<-rep(NA_real_,length(R))
    pred[R]<-p
    hist[R]<-p
  }
  list(pred=pred[pos_te],steps_ahead=match(tt[pos_te],steps),filled=filled)
}

# recursive evaluation of all folds: a model per fold with the selected hyperparameters
# (args_train: the caret::train arguments of the final model), then the recursive forecasts
#' @export
sl_recursive_eval<-function(args_train,ctx,x,y,caret_folds,hmap,known=character(0),exo="last",seed=NULL,progress=NULL,best_tune=NULL){
  X<-matrix(NA_real_,length(ctx$ids),ncol(x),dimnames=list(ctx$ids,colnames(x)))
  data_x<-ctx$data_x
  for(cn in colnames(x)) if(cn%in%colnames(data_x)) X[,cn]<-as.numeric(data_x[[cn]])
  # rows of the training data keep the values used in training
  X[rownames(x),]<-as.matrix(x)
  xm<-as.matrix(x)
  out<-list()
  filled<-0L
  folds<-names(caret_folds$index)
  for(i in seq_along(folds)){
    f<-folds[i]
    if(is.function(progress)) progress(i/length(folds),f)
    tr<-caret_folds$index[[f]]
    te<-caret_folds$indexOut[[f]]
    a<-args_train
    a[[1]]<-xm[tr,,drop=FALSE]
    a[[2]]<-y[tr]
    a$trControl<-caret::trainControl(method="none")
    a$tuneGrid<-best_tune
    a$tuneLength<-NULL
    if(!is.null(a$weights)) a$weights<-a$weights[tr]
    fit<-tryCatch(suppressWarnings({ if(!is.null(seed)) set.seed(seed); do.call(caret::train,a) }),error=function(e) e)
    if(inherits(fit,"error")) next
    r<-sl_recursive_fold(fit,ctx,X,rownames(x)[tr],rownames(x)[te],known,exo)
    filled<-filled+r$filled
    lead<-hmap[[f]]$lead[match(te,hmap[[f]]$rowIndex)]
    out[[f]]<-data.frame(Resample=f,rowIndex=te,pred=r$pred,obs=y[te],lead=lead,steps_ahead=r$steps_ahead,stringsAsFactors=FALSE)
  }
  pred<-if(length(out)) do.call(rbind,out) else NULL
  list(pred=pred,folds=length(out),failed=length(folds)-length(out),filled=filled)
}

# folds for tuning the hyperparameters in the recursive evaluation: each fold tests only its
# first block after the gap (one-step predictions with observed predictors)
#' @export
sl_onestep_folds<-function(caret_folds){
  p<-attr(caret_folds,"params")
  hm<-p$horizon_map
  gap<-if(length(p$gap_blocks)&&!is.na(p$gap_blocks[1])) p$gap_blocks[1] else 0
  out<-lapply(names(caret_folds$index),function(f) hm[[f]]$rowIndex[hm[[f]]$lead==gap+1])
  names(out)<-names(caret_folds$index)
  keep<-lengths(out)>0
  list(index=caret_folds$index[keep],indexOut=out[keep])
}

# recursive evaluation of a trained caret model (stored in attr(m,"recursive")): the folds of
# the temporal scheme, the selected hyperparameters and the arguments of caret::train
#' @export
sl_run_recursive<-function(m,args_train,caret_folds,x,y,response,data_x,y_all,seed=NULL,progress=NULL){
  p<-attr(caret_folds,"params")
  t0<-Sys.time()
  ctx<-sl_recursive_setup(data_x,colnames(x),response,y_all)
  if(!isTRUE(ctx$ok)) return(list(pred=NULL,note=ctx$reason))
  r<-sl_recursive_eval(args_train,ctx,x,y,caret_folds,p$horizon_map,
                       known=if(identical(p$exo_mode,"known")) colnames(x) else if(is.null(p$known_vars)) character(0) else p$known_vars,
                       exo=if(is.null(p$exo_mode)||is.na(p$exo_mode)) "last" else p$exo_mode,
                       seed=seed,progress=progress,best_tune=m$bestTune)
  r$exo_mode<-p$exo_mode
  r$known<-p$known_vars
  r$time_col<-ctx$time_col
  r$features<-ctx$features
  r$run_time<-difftime(Sys.time(),t0,units="secs")
  r
}

# horizons (in blocks) free of leakage with the observed predictors: the predictors derived
# from the response must use values from at least (gap + h) blocks before t
#' @export
sl_valid_horizon<-function(meta,predictors,response,gap=0,steps_per_block=1){
  if(is.null(meta)||!nrow(meta)) return(Inf)
  m<-meta[meta$feature%in%predictors&meta$source%in%response&!meta$uses_future&!meta$includes_current,,drop=FALSE]
  if(!nrow(m)) return(Inf)
  used_lag<-min(ifelse(m$type=="lag",m$k,1))
  floor(used_lag/max(1,steps_per_block))-gap
}

# ---- Model setup checks ------------------------------------------------------------
# What a validation scheme depends on: the X Datalist, the training rows (in order),
# the response and the partition. Schemes store this at creation and are cleared when
# the current setup no longer matches (column filters and tuning do not matter).
#' @export
sl_setup_signature<-function(args){
  if(is.null(args)||is.null(args$x_train)) return(NULL)
  list(
    data_x=args$data_x,
    ids=rownames(args$x_train),
    y=if(!is.null(args$y_train)) as.vector(args$y_train[,1]) else NULL,
    partition=args$partition,
    partition_ref=if(identical(args$partition,"None")) NULL else args$partition_ref
  )
}

# Problems that would make training fail or leak the response, in plain words.
# x_data: X Datalist (already column-filtered); train_ids/test_ids: IDs from the Y
# Datalist; y: response for train_ids.
#' @export
sl_setup_issues<-function(x_data,train_ids,test_ids=NULL,y=NULL,var_y=NULL,x_name="X",y_name="Y"){
  ids<-c(train_ids,test_ids)
  x_ids<-rownames(x_data)
  if(!length(intersect(ids,x_ids))){
    return(paste0("X (",x_name,") and Y (",y_name,") have no observation IDs in common. Choose Datalists that share the same observation IDs."))
  }
  miss<-setdiff(ids,x_ids)
  if(length(miss)){
    return(paste0(
      length(miss)," of ",length(ids)," observations of Y (",y_name,") are not in X (",x_name,"): ",
      paste(head(miss,3),collapse=", "),if(length(miss)>3) " and others" else "",
      ". Choose Datalists with matching observation IDs."
    ))
  }
  issues<-character(0)
  x_train<-x_data[train_ids,,drop=FALSE]
  if(!is.null(y)&&is.numeric(y)){
    same<-vapply(x_train,function(col){
      is.numeric(col)&&isTRUE(all(col==y|(is.na(col)&is.na(y))))
    },logical(1))
    leak<-names(same)[same]
    if(length(leak)){
      issues<-c(issues,paste0(
        "The response '",var_y,"' is also among the predictors in X (",paste0("'",leak,"'",collapse=", "),
        "). Remove it with '+ select columns' or choose another X Datalist."
      ))
    }
  }
  n_na_x<-sum(is.na(x_train))
  if(n_na_x>0){
    issues<-c(issues,paste0("X has ",n_na_x," missing value(s) in the training observations. Use Pre-processing (e.g. Data imputation) or remove the affected observations or columns."))
  }
  if(!is.null(y)&&anyNA(y)){
    issues<-c(issues,paste0("Y has ",sum(is.na(y))," missing value(s) in the training observations."))
  }
  issues
}

#' @export
sl_setup_issues_ui<-function(issues){
  if(!length(issues)) return(NULL)
  div(class="alert_warning",style="padding: 6px 10px; margin: 4px 0px; font-size: 12px;",
      strong(icon("triangle-exclamation")," Check the model setup:"),
      tags$ul(style="margin: 2px 0px 0px 0px; padding-left: 18px;",lapply(issues,tags$li)))
}

# ---- Temporal predictions (Predict > Temporal) ---------------------------------------
# Predictions, observed values, time and coordinates are always matched by observation ID.

# pred: data.frame with one column and IDs as rownames; obs: named vector (IDs) or NULL;
# source: Datalist that holds the Temporal-Attribute (and coords) of the predicted IDs.
#' @export
sl_temporal_pred_data<-function(pred,obs,source,time_col){
  time_attr<-attr(source,"time")
  validate(need(!is.null(time_attr)&&time_col%in%colnames(time_attr),"The Datalist of the predictions has no Temporal-Attribute with the selected column."))
  ids<-rownames(pred)
  validate(need(all(ids%in%rownames(time_attr)),"The Temporal-Attribute does not contain all predicted observations."))
  tt<-time_attr[ids,time_col,drop=TRUE]
  if(!inherits(tt,c("Date","POSIXt"))&&!is.numeric(tt)){
    guess<-guess_time_settings(tt)
    if(guess$type%in%c("date","datetime")) tt<-convert_time_column(tt,guess$type,guess$format,guess$custom)
  }
  coords<-attr(source,"coords")
  has_coords<-!is.null(coords)&&ncol(as.data.frame(coords))>=2&&all(ids%in%rownames(coords))
  if(has_coords){
    xy<-as.data.frame(coords)[ids,1:2,drop=FALSE]
    loc<-paste0(round(xy[,1],6),"_",round(xy[,2],6))
  } else{
    xy<-data.frame(x=rep(NA_real_,length(ids)),y=rep(NA_real_,length(ids)))
    loc<-rep("All",length(ids))
  }
  p<-pred[,1]
  o<-if(is.null(obs)) rep(NA,length(ids)) else obs[ids]
  if(is.factor(p)){
    lev<-levels(p)
    o<-factor(as.character(o),levels=union(lev,unique(as.character(o[!is.na(o)]))))
  } else{
    o<-suppressWarnings(as.numeric(as.character(o)))
  }
  df<-data.frame(id=ids,time=tt,loc=loc,x=xy[,1],y=xy[,2],stringsAsFactors=FALSE)
  df$obs<-o
  df$pred<-p
  attr(df,"has_coords")<-has_coords
  attr(df,"is_class")<-is.factor(p)
  df[order(df$time,df$loc),,drop=FALSE]
}

# Observed vs predicted through time. mode="mean": mean (+/- SD) across locations at
# each time (regression) or accuracy per time (classification); mode="locations": the
# selected locations, one panel each.
#' @export
gg_temporal_series<-function(df,mode="mean",locs=NULL,col_obs="#1B1B1B",col_pred="#D95F02",linewidth=0.7,point_size=1.8,show_points=TRUE,show_ribbon=TRUE,theme="theme_bw",base_size=12,title="Observed and predicted through time",xlab="Time",ylab=NULL,legend.position="bottom",facet_ncol=2,x_angle=0){
  is_class<-isTRUE(attr(df,"is_class"))
  has_obs<-any(!is.na(df$obs))
  theme_fun<-switch(theme,theme_light=ggplot2::theme_light,theme_minimal=ggplot2::theme_minimal,theme_classic=ggplot2::theme_classic,theme_grey=ggplot2::theme_grey,ggplot2::theme_bw)
  cols<-c(Observed=col_obs,Predicted=col_pred)

  if(identical(mode,"mean")){
    if(is_class){
      validate(need(has_obs,"Observed values are needed to show the accuracy through time."))
      acc<-stats::aggregate(list(value=as.character(df$obs)==as.character(df$pred)),list(time=df$time),mean,na.rm=TRUE)
      p<-ggplot2::ggplot(acc,ggplot2::aes(x=time,y=value))+
        ggplot2::geom_line(color=col_pred,linewidth=linewidth)
      if(isTRUE(show_points)) p<-p+ggplot2::geom_point(color=col_pred,size=point_size)
      p<-p+ggplot2::scale_y_continuous(limits=c(0,1))+ggplot2::labs(y=if(is.null(ylab)||!nzchar(ylab)) "Accuracy (all locations)" else ylab)
    } else{
      long<-rbind(
        if(has_obs) data.frame(time=df$time,value=df$obs,series="Observed"),
        data.frame(time=df$time,value=df$pred,series="Predicted")
      )
      agg<-stats::aggregate(value~time+series,long,function(v) c(mean=mean(v,na.rm=TRUE),sd=if(sum(!is.na(v))>1) stats::sd(v,na.rm=TRUE) else NA_real_))
      agg<-do.call(data.frame,agg)
      names(agg)<-c("time","series","mean","sd")
      p<-ggplot2::ggplot(agg,ggplot2::aes(x=time,y=mean,color=series,fill=series,group=series))
      if(isTRUE(show_ribbon)) p<-p+ggplot2::geom_ribbon(ggplot2::aes(ymin=mean-sd,ymax=mean+sd),alpha=0.18,color=NA,na.rm=TRUE)
      p<-p+ggplot2::geom_line(linewidth=linewidth)
      if(isTRUE(show_points)) p<-p+ggplot2::geom_point(size=point_size)
      p<-p+ggplot2::scale_color_manual(values=cols,name=NULL)+ggplot2::scale_fill_manual(values=cols,name=NULL)+
        ggplot2::labs(y=if(is.null(ylab)||!nzchar(ylab)) paste0("Mean",if(isTRUE(show_ribbon)) " +/- SD"," across locations") else ylab)
    }
  } else{
    validate(need(length(locs)>0,"Select at least one location."))
    sub<-df[df$loc%in%locs,,drop=FALSE]
    validate(need(nrow(sub)>0,"No predictions for the selected locations."))
    long<-rbind(
      if(has_obs) data.frame(time=sub$time,loc=sub$loc,value=if(is_class) as.character(sub$obs) else sub$obs,series="Observed"),
      data.frame(time=sub$time,loc=sub$loc,value=if(is_class) as.character(sub$pred) else sub$pred,series="Predicted")
    )
    if(is_class) long$value<-factor(long$value,levels=union(levels(df$pred),unique(long$value)))
    p<-ggplot2::ggplot(long,ggplot2::aes(x=time,y=value,color=series,group=series))
    if(!is_class) p<-p+ggplot2::geom_line(linewidth=linewidth)
    if(isTRUE(show_points)||is_class) p<-p+ggplot2::geom_point(size=point_size,position=if(is_class) ggplot2::position_dodge(width=0) else "identity")
    p<-p+ggplot2::scale_color_manual(values=cols,name=NULL)+
      ggplot2::facet_wrap(~loc,ncol=facet_ncol,scales=if(is_class) "fixed" else "free_y")+
      ggplot2::labs(y=if(is.null(ylab)||!nzchar(ylab)) (if(is_class) "Class" else "Value") else ylab)
  }
  p<-p+ggplot2::labs(x=xlab,title=title)+theme_fun(base_size=base_size)+ggplot2::theme(legend.position=legend.position)
  if(!is.na(x_angle)&&x_angle>0) p<-p+ggplot2::theme(axis.text.x=ggplot2::element_text(angle=x_angle,hjust=1))
  p
}

# One value per location for the map. time_value="__all__" aggregates all times.
# what: "pred", "obs" or "error". Regression errors: "abs" (|pred-obs|), "diff" (pred-obs),
# "rmse", "mae", "bias"; classification: "accuracy" (share of correct predictions).
#' @export
sl_temporal_map_data<-function(df,source,time_value="__all__",what="pred",error_metric="rmse"){
  validate(need(isTRUE(attr(df,"has_coords")),"The Datalist of the predictions has no Coords-Attribute for all predicted observations."))
  is_class<-isTRUE(attr(df,"is_class"))
  if(!identical(time_value,"__all__")){
    df<-df[as.character(df$time)==time_value,,drop=FALSE]
    validate(need(nrow(df)>0,"No predictions at the selected time."))
  }
  if(what%in%c("obs","error")) validate(need(any(!is.na(df$obs)),"Observed values are not available for these predictions."))
  mode_of<-function(v){v<-v[!is.na(v)]; if(!length(v)) return(NA_character_); names(sort(table(as.character(v)),decreasing=TRUE))[1]}
  groups<-split(seq_len(nrow(df)),df$loc)
  vals_loc<-lapply(groups,function(i){
    o<-df$obs[i]; p<-df$pred[i]
    if(what=="pred") return(if(is_class) mode_of(p) else mean(p,na.rm=TRUE))
    if(what=="obs") return(if(is_class) mode_of(o) else mean(o,na.rm=TRUE))
    ok<-!is.na(o)
    if(!any(ok)) return(NA)
    if(is_class) return(mean(as.character(o[ok])==as.character(p[ok])))
    e<-p[ok]-o[ok]
    switch(error_metric,abs=mean(abs(e)),diff=mean(e),bias=mean(e),mae=mean(abs(e)),sqrt(mean(e^2)))
  })
  first<-vapply(groups,`[`,integer(1),1)
  z<-unlist(vals_loc)
  if(is_class&&what%in%c("pred","obs")) z<-factor(z,levels=intersect(levels(df$pred),unique(z)))
  out<-data.frame(z=z,row.names=df$id[first])
  coords<-data.frame(x=df$x[first],y=df$y[first],row.names=df$id[first])
  out<-out[!is.na(out$z),,drop=FALSE]
  attr(out,"coords")<-coords[rownames(out),,drop=FALSE]
  attr(out,"base_shape")<-attr(source,"base_shape")
  attr(out,"layer_shape")<-attr(source,"layer_shape")
  validate(need(nrow(out)>0,"No values to map."))
  out
}

# Upper limits of n equal-width classes, rounded (breaks_label() adds the minimum)
#' @export
temporal_map_breaks<-function(z,n=5){
  r<-range(z,na.rm=TRUE)
  if(!is.finite(diff(r))||diff(r)==0) return(r[2])
  dp<-max(0,2-floor(log10(diff(r))))
  b<-round(seq(r[1],r[2],length.out=max(2,n)+1),dp)
  b[length(b)]<-ceiling_decimal(r[2],dp)
  unique(b[-1])
}

# Map with the same pipeline as Spatial Tools: gg_rst + titles + axes + scale bar + north
#' @export
gg_temporal_map<-function(mapdata,newcolhabs,pal="turbo",reverse_palette=FALSE,nbreaks=5,min_radius=1,max_radius=3,scale_radius=FALSE,
                          main="",leg_title=NULL,legend.position="right",axis_style="bw_blocks",axis_width=0.1,
                          xlab="Longitude",ylab="Latitude",axis.text_size=11,axis.title_size=11,
                          base_shape=TRUE,base_color="gray95",layer_shape=TRUE,layer_color="gray80",shape_border="gray40",
                          bar_position="bottomright",bins_km=100,n_bins=2,n_location="tl"){
  shape_args<-function(on,color) list(shape=isTRUE(on),color=color,weight=1,border_col=shape_border,fillOpacity=1,stroke=TRUE,fill=TRUE)
  base_args<-shape_args(base_shape&&!is.null(attr(mapdata,"base_shape")),base_color)
  layer_args<-shape_args(layer_shape&&!is.null(attr(mapdata,"layer_shape")),layer_color)
  z<-mapdata[,1]
  breaks<-if(is.factor(z)) NULL else temporal_map_breaks(z,nbreaks)
  p<-gg_rst(data=mapdata,newcolhabs=newcolhabs,pal=pal,reverse_palette=reverse_palette,custom_breaks=breaks,factor=is.factor(z),
            min_radius=min_radius,max_radius=max_radius,scale_radius=scale_radius,addCircles=TRUE,addMinicharts=FALSE,
            base_shape_args=base_args,layer_shape_args=layer_args,args_extra_shape=NULL,show_coords="None",
            legend.position=legend.position,leg_title=leg_title)
  p<-gg_add_titles(p,main=main)
  p<-gg_style_axes(p,axis_style=axis_style,xlab=xlab,ylab=ylab,axis.text_size=axis.text_size,axis.title_size=axis.title_size,
                   axis_width=axis_width,data=mapdata,base_shape_args=base_args,layer_shape_args=layer_args,args_extra_shape=NULL)
  p<-add_bar_scale(p,data=mapdata,position=bar_position,unit="km",position_label="above",n_bins=n_bins,bins_km=bins_km,
                   bar_height=0.2,pad_x=0.05,pad_y=0.025,size_scalebar_text=3)
  gg_add_north(p,n_location=n_location,n_which_north="grid",n_width=40,n_height=40,n_pad_x=0.1,n_pad_y=0.15,n_cex.text=10)
}

# ---- Temporal prediction diagnostics (Predict > Temporal) ---------------------------
# All take the data.frame from sl_temporal_pred_data() and return a ggplot with the
# summarised values in attr(p,"table") (used by the table download).

temporal_theme<-function(theme){
  switch(theme,theme_light=ggplot2::theme_light,theme_minimal=ggplot2::theme_minimal,theme_classic=ggplot2::theme_classic,theme_grey=ggplot2::theme_grey,ggplot2::theme_bw)
}

temporal_need_obs<-function(df){
  validate(need(any(!is.na(df$obs)),"Observed values are needed for this plot (use Partition, Training, or a New Data with the observed variable)."))
}

# residual (pred - obs) for regression; correct/incorrect for classification
temporal_residuals<-function(df){
  df<-df[!is.na(df$obs)&!is.na(df$pred),,drop=FALSE]
  if(isTRUE(attr(df,"is_class"))){
    df$correct<-as.character(df$obs)==as.character(df$pred)
  } else{
    df$resid<-df$pred-df$obs
  }
  df
}

# Hovmoller: locations (y) x time (x), filled by the residual
#' @export
gg_temporal_hovmoller<-function(df,order_by="y",fill_type="diff",low="#2166AC",mid="white",high="#B2182B",
                                base_size=12,title="Residuals by location and time",xlab="Time",ylab="Location",
                                show_loc_labels=FALSE,leg_title=NULL,x_angle=0){
  temporal_need_obs(df)
  is_class<-isTRUE(attr(df,"is_class"))
  r<-temporal_residuals(df)
  key<-switch(order_by,
              x=tapply(r$x,r$loc,mean),
              error=if(is_class) tapply(!r$correct,r$loc,mean) else tapply(abs(r$resid),r$loc,mean),
              tapply(r$y,r$loc,mean))
  r$loc<-factor(r$loc,levels=names(sort(key)))
  if(is_class){
    r$value<-factor(ifelse(r$correct,"Correct","Incorrect"),levels=c("Correct","Incorrect"))
    p<-ggplot2::ggplot(r,ggplot2::aes(x=time,y=loc,fill=value))+ggplot2::geom_tile()+
      ggplot2::scale_fill_manual(values=c(Correct=low,Incorrect=high),name=if(is.null(leg_title)||!nzchar(leg_title)) NULL else leg_title)
  } else{
    r$value<-if(identical(fill_type,"abs")) abs(r$resid) else r$resid
    p<-ggplot2::ggplot(r,ggplot2::aes(x=time,y=loc,fill=value))+ggplot2::geom_tile()
    lt<-if(is.null(leg_title)||!nzchar(leg_title)) (if(identical(fill_type,"abs")) "|Predicted - Observed|" else "Predicted - Observed") else leg_title
    if(identical(fill_type,"abs")){
      p<-p+ggplot2::scale_fill_gradient(low=mid,high=high,name=lt)
    } else{
      lim<-max(abs(r$value),na.rm=TRUE)
      p<-p+ggplot2::scale_fill_gradient2(low=low,mid=mid,high=high,midpoint=0,limits=c(-lim,lim),name=lt)
    }
  }
  ylab2<-if(order_by%in%c("x","y","error")) paste0(ylab," (ordered by ",switch(order_by,x="x coordinate",y="y coordinate",error="mean error"),")") else ylab
  p<-p+ggplot2::labs(x=xlab,y=ylab2,title=title)+ggplot2::theme_bw(base_size=base_size)+
    ggplot2::theme(panel.grid=ggplot2::element_blank())
  if(!isTRUE(show_loc_labels)) p<-p+ggplot2::theme(axis.text.y=ggplot2::element_blank(),axis.ticks.y=ggplot2::element_blank())
  if(!is.na(x_angle)&&x_angle>0) p<-p+ggplot2::theme(axis.text.x=ggplot2::element_text(angle=x_angle,hjust=1))
  attr(p,"table")<-data.frame(id=r$id,time=r$time,loc=as.character(r$loc),value=r$value)
  p
}

# Error metrics at each time step (across locations)
#' @export
gg_temporal_error<-function(df,metrics=c("rmse","bias"),show_points=TRUE,linewidth=0.8,point_size=2,
                            theme="theme_bw",base_size=12,title="Error through time",xlab="Time",ylab=NULL,
                            legend.position="bottom",x_angle=0){
  temporal_need_obs(df)
  is_class<-isTRUE(attr(df,"is_class"))
  r<-temporal_residuals(df)
  times<-sort(unique(r$time))
  if(is_class){
    tab<-do.call(rbind,lapply(times,function(t){
      s<-r[r$time==t,,drop=FALSE]
      out<-data.frame(time=t,metric="Accuracy",value=mean(s$correct))
      per_class<-lapply(levels(df$pred),function(cl){
        k<-as.character(s$obs)==cl
        if(!any(k)) return(NULL)
        data.frame(time=t,metric=paste0("Recall: ",cl),value=mean(s$correct[k]))
      })
      rbind(out,do.call(rbind,per_class))
    }))
    validate(need(length(metrics)>0,"Select at least one metric."))
    keep<-if("per_class"%in%metrics) unique(tab$metric) else "Accuracy"
    tab<-tab[tab$metric%in%keep,,drop=FALSE]
    ylab<-if(is.null(ylab)||!nzchar(ylab)) "Proportion correct" else ylab
  } else{
    validate(need(length(metrics)>0,"Select at least one metric."))
    labels<-c(rmse="RMSE",mae="MAE",bias="Bias (pred - obs)")
    tab<-do.call(rbind,lapply(times,function(t){
      e<-r$resid[r$time==t]
      data.frame(time=t,metric=labels[metrics],value=c(rmse=sqrt(mean(e^2)),mae=mean(abs(e)),bias=mean(e))[metrics])
    }))
    tab$metric<-factor(tab$metric,levels=labels[metrics])
    ylab<-if(is.null(ylab)||!nzchar(ylab)) "Error (across locations)" else ylab
  }
  p<-ggplot2::ggplot(tab,ggplot2::aes(x=time,y=value,color=metric,group=metric))
  if(!is_class&&"bias"%in%metrics) p<-p+ggplot2::geom_hline(yintercept=0,linetype=2,color="gray50")
  p<-p+ggplot2::geom_line(linewidth=linewidth)
  if(isTRUE(show_points)) p<-p+ggplot2::geom_point(size=point_size)
  p<-p+ggplot2::scale_color_brewer(palette="Dark2",name=NULL)+
    ggplot2::labs(x=xlab,y=ylab,title=title)+temporal_theme(theme)(base_size=base_size)+
    ggplot2::theme(legend.position=legend.position)
  if(!is.na(x_angle)&&x_angle>0) p<-p+ggplot2::theme(axis.text.x=ggplot2::element_text(angle=x_angle,hjust=1))
  rownames(tab)<-NULL
  attr(p,"table")<-tab
  p
}

# Season / month / year of each time value (seasons by hemisphere)
temporal_group<-function(time,group_by="season_south"){
  if(identical(group_by,"none")) return(rep("All",length(time)))
  if(!inherits(time,c("Date","POSIXt"))) return(rep("All",length(time)))
  mm<-as.integer(format(time,"%m"))
  if(identical(group_by,"month")) return(factor(month.abb[mm],levels=month.abb))
  if(identical(group_by,"year")) return(factor(format(time,"%Y")))
  south<-c("Summer","Summer","Autumn","Autumn","Autumn","Winter","Winter","Winter","Spring","Spring","Spring","Summer")
  north<-c("Winter","Winter","Spring","Spring","Spring","Summer","Summer","Summer","Autumn","Autumn","Autumn","Winter")
  s<-if(identical(group_by,"season_north")) north[mm] else south[mm]
  factor(s,levels=c("Summer","Autumn","Winter","Spring"))
}

# Observed vs predicted coloured by season / month / year (regression);
# accuracy by group (classification)
#' @export
gg_temporal_obs_pred<-function(df,group_by="season_south",colors=NULL,show_1to1=TRUE,show_fit=TRUE,point_size=1.8,alpha=0.6,
                               theme="theme_bw",base_size=12,title="Observed vs predicted",xlab="Observed",ylab="Predicted",
                               legend.position="right",facet=FALSE){
  temporal_need_obs(df)
  is_class<-isTRUE(attr(df,"is_class"))
  r<-temporal_residuals(df)
  if(!inherits(r$time,c("Date","POSIXt"))&&!identical(group_by,"none")){
    validate(need(FALSE,"Season, month and year need a Date or Date-time temporal column. Use 'None'."))
  }
  r$group<-temporal_group(r$time,group_by)
  n_groups<-nlevels(factor(r$group))
  pal<-if(is.null(colors)) NULL else colors(n_groups)
  if(is_class){
    tab<-stats::aggregate(list(Accuracy=r$correct),list(group=factor(r$group)),mean)
    tab$n<-as.vector(table(factor(r$group)))
    p<-ggplot2::ggplot(tab,ggplot2::aes(x=group,y=Accuracy,fill=group))+ggplot2::geom_col(width=0.7,show.legend=FALSE)+
      ggplot2::geom_text(ggplot2::aes(label=paste0("n=",n)),vjust=-0.4,size=3.2)+
      ggplot2::scale_y_continuous(limits=c(0,1.05))+
      ggplot2::labs(x=NULL,y="Accuracy",title=if(identical(title,"Observed vs predicted")) "Accuracy by period" else title)
    if(!is.null(pal)) p<-p+ggplot2::scale_fill_manual(values=pal)
    p<-p+temporal_theme(theme)(base_size=base_size)
    attr(p,"table")<-tab
    return(p)
  }
  lim<-range(c(r$obs,r$pred),na.rm=TRUE)
  p<-ggplot2::ggplot(r,ggplot2::aes(x=obs,y=pred,color=group))
  if(isTRUE(show_1to1)) p<-p+ggplot2::geom_abline(slope=1,intercept=0,linetype=2,color="gray40")
  p<-p+ggplot2::geom_point(size=point_size,alpha=alpha)
  if(isTRUE(show_fit)) p<-p+ggplot2::geom_smooth(method="lm",formula=y~x,se=FALSE,linewidth=0.8)
  if(!is.null(pal)) p<-p+ggplot2::scale_color_manual(values=pal,name=NULL)
  if(isTRUE(facet)&&n_groups>1) p<-p+ggplot2::facet_wrap(~group)
  p<-p+ggplot2::coord_equal(xlim=lim,ylim=lim)+ggplot2::labs(x=xlab,y=ylab,title=title,color=NULL)+
    temporal_theme(theme)(base_size=base_size)+ggplot2::theme(legend.position=if(n_groups>1) legend.position else "none")
  stats_g<-do.call(rbind,lapply(split(r,r$group),function(s){
    if(!nrow(s)) return(NULL)
    data.frame(group=as.character(s$group[1]),n=nrow(s),RMSE=sqrt(mean(s$resid^2)),Bias=mean(s$resid),
               R2=if(nrow(s)>2) suppressWarnings(stats::cor(s$obs,s$pred)^2) else NA_real_)
  }))
  rownames(stats_g)<-NULL
  attr(p,"table")<-stats_g
  p
}

# Autocorrelation of the residuals through time
#' @export
gg_temporal_resid_acf<-function(df,mode="mean",max_lag=12,color="#05668D",base_size=12,
                                title="Autocorrelation of the residuals",theme="theme_bw"){
  temporal_need_obs(df)
  is_class<-isTRUE(attr(df,"is_class"))
  r<-temporal_residuals(df)
  r$e<-if(is_class) as.numeric(!r$correct) else r$resid
  times<-sort(unique(r$time))
  validate(need(length(times)>=4,"At least four time steps are needed for the autocorrelation."))
  max_lag<-max(1,min(as.integer(max_lag),length(times)-1))
  ylab<-if(is_class) "ACF of the error rate" else "ACF of the residuals"
  if(identical(mode,"mean")){
    series<-tapply(r$e,factor(r$time,levels=times),mean,na.rm=TRUE)
    ac<-stats::acf(as.numeric(series),lag.max=max_lag,plot=FALSE,na.action=stats::na.pass)
    tab<-data.frame(lag=as.integer(ac$lag[,1,1]),acf=as.numeric(ac$acf[,1,1]))
    ci<-1.96/sqrt(length(series))
    p<-ggplot2::ggplot(tab,ggplot2::aes(x=lag,y=acf))+
      ggplot2::geom_hline(yintercept=0,color="gray40")+
      ggplot2::geom_hline(yintercept=c(-ci,ci),linetype=2,color="#B2182B")+
      ggplot2::geom_segment(ggplot2::aes(xend=lag,y=0,yend=acf),color=color,linewidth=0.9)+
      ggplot2::geom_point(color=color,size=2)+
      ggplot2::labs(x="Lag (time steps)",y=paste0(ylab," (mean across locations)"),title=title,
                    caption="Dashed lines: approximate 95% limits (+/- 1.96/sqrt(n))")
  } else{
    tab<-do.call(rbind,lapply(split(r,r$loc),function(s){
      s<-s[order(s$time),,drop=FALSE]
      if(nrow(s)<4||stats::sd(s$e)==0) return(NULL)
      ac<-stats::acf(s$e,lag.max=min(max_lag,nrow(s)-1),plot=FALSE,na.action=stats::na.pass)
      data.frame(loc=s$loc[1],lag=as.integer(ac$lag[,1,1]),acf=as.numeric(ac$acf[,1,1]))
    }))
    validate(need(!is.null(tab)&&nrow(tab)>0,"Not enough variation in the residuals of each location."))
    tab<-tab[tab$lag>0,,drop=FALSE]
    p<-ggplot2::ggplot(tab,ggplot2::aes(x=factor(lag),y=acf))+
      ggplot2::geom_hline(yintercept=0,color="gray40")+
      ggplot2::geom_boxplot(fill=grDevices::adjustcolor(color,0.35),color=color,outlier.size=0.8)+
      ggplot2::labs(x="Lag (time steps)",y=paste0(ylab," (one value per location)"),title=title)
  }
  p<-p+temporal_theme(theme)(base_size=base_size)
  rownames(tab)<-NULL
  attr(p,"table")<-tab
  p
}
