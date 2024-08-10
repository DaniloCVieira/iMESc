SL_models<-readRDS("www/SL_models.rds")
sl_tips<-readRDS("www/sl_tips.rds")
#sl_tips<-sl_tips2
sl_formals<-readRDS("www/sl_formals.rds")
sl_ctl_formals<-readRDS("www/sl_ctl_formals.rds")

model_fits=SL_models$model_fits
model_grid=SL_models$model_grid
model_labels=SL_models$model_labels
model_labels2=SL_models$model_labels2
model_labels3=SL_models$model_labels3
model_library=SL_models$model_library
model_parameters=SL_models$model_parameters
model_tags=SL_models$model_tags
model_varImp=SL_models$model_varImp
models=SL_models$models
newmodels=SL_models$newmodels
train_functions=SL_models$train_functions
train_functions2=SL_models$train_functions2
available_models<-SL_models$models
is_installed <- function(package) {
  # Check if the package is installed
  installed_packages <- rownames(installed.packages())
  return(package %in% installed_packages)
}
install_packages <- function(package) {
  install.packages(package, dependencies = TRUE)
}
make_unique_names <- function(string_vector) {
  unique_names <- character(length(string_vector))
  counts <- table(string_vector)
  index_map <- integer(length(string_vector))

  for (i in seq_along(string_vector)) {
    if (counts[string_vector[i]] > 1) {
      index <- which(string_vector == string_vector[i])
      index_map[index] <- 1:length(index)
      unique_names[i] <- paste(string_vector[i], ".", index_map[i], sep = "")
    } else {
      if (string_vector[i] %in% unique_names) {
        index <- which(unique_names == string_vector[i])
        index_map[i] <- 2
        unique_names[i] <- paste(string_vector[i], ".", index_map[i], sep = "")
      } else {
        unique_names[i] <- paste(string_vector[i], ".1", sep = "")
      }
    }
  }

  return(unique_names)
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
box_caret<-function(id,content=NULL,inline=T, class="train_box",click=T,title=NULL,button_title=NULL,tip=NULL,auto_overflow=F, color=NULL,show_tittle=T,hide_content=F){
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
  if(isTRUE(show_tittle)){
    div_title<-div(class="box_title",
                   actionButton(ns("show_hide"),hide_content,style=style),
                   title,tip)
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
      toggle("content")
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
  st<-paste(paste0('.tip_box_caret',id), '.tippy-tooltip.material-theme {background:', lighten(color0,0.95),";box-shadow: 0 0px 2px",color0,";}")
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
                             bsTooltip(ns(paste0(link,"_help")),"Click for details", "right"),
                             content
                           )
    )
  )
}

box2_help_content<-{div(
  p("This panel allows users to specify comma-delimited values for tuning parameters."),
  p("If the user selects a different option other than 'Grid' or 'Random' in the box_caret1 panel, they can define the tuning parameters directly within this pink panel."),
  p("IMESc will then use expand.grid to combine the specified values for tuning.")
)}

box4_help_content<-{div(
  p("")
)}
box5_help_content<-{
  div(
    p(""),
    p("It serves as a quick reference for the user to review and confirm their chosen settings before running the analysis.")
  )
}
box7_help_content<-{
  div("Print output of the model (class=,",code("train"),")")
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


  p<-ggpairs(df,
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


filter_avNNet<-function(m,vars){


  dfs<-dfs_avNNet(m=m)
  dfneu<-dfs$dfneu
  df_segments<-dfs$df_segments
  var_names<-attr(dfneu,"var_names")
  vars_remove<-var_names[!var_names%in%vars]
  bias<-subset(dfneu,neu_type=="Bias")
  dfneu<-dfneu[-which(dfneu$neuron%in%  which(var_names%in%vars_remove)),]
  df_segments<-dfs$df_segments
  df_segments<-df_segments[df_segments$neuron%in%dfneu$neuron,]
  df_segments<-df_segments[df_segments$conn%in%dfneu$neuron,]

  attr(dfneu,"var_names")<-vars

  list(dfneu=dfneu,
       df_segments=df_segments
  )
}


predict_cforest_tree<-function(tree,newdata){
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
        nobs=length(nrow(m$training))
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
            df<-cbind(data.frame(modelid=paste0(i,j),Datalist=data_x,Model=cmodel,Model_name=model_name,Model_type=m$modelType,nobs=length(pred),rbind(  postResample(pred,obs))
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


table_allmodels<-function(saved_data,available_models){
  data_x<-names(saved_data)[1]
  res<-lapply(names(saved_data),function(data_x){
    table_training_datalist(saved_data,data_x)

  })
  names(res)<-names(saved_data)
  res
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
