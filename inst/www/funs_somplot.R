pickerInput_fromtop<-function(inputId, label = NULL, choices, selected = NULL, multiple = FALSE,options = list(), choicesOpt = NULL, width = NULL, inline = FALSE,stateInput = TRUE, autocomplete = FALSE){
  win_pad<-shinyWidgets::pickerOptions(windowPadding="top",size=10)
  options[names(win_pad)]<-win_pad



  shinyWidgets::pickerInput(inputId, label = label, choices, selected = selected, multiple = multiple,options = options, choicesOpt = choicesOpt, width = width, inline = inline)
}

pickerInput_fromtop_live<-function(inputId, label = NULL, choices, selected = NULL, multiple = FALSE,options = list(), choicesOpt = NULL, width = NULL, inline = FALSE,stateInput = TRUE, autocomplete = FALSE){
  win_pad<-shinyWidgets::pickerOptions(windowPadding="top",size=10,liveSearch =T,container="body")
  options[names(win_pad)]<-win_pad



  shinyWidgets::pickerInput(inputId, label = label, choices, selected = selected, multiple = multiple,options = options, choicesOpt = choicesOpt, width = width, inline = inline)
}


#' @export
supersom_summaries<-function(m){
  traindata <- m$data

  summ <- m$grid[-1]
  summ$neighbourhood.fct <- as.character(summ$neighbourhood.fct)
  summ <- do.call(rbind, summ)
  som_qual<-errors_som(m)




  #  t(rbind(mean=mean,t(som_qual[[1]])))

  df2<-data.frame(n.obs = sapply(traindata, nrow),
                  n.variables = sapply(traindata, ncol),
                  user.weights = m$user.weights,
                  dist.fcts = m$dist.fcts
  )
  df2<-data.frame(t(df2))

  mean=apply(apply(t(som_qual[[1]]),1,function(x) as.numeric(x)),1,mean)

  df3<-data.frame(mean)
  rownames(df3)<-rownames(som_qual[[1]])
  df4<-data.frame(som_qual[[1]])
  rownames(df3)<-paste0("mean_",rownames(df4))
  neu.uti<-som_qual[[2]]

  df2<-rbind(df2,df4)

  n.units=nrow(m$grid$pts)
  xdim=m$grid$xdim
  ydim=m$grid$ydim
  topo=m$grid$topo
  toroidal=m$grid$toroidal
  neighbourhood.fct=m$grid$neighbourhood.fct

  mode =attr(m,"mode")
  alpha0 = m$alpha[1]
  alpha1 =m$alpha[2]
  radius0 =m$radius[1]
  radius1 =m$radius[2]
  maxNA.fraction = m$maxNA.fraction
  normalizeDataLayers=attr(m,'normalizeDataLayers')

  df5<-rbind(neu.uti,n.units,xdim,ydim,topo,toroidal,neighbourhood.fct,mode,alpha0,alpha1,radius0,radius1,maxNA.fraction,normalizeDataLayers)
  colnames(df3)<-colnames(df5)<-"Value"



  df1<-data.frame(value=rbind(df3,df5))


  list(df2,df1)
}


#' @export
supersom_df2_df6<-function(m){


  traindata <- m$data

  summ <- m$grid[-1]
  summ$neighbourhood.fct <- as.character(summ$neighbourhood.fct)
  summ <- do.call(rbind, summ)
  som_qual<-errors_som(m)
  #  t(rbind(mean=mean,t(som_qual[[1]])))

  df2<-data.frame(

    n.obs = as.vector(sapply(traindata, nrow)),
    n.variables = as.vector(sapply(traindata, ncol)),
    user.weights = m$user.weights,
    dist.fcts = m$dist.fcts
  )
  df2<-data.frame(t(df2))

  mean=apply(apply(t(som_qual[[1]]),1,function(x) as.numeric(x)),1,mean)

  df3<-data.frame(mean)
  rownames(df3)<-rownames(som_qual[[1]])
  df4<-data.frame(som_qual[[1]])
  rownames(df3)<-paste0("mean_",rownames(df4))
  neu.uti<-som_qual[[2]]

  colnames(df2)<-colnames(df4)<-names(traindata)
  df2<-rbind(df2,df4)

  n.units=nrow(m$grid$pts)
  xdim=m$grid$xdim
  ydim=m$grid$ydim
  topo=m$grid$topo
  toroidal=m$grid$toroidal
  neighbourhood.fct=m$grid$neighbourhood.fct

  mode =attr(m,"mode")
  alpha0 = m$alpha[1]
  alpha1 =m$alpha[2]
  radius0 =m$radius[1]
  radius1 =m$radius[2]
  maxNA.fraction = m$maxNA.fraction
  normalizeDataLayers=attr(m,'normalizeDataLayers')


  c(neu.uti,n.units,xdim,ydim,topo,toroidal,neighbourhood.fct,mode,alpha0,alpha1,radius0,radius1,maxNA.fraction,normalizeDataLayers) |>  length()
  df5<-rbind(neu.uti,n.units,xdim,ydim,topo,toroidal,neighbourhood.fct,mode,alpha0,alpha1,radius0,radius1,maxNA.fraction,normalizeDataLayers)
  df6<-matrix(NA, nrow(df5),ncol(df2))
  for(i in 1:nrow(df6)){
    df6[i,1]<- df5[i,1]
  }

  colnames(df3)<-colnames(df5)<-"Params"
  df2<-data.frame(matrix(unlist(df2), ncol=ncol(df2), dimnames = list(rownames(df2), colnames(df2))))

  df6 <- data.frame(matrix(c(rownames(df5),unlist(df5)),ncol=ncol(df5)+1))
  colnames(df6)<-c("Params","Values")
  return(list(df2=df2,df6=df6))
}
get_som_summtab <- function(df2, df6, model_name = "SOM1") {

  # Create HTML table structure
  combined_table <- HTML(paste0('<table border="1" style="border-collapse: collapse;" class="dataframe">
               <thead>
                 <tr style="text-align: center;">
                   <th colspan="', ncol(df2) + 1, '">', model_name, '</th>
                 </tr>
                 <tr>
                   <th style="font-weight: normal;">', 'Layers', '</th>',
               '<th style="font-weight: normal;">', paste(colnames(df2), collapse = '</th><th style="font-weight: normal;">'), '</th>',
               '</tr>
               </thead>
               <tbody>'))

  # Render df2
  for (i in 1:nrow(df2)) {
    row_html <- paste('<td>', df2[i, ], '</td>', collapse = '')
    combined_table <- paste0(combined_table, '<tr>',
                             '<td>', rownames(df2)[i], '</td>',
                             row_html,
                             '</tr>')
  }

  # Merge cells for df6 title (bold)
  combined_table <- paste0(combined_table, '<tr>',
                           '<td colspan="', ncol(df2) + 1, '"><b>Parameters</b></td>',
                           '</tr>')

  # Render df6
  for (i in 1:nrow(df6)) {
    combined_table <- paste0(combined_table, '<tr>',
                             '<td>', df6$Params[i], '</td>',
                             '<td colspan="', ncol(df2), '">', df6$Values[i], '</td>',
                             '</tr>')
  }

  # Close HTML table
  combined_table <- paste0(combined_table, '</tbody></table>')

  HTML(combined_table)
}

supersom_summaries2<-function(m,name='SOM1'){
  dfs<-supersom_df2_df6(m)
  df2<-dfs$df2


  df6<-dfs$df6
  get_som_summtab(df2,df6,name)
}

supersom_summaries3<-function(m,name){

  dfs<-supersom_df2_df6(m)
  df2<-dfs$df2
  df2<-rbind(colnames(df2),df2)
  rownames(df2)[1]<-"Layers"
  df6<-dfs$df6
  df7<-df6[,-1, drop=F]
  rownames(df7)<-df6[,1]
  df7<-rbind(data.frame(Values="Params"),df7)
  rownames(df7)[1]<-""
  colnames(df7)<-colnames(df2)[1]
  notin<-colnames(df2)[!colnames(df2)%in%colnames(df7)]
  for(i in notin){
    df7[,i]<-df7[,1]
  }



  result<-rbind(rep(name,ncol(df2)),df2,df7)
  colnames(result)<-NULL
  rownames(result)[1]<-"Model name"
  result
}




#' @export

#' @export
gethex<-function(m){
  grid<-data.frame(m$grid$pts)
  pxpy<-getpxpy(m)
  px<-pxpy[[1]][1]
  py<-pxpy[[1]][2]
  py2<-2*py
  rang<-pxpy[[2]]
  rang2<-pxpy[[3]]
  hexs<-lapply(1:nrow(grid), function(i){
    a=grid[i,1]
    b=a-px
    c=a+px
    x=c(a,b,b,a,c,c,a)
    a=grid[i,2]
    b=a-(py2)
    c=a-py
    d=a+py
    e=a+(py2)
    y=c(b,c,d,e,d,c,b)
    res<-data.frame(x,y, neu=i)
    res
  })
  attr(hexs,'pxpy')<-pxpy
  hexs
}

#' @export
hc_hex<-function(hexs){
  hc<-do.call(c,lapply(hexs,function(x) x$group[1]))
  names(hc)<-1:length(hc)
  hc
}
#' @export

#' @export
getbp_som<-function(m,indicate,npic,hc){
  if(is.null(indicate)){return(NULL)}
  inds<-indsom(m,indicate=indicate,npic=ncol(do.call(cbind,m$codes)))
  CORMAP<-inds[[1]]
  result<-inds[[2]]
  bp=data.frame(na.omit(as.matrix(t(CORMAP))))
  bp[,1]<-  scales::rescale(bp[,1], c(min(m$grid$pts[,1]), max(m$grid$pts[,1])))
  bp[,2]<-  scales::rescale(bp[,2], c(min(m$grid$pts[,2]), max(m$grid$pts[,2])))
  bp<-bp[ inds[[3]],]
  if(indicate%in%c('cor_bmu','cor_hc')){
    picvar<-getvars_bycluster(bp, m, result, hc)[1:npic]
  } else{
    picvar<-inds[[3]][1:npic]
  }
  indicadores<-picvar
  colnames(bp)<-c('x','y')
  bp$id<-rownames(bp)

  bp<-bp[indicadores,]
  bp$importance<-inds[[2]][indicadores,]
  bp
}
#' @export
getpxpy2<-function(m){
  px<-diff(unique(m$grid$pts[,1])[1:2])/2
  py<-diff(unique(m$grid$pts[,2])[1:2])/2
  nx=m$grid$xdim
  ny=m$grid$ydim
  rang<-range(m$grid$pts[,1])
  rang<-c(rang[1]-px,rang[2]+px)
  rang2<-range(m$grid$pts[,2])
  py2<-1*py
  rang2<-c(rang2[1]-(py2),rang2[2]+(py2))
  list(c(px,py),rang,rang2,c(nx,ny))
}
#' @export
gethex2<-function(m){
  grid<-data.frame(m$grid$pts)
  pxpy<-getpxpy2(m)
  px<-pxpy[[1]][1]
  py<-pxpy[[1]][2]
  py2<-py
  rang<-pxpy[[2]]
  rang2<-pxpy[[3]]
  hexs<-lapply(1:nrow(grid), function(i){
    a=grid[i,1]
    b=a-px
    c=a+px
    x=c(a,b,b,a,c,c,a)
    a=grid[i,2]
    b=a-(py2)
    c=a-py
    d=a+py
    e=a+(py2)
    y=c(b,c,d,e,d,c,b)
    res<-data.frame(x,y, neu=i)
    res
  })
  attr(hexs,'pxpy')<-pxpy
  hexs
}
#' @export
get_neurons<-function(m,background_type=NULL,property=NULL,hc=NULL){
  hexs<-if(m$grid$topo=="hexagonal"){gethex(m)} else{gethex2(m) }

  if(is.null(background_type)){
    hc<-rep("None", length(hexs))
    leg_name<-NULL
    backtype<-"None"
  } else {
    if(background_type=="uMatrix"){
      leg_name<-"Distance"
      nhbrdist <- unit.distances(m$grid)
      cddist <- as.matrix(object.distances(m, type = "codes"))
      cddist[abs(nhbrdist - 1) > 0.001] <- NA
      neigh.dists <- colMeans(cddist, na.rm = TRUE)
      #dists<-as.matrix(kohonen::object.distances(m,"codes"))
      hc <- neigh.dists
      backtype<-"uMatrix"

    } else  if(background_type=="property") {
      leg_name<-property
      hc<-do.call(cbind,m$codes)[,property]
      backtype<-"property"
    } else  if(background_type=="hc") {
      leg_name<-"Group"

      hc<-hc
      backtype<-"hc"
    }
  }

  hexs2<-lapply(1:length(hexs),function(i){
    x<-hexs[[i]]
    x$group<-hc[i]
    x
  })
  attr(hexs2,'pxpy')<-attr(hexs,'pxpy')
  attr(hexs2,"backtype")<-backtype
  attr(hexs2,"leg_name")<-leg_name
  hexs2
}
#' @export
get_neurons_hc<-function(m,background_type=NULL,property=NULL,hc=NULL){
  rephc<-if(m$grid$topo=="hexagonal"){7} else{5}
  group_res<-group<-rep("None", each=nrow(m$grid$pts))
  if(is.null(background_type)){
    leg_name<-NULL
    backtype<-"None"
  } else {
    if(background_type=="uMatrix"){


      leg_name<-"Distance"
      nhbrdist <- unit.distances(m$grid)
      cddist <- as.matrix(object.distances(m, type = "codes"))
      cddist[abs(nhbrdist - 1) > 0.001] <- NA
      neigh.dists <- colMeans(cddist, na.rm = TRUE)
      #dists<-as.matrix(kohonen::object.distances(m,"codes"))
      hc <- neigh.dists
      backtype<-"uMatrix"
      group<-rep(hc, each=rephc)
      splitfac<-rep(1:nrow(m$grid$pts), each=rephc)
      group_res<-as.numeric(as.vector(unlist(split(group,splitfac))))



    } else  if(background_type=="property") {
      leg_name<-property
      hc<-do.call(cbind,m$codes)[,property]
      backtype<-"property"
      group<-rep(hc, each=rephc)
      splitfac<-rep(1:nrow(m$grid$pts), each=rephc)
      group_res<-as.numeric(as.vector(unlist(split(group,splitfac))))

    } else  if(background_type=="hc") {
      leg_name<-"Group"

      hc<-hc
      backtype<-"hc"
      group<-rep(hc, each=rephc)
      splitfac<-rep(1:nrow(m$grid$pts), each=rephc)
      req(length(group)==length(splitfac))
      group_res<-factor(as.vector(unlist(split(group,splitfac))))

    }
  }

  attr(group_res,"backtype")<-backtype
  attr(group_res,"leg_name")<-leg_name
  group_res
}
#' @export
new_scale<-function (new_aes) {
  structure(ggplot2::standardise_aes_names(new_aes), class = "new_aes")
}
#' @export
new_scale_fill<-function () {
  ggnewscale::new_scale("fill")
}
get_sigma_som<-function(codebook,unit.classif,neus_ids){
  nb <- table(factor(unit.classif, levels=neus_ids))
  sqrt(apply(codebook,2,function(x,effectif){m2<-sum(effectif*(x- weighted.mean(x,effectif, na.rm=T))^2, na.rm=T)/(sum(effectif, na.rm=T)-1)},effectif=nb))

}

importance_som_hc<-function(m,layer,hc, n_top){
  codebook<-data.frame(abs(m$codes[[layer]]))
  #codebook<-data.frame(m$codes[[layer]])
  colnames(codebook)<-colnames(m$codes[[layer]])
  rownames(codebook)<-paste0("neu",1:nrow(codebook))


  grid<-data.frame(m$grid$pts)
  grid$hc<-hc
  hc_ids<-lapply(split(grid,grid$hc),function(x) as.numeric(rownames(x)))
  hc_imp_rel<-codebook
  ids<-hc_ids[[1]]
  split_codebook<-lapply(hc_ids,function(ids){
    code<-hc_imp_rel[ids,]
    unit.classif=m$unit.classif[m$unit.classif%in%ids]
    x<-get_sigma_som(code,unit.classif=unit.classif,neus_ids=ids)
    x
  })
  sigs<-data.frame(split_codebook)
  hc_imp_rel<-vegan::decostand(sigs,"total")
  top_indicators <- apply(hc_imp_rel, 2, function(x) {
    top_indices <- order(x, decreasing = TRUE)[1:n_top]
    rownames(hc_imp_rel)[top_indices]
  })
  indicadores<-as.vector(unlist(top_indicators))
  l_inds<-unique(indicadores)
  attr(l_inds,"imp_results")<-hc_imp_rel
  attr(l_inds,"imp_layer")<-layer
  l_inds
}
importance_codebook<-function(m,hc=NULL,n=5,var_pie_type=c('top_hc','top','top_w','manual'), layer=1){
  var_pie_type=match.arg(var_pie_type,c('top_hc','top','top_w','manual'))

  top_indicators<-hc_imp_rel<-NULL

  codebook<-data.frame(abs(m$codes[[layer]]))
  colnames(codebook)<-colnames(m$codes[[layer]])

  n_top<-n
  if(n_top>ncol(codebook)){
    n_top<-ncol(codebook)
  }
  grid<-data.frame(m$grid$pts)
  if(var_pie_type=='top_hc'){
    l_inds<-importance_som_hc(m,layer,hc,n_top)
    return(l_inds)
  } else if(var_pie_type=="top"){
    hc_imp_rel<-as.matrix(t(apply(codebook,1,function(x) x/sum(x))))
    pic<-order(colSums(hc_imp_rel),decreasing=T)[1:n_top]
    l_inds<-colnames(hc_imp_rel)[pic]
  } else if(var_pie_type=="top_w"){
    pic<-order(colSums(codebook),decreasing=T)[1:n_top]
    l_inds<-colnames(codebook)[pic]
  }
  attr(l_inds,"imp_results")<-hc_imp_rel
  attr(l_inds,"imp_layer")<-layer
  l_inds
}



add_codebook_pies<-function(p,m,hc=NULL,n=5,var_pie_type=c('top_hc','top','top_w','manual'),Y_palette="turbo", var_pie=F,newcolhabs=list(turbo=viridis::turbo),var_pie_transp=0.1, layer=1,pie_variables=1:2){
  var_pie_type=match.arg(var_pie_type,c('top_hc','top','top_w','manual'))
  if(isFALSE(var_pie)){
    return(p)
  }
  if(var_pie_type=="manual"){
    varY<-pie_variables
  } else{
    varY<-importance_codebook(m,hc,n,var_pie_type, layer)
  }

  c_code<-data.frame(do.call(cbind,m$codes))
  colnames(c_code)<-as.character(unlist(sapply(m$codes,colnames)))
  c_code<-c_code[,varY]

  codes2<-data.frame(id=rownames(c_code),m$grid$pts,c_code)
  df3<-reshape2::melt(codes2, c('id',"x","y"))
  colY<-c(newcolhabs[[Y_palette]] (length(varY)))
  colY<-colorspace::lighten(colY,var_pie_transp)
  df3$value2<-scales::rescale(df3$value,c(0,.45))
  p2<-p +  ggforce::geom_arc_bar(data=df3,aes(x0 = x, y0 = y, r0 = 0, r = value2, amount = value,fill = variable),stat = 'pie',linewidth=0, colour=NA)

  p<-p2+
    scale_fill_manual(name="Variables",values = colY ,drop=F)
  attr(p,"imp_results")<-attr(varY,"imp_results")
  attr(p,"imp_layer")<-attr(varY,"imp_layer")
  attr(p,"imp_vars")<-varY
  p
}
#' @export
bmu_plot<-function(m,hexs,points_tomap=NULL,bp=NULL,points=T,points_size=2,points_palette="turbo",pch=16,text=F,text_factor=NULL,text_size=1.5,text_palette="turbo",bg_palette="viridis",newcolhabs= vals$newcolhabs,bgalpha=1,border="white",indicate=NULL,cex.var=1,col.text="black",col.bg.var="white",col.bg.var.alpha=.8, newdata=NULL, show_error=NULL,base_size=12,show_neucoords=T,title="", hc=NULL, plotY=F,Y_palette="turbo", property=NULL,fill_neurons=F,border_width=0.5,
                   var_pie=F,
                   var_pie_type="top",
                   var_pie_transp=0.1,
                   n_var_pie=5,
                   var_pie_layer=1,
                   pie_variables=1:2,text_repel=F,max.overlaps=10,show_legend=T,points_legend="Observations",neuron_legend=NULL,vfm_max.overlaps=10,...) {

  if(!is.factor(points_tomap$point)){
    points_tomap$point<-factor(points_tomap$point)
  }
  if(!is.factor(points_tomap$label)){
    points_tomap$label<-factor(points_tomap$label)
  }

  req(length(hexs)>0)
  hc<-do.call(c,lapply(hexs,function(x) x$group[1]))
  req(length(hc[1])>0)
  if(hc[1]=="None"){
    background_type<-NULL
  } else{
    background_type<-attr(hexs,"backtype")
  }
  grid<-data.frame(m$grid$pts)
  df<-do.call(rbind,hexs)
  pxpy<-attr(hexs,"pxpy")
  nx=pxpy[[4]][[1]]; ny=pxpy[[4]][[2]]; px=pxpy[[1]][[1]]; py=pxpy[[1]][[2]]; rang=pxpy[[2]]; rang2=pxpy[[3]];

  if(!exists("col.arrow")){
    col.arrow<-"black"
  }

  if(!is.null(background_type)&!is.null(hc)){
    if(background_type%in%"hc"){
      bg0<-bg_color<-newcolhabs[[bg_palette]](nlevels(hc))
    } else{
      bg0<-bg_color <-getcolhabs(newcolhabs,bg_palette,length(hc))
    }

  } else{

    bg0<-bg_color <-getcolhabs(newcolhabs,bg_palette,1)

  }


  bg_color<-colorspace::lighten(bg_color, bgalpha)
  if(is.null(neuron_legend)){
    leg_name<-attr(hexs,"leg_name")
  } else{
    leg_name<-neuron_legend
  }

  mybreaks<-scales::rescale(1:nx,c(min(grid$x)+px,max(grid$x)))
  mybreaksy<-scales::rescale(1:ny,c(min(grid$y),max(grid$y)))
  if(!is.null(show_error)){
    err<-show_error

    colnames(show_error)<-c("x","y","id")
    show_error$err<-colnames(err)[3]
    show_error$hc<-rownames(show_error)
  }
  if(!is.null(background_type)){
    p<-ggplot() +
      geom_polygon(data=df, mapping=aes(x=x, y=y, group=neu, fill=group), col=border,linewidth=border_width) +xlab("")+ylab("")
  } else{
    p<-ggplot() +
      geom_polygon(data=df, mapping=aes(x=x, y=y, group=neu), col=border,linewidth=border_width,fill=bg_color) +xlab("")+ylab("")
  }


  if(is.null(background_type)){
    p<-add_codebook_pies(p,m,hc=hc,n=n_var_pie,var_pie_type=var_pie_type,Y_palette=Y_palette, var_pie=var_pie,newcolhabs=newcolhabs,var_pie_transp=var_pie_transp,layer=var_pie_layer,pie_variables=pie_variables)
  }

  imp_results<-attr(p,"imp_results")
  imp_layer<-attr(p,"imp_layer")
  imp_vars<-attr(p,"imp_vars")

  if(!is.null(background_type)&!is.null(hc)&length(unique(df$group))!=1){
    if(background_type%in%"uMatrix"){
      p<- p+scale_fill_gradientn(name=leg_name,colours=bg_color)
    } else if(background_type%in%"property"){
      p<- p+scale_fill_gradientn(name=leg_name,colours=bg_color)
    } else if ( background_type%in%"hc"){
      p<-p+scale_fill_manual(name=paste0(leg_name,if(!is.null(show_error)){paste0("",colnames(err)[3])}),values=bg_color,labels=if( !is.null(show_error)){
        c(show_error$id,
          if(anyNA(df)){""})
      } else{   levels(hc) })
    }

  } else{
    if(isFALSE(var_pie))
      p<-p+guides(fill="none")
  }
  if(isTRUE(plotY)){
    codes2<-cbind(m$codes[[2]])
    rownames(codes2)<-1:nrow(codes2)
    codes2<-data.frame(id=rownames(codes2),m$grid$pts,codes2)

    df3<-reshape2::melt(codes2, c('id',"x","y"))
    df3$variable<-factor(df3$variable, levels=levels(df3$variable),labels=m$obsLevels)

    colY<-c(newcolhabs[[Y_palette]] (length(m$obsLevels)))
    df3$value2<-scales::rescale(df3$value,c(0,.45))
    p2<-p +   ggnewscale::new_scale_fill()+
      ggforce::geom_arc_bar(data=df3,aes(x0 = x, y0 = y, r0 = 0, r = value2, amount = value,fill = variable),stat = 'pie',linewidth=0, colour=NA )+
      scale_fill_manual(name=attr(m,"supervisor"),labels =  c(m$obsLevels),values = colY ,drop=F)

    p<-p2
  }





  if(isTRUE(show_legend)){
    guide="legend"
  } else{
    guide="none"
  }


  if(!is.null(points_tomap)){
    if(isTRUE(points)){
      req(length(points_tomap)>0)
      colpoints<-newcolhabs[[points_palette]](nlevels(points_tomap$point))
      namepoints<-attr(points_tomap,"namepoints")
      if(length(unique(colpoints))==1){
        points_tomap$point<-points_legend
        namepoints=""
      }

      p<- p+geom_point(data=points_tomap, aes(x_pt, y_pt,col=point), size=points_size+2, shape=pch)+
        scale_color_manual(name=namepoints,values=colpoints,guide=guide)
    }

    if(isTRUE(text)){
      req(length(points_tomap)>0)
      coltext<-newcolhabs[[text_palette]](1)
      if(isTRUE(text_repel)){
        p<- p+ggrepel::geom_text_repel(data=points_tomap, aes(x_pt, y_pt, label=label),size=text_size+3,col=coltext,max.overlaps = max.overlaps)
      } else{
        p<- p+geom_text(data=points_tomap, aes(x_pt, y_pt, label=label),size=text_size+3,col=coltext)
      }

    }

  }

  if(!is.null(indicate)) {
    validate(need(length(bp)>0,"Require variable biplot (bp)"))
    coltext<-getcolhabs(newcolhabs,col.text,1)
    colbg.var<-getcolhabs(newcolhabs,col.bg.var,1)
    colbg.var<-adjustcolor(colbg.var, col.bg.var.alpha)
    unit(1,"points")

    p<-p+ggrepel::geom_label_repel(size = cex.var+3,data=bp, aes(x,y,label=id), fill=colbg.var,colour=coltext, seed=1,max.overlaps=vfm_max.overlaps)

  }




  if(nx>1){
    p<-p+scale_x_continuous(limits=rang,breaks=mybreaks ,labels=1:nx)
  }
  p<-p+scale_y_continuous(limits=rang2,breaks=mybreaksy ,labels=1:ny)
  p<-p+ coord_fixed()
  if(isTRUE(show_neucoords)){
    p<-p+ theme_light(
      base_size=base_size
    )+theme(panel.background=element_blank())

  } else{
    p<-p+ theme_void(
      base_size=base_size
    )+theme(panel.background=element_blank())
  }

  p<-p+ggtitle(title)
  if(isTRUE(show_legend)){
    p<-p+
      guides(color = guide_legend(order=1),
             size = guide_legend(order=2),
             shape = guide_legend(order=3))
  }


  attr(p,"imp_results")<-imp_results
  attr(p,"imp_layer")<-imp_layer
  attr(p,"imp_vars")<-imp_vars
  p
}
#' @export


bmu_plot_hc<-function(m,hexs,points_tomap=NULL,bp=NULL,points=T,points_size=2,points_palette="turbo",pch=16,text=F,text_factor=NULL,text_size=1.5,text_palette="turbo",bg_palette="viridis",newcolhabs= vals$newcolhabs,bgalpha=1,border="white",indicate=NULL,cex.var=1,col.text="black",col.bg.var="white",col.bg.var.alpha=.8, newdata=NULL, show_error=NULL,base_size=12,show_neucoords=T,title="", hc=NULL, plotY=F,Y_palette="turbo", property=NULL,fill_neurons=F,border_width=0.5,var_pie=F,var_pie_type="top",var_pie_transp=0.1,n_var_pie=5,var_pie_layer=1,pie_variables=1:2,text_repel=F,max.overlaps=10,show_legend=T,neuron_legend="Group",points_legend="Observations") {

  if(isTRUE(show_legend)){
    guide="legend"
  } else{
    guide="none"
  }

  if(!is.factor(points_tomap$point)){
    points_tomap$point<-factor(points_tomap$point)
  }
  if(!is.factor(points_tomap$label)){
    points_tomap$label<-factor(points_tomap$label)
  }

  req(length(hexs)>0)
  hc<-do.call(c,lapply(hexs,function(x) x$group[1]))
  req(length(hc[1])>0)
  grid<-data.frame(m$grid$pts)
  df<-do.call(rbind,hexs)
  pxpy<-attr(hexs,"pxpy")
  nx=pxpy[[4]][[1]]; ny=pxpy[[4]][[2]]; px=pxpy[[1]][[1]]; py=pxpy[[1]][[2]]; rang=pxpy[[2]]; rang2=pxpy[[3]];
  if(!exists("col.arrow")){
    col.arrow<-"black"
  }
  bg0<-bg_color<-newcolhabs[[bg_palette]](nlevels(hc))
  bg_color<-colorspace::lighten(bg_color, bgalpha)
  leg_name<-attr(hexs,"leg_name")
  mybreaks<-scales::rescale(1:nx,c(min(grid$x)+px,max(grid$x)))
  mybreaksy<-scales::rescale(1:ny,c(min(grid$y),max(grid$y)))
  if(!is.null(show_error)){
    err<-show_error
    colnames(show_error)<-c("x","y","id")
    show_error$err<-colnames(err)[3]
    show_error$hc<-rownames(show_error)
  }

  if(isFALSE(fill_neurons)){
    p<-ggplot() +
      geom_polygon(data=df, mapping=aes(x=x, y=y, group=neu, col=group), fill='white',linewidth=border_width) +xlab("")+ylab("")
  } else{
    p<-ggplot() +
      geom_polygon(data=df, mapping=aes(x=x, y=y, group=neu, fill=group), col=border,linewidth=border_width) +xlab("")+ylab("")
  }




  if(isFALSE(fill_neurons)){
    p<-p+scale_color_manual(name=neuron_legend,values=bg_color,labels=  levels(hc) )
  } else{
    p<-p+scale_fill_manual(name=neuron_legend,values=bg_color,labels=if( !is.null(show_error)){
      c(show_error$id,
        if(anyNA(df)){""})
    } else{   levels(hc) })
  }

  p<-add_codebook_pies(p,m,hc=hc,n=n_var_pie,var_pie_type=var_pie_type,Y_palette=Y_palette, var_pie=var_pie,newcolhabs=newcolhabs,var_pie_transp=var_pie_transp,layer=var_pie_layer,pie_variables=pie_variables)
  imp_results<-attr(p,"imp_results")
  imp_layer<-attr(p,"imp_layer")
  imp_vars<-attr(p,"imp_vars")

  if(!is.null(points_tomap)){
    if(isTRUE(points)){
      req(length(points_tomap)>0)
      if(isTRUE(fill_neurons)){
        colpoints<-newcolhabs[[points_palette]](nlevels(points_tomap$point))
      } else{
        colpoints<-newcolhabs[[points_palette]](1)
      }

      namepoints<-attr(points_tomap,"namepoints")
      if(length(unique(colpoints))==1){
        points_tomap$point<-points_legend
        namepoints=""
      }


      if(isTRUE(fill_neurons)){
        p<- p+geom_point(data=points_tomap, aes(x_pt, y_pt,col=point), size=points_size+2, shape=pch)
        p<-p+scale_color_manual(name=namepoints,values=colpoints)
      } else{
        p<- p+geom_point(data=points_tomap, aes(x_pt, y_pt), size=points_size+2, shape=pch, col=colpoints)
      }


    }


    if(isTRUE(text)){
      req(length(points_tomap)>0)
      coltext<-newcolhabs[[text_palette]](1)
      if(isTRUE(text_repel)) {
        p<- p+ggrepel::geom_text_repel(data=points_tomap, aes(x_pt, y_pt, label=label),size=text_size+3,col=coltext,max.overlaps = max.overlaps)
      } else{
        p<- p+geom_text(data=points_tomap, aes(x_pt, y_pt, label=label),size=text_size+3,col=coltext)
      }

    }

  }

  if(!is.null(indicate)) {
    validate(need(length(bp)>0,"Require variable biplot (bp)"))
    coltext<-getcolhabs(newcolhabs,col.text,1)
    colbg.var<-getcolhabs(newcolhabs,col.bg.var,1)
    colbg.var<-adjustcolor(colbg.var, col.bg.var.alpha)
    unit(1,"points")

    p<-p+ggrepel::geom_label_repel(size = cex.var+3,data=bp, aes(x,y,label=id), fill=colbg.var,colour=coltext, seed=1)

  }




  p<-p+scale_x_continuous(limits=rang,breaks=mybreaks ,labels=1:nx)+
    scale_y_continuous(limits=rang2,breaks=mybreaksy ,labels=1:ny)
  p<-p+ coord_fixed()
  if(isTRUE(show_neucoords)){
    p<-p+ theme_light(
      base_size=base_size
    )+theme(panel.background=element_blank())

  } else{
    p<-p+ theme_void(
      base_size=base_size
    )+theme(panel.background=element_blank())
  }
  p<-p+ggtitle(title)
  if(isTRUE(show_legend)){
    p<-p+
      guides(color = guide_legend(order=1),
             size = guide_legend(order=2),
             shape = guide_legend(order=3))
  }  else{
    p<-p+guides(color="none",fill="none")
  }



  attr(p,"imp_results")<-imp_results
  attr(p,"imp_layer")<-imp_layer
  attr(p,"imp_vars")<-imp_vars


  p
}
#' @export
pclus_new<-function(m,somC,points,points_factor,points_size,points_palette,pch,text,text_factor,text_size,text_palette,bg_palette,newcolhabs,bgalpha,border,npic,indicate,cex.var,col.text,col.bg.var,col.bg.var.alpha,background_type=c("hc","property","uMatrix"), property=NULL) {
  background_type<-match.arg(background_type,c("property","uMatrix","hc"))

  if(!exists("col.arrow")){
    col.arrow<-"black"
  }
  if(isTRUE(text)){
    req(text_factor)
  }

  if(isTRUE(points)){
    if(is.null(points_factor)){
      points_factor<-names(m$unit.classif)
    }
  }
  if(background_type=="uMatrix"){
    nhbrdist <- unit.distances(m$grid)
    cddist <- as.matrix(object.distances(m, type = "codes"))
    cddist[abs(nhbrdist - 1) > 0.001] <- NA
    neigh.dists <- colMeans(cddist, na.rm = TRUE)
    #dists<-as.matrix(kohonen::object.distances(m,"codes"))
    hc <- neigh.dists
    bg_color<-newcolhabs[[bg_palette]](length(x))

  } else  if(background_type=="property"){
    hc<-do.call(cbind,m$codes)[,property]
    bg_color <-getcolhabs(newcolhabs,bg_palette,length(hc))
  } else  if(background_type=="hc"){
    validate(need(!is.null(somC$som.hc),"HC not found"))
    hc<-somC$som.hc
    bg_color= newcolhabs[[bg_palette]](nlevels(hc))
  }
  bg_color<-colorspace::lighten(bg_color, bgalpha)

  grid<-data.frame(m$grid$pts)


  grid<-data.frame(m$grid$pts)
  px<-diff(unique(m$grid$pts[,1])[1:2])/2
  py<-diff(unique(m$grid$pts[,2])[1:2])/3



  nx<-m$grid$xdim
  ny<-m$grid$ydim

  rang<-range(grid$x)
  rang[1]<-rang[1]-px
  rang[2]<-rang[2]+px


  rang2<-range(grid$y)
  rang2[1]<-rang2[1]-(2*py)
  rang2[2]<-rang2[2]+(2*py)



  dists<-kohonen::dist2WU(m)
  co<-cmdscale(object.distances(m))
  copoints<-data.frame(x_pt=scales::rescale(co[,1],rang),
                       y_pt=scales::rescale(co[,2],rang2),
                       neu=m$unit.classif,
                       label=text_factor)

  copoints_list<-split(copoints,copoints$neu)
  i=56
  j=1
  hexs<-lapply(1:nrow(grid), function(i){

    x=c(
      grid[i,1],
      grid[i,1]-px,
      grid[i,1]-px,
      grid[i,1],
      grid[i,1]+px,
      grid[i,1]+px,
      grid[i,1]
    )

    y=c(
      grid[i,2]-(2*py),
      grid[i,2]-py,
      grid[i,2]+py,
      grid[i,2]+(2*py),
      grid[i,2]+py,
      grid[i,2]-py,
      grid[i,2]-(2*py)
    )
    res<-data.frame(x,y)
    cop<-copoints_list[[as.character(i)]]
    if(is.null(cop)){
      cop=NA
    } else{
      radius<-  (y[3]-y[2])*(sqrt(3)/2)
      #radius=.4
      newlim_x<-range(range(res$x))
      newlim_y<-range(range(res$y))
      newlim_x[1]<-newlim_x[1]+.25
      newlim_x[2]<-newlim_x[2]-.25

      newlim_y<-  c(res$y[2]+(py/2),(res$y[3]-(py/2)))

      cop$x_pt<-scales::rescale(cop$x_pt,  newlim_x)
      cop$y_pt<-scales::rescale(cop$y_pt,  newlim_y)
    }
    res$neu=i
    res$group<-hc[i]
    attr(res,"points")<-cop
    res
  })

  {


    df<-data.frame(do.call(rbind,hexs))

    if(background_type=="uMatrix"){
      leg_name<-"Distance"
    } else if(background_type=="property"){
      leg_name<-property
    } else if ( background_type=="hc"){
      leg_name<-"Cluster"
    }

    mybreaks<-scales::rescale(1:nx,c(min(grid$x)+px,max(grid$x)))
    mybreaksy<-scales::rescale(1:ny,c(min(grid$y),max(grid$y)))




    p<-ggplot() +
      geom_polygon(data=df, mapping=aes(x=x, y=y, group=neu, fill=group), col=border) +xlab("")+ylab("")
    if(background_type=="uMatrix"){
      p<- p+scale_fill_gradientn(name=leg_name,colours=bg_color)
    } else if(background_type=="property"){
      p<- p+scale_fill_gradientn(name=leg_name,colours=bg_color)
    } else if ( background_type=="hc"){
      p<- p+scale_fill_manual(name=leg_name,values=bg_color)
    }




    p
    copoints2<-na.omit(do.call(rbind,lapply(hexs,function(x){
      attr(x,"points")
    })))
    if(isTRUE(points)){
      p<- p+geom_point(data=copoints2, aes(x_pt, y_pt), size=points_size, shape=pch)
    }
    if(isTRUE(text)){
      p<- p+geom_text(data=copoints2, aes(x_pt, y_pt, label=label),size=text_size)
    }
    if(!is.null(indicate)) {

      inds<-indsom(m,indicate=indicate,npic=ncol(do.call(cbind,m$codes)))


      CORMAP<-inds[[1]]
      result<-inds[[2]]
      bp=data.frame(na.omit(as.matrix(t(CORMAP))))
      bp[,1]<-  scales::rescale(bp[,1], c(min(m$grid$pts[,1]), max(m$grid$pts[,1])))
      bp[,2]<-  scales::rescale(bp[,2], c(min(m$grid$pts[,2]), max(m$grid$pts[,2])))


      bp<-bp[ inds[[3]],]

      p<-p+scale_x_continuous(limits=rang,breaks=mybreaks ,labels=1:nx)+
        scale_y_continuous(limits=rang2,breaks=mybreaksy ,labels=1:ny)

      if(indicate%in%c('cor_bmu','cor_hc')){
        picvar<-getvars_bycluster(bp, m, result, hc=somC$som.hc)[1:npic]

      } else{
        picvar<-inds[[3]][1:npic]
      }

      indicadores<-picvar
      coltext<-getcolhabs(newcolhabs,col.text,1)

      colbg.var<-getcolhabs(newcolhabs,col.bg.var,1)
      colbg.var<-adjustcolor(colbg.var, col.bg.var.alpha)

      {
        colnames(bp)<-c('x','y')
        bp$id<-rownames(bp)

        p<-p+ggrepel::geom_label_repel(data=bp[indicadores,], aes(x,y,label=id), fill=colbg.var,colour=coltext, seed=1)

      }



      colreq<-getcolhabs(newcolhabs,factor.pal,2)


    }

    p<-p+ coord_fixed() + theme(panel.background=element_blank())
  }

p
}

#' @export
legend.col <- function(col, lev,  bx =par("usr")*.8, cex=1,property){

  opar <- par
  n <- length(col)
  box.cx <- c(bx[2] + (bx[2] - bx[1]) / 1000,
              bx[2] + (bx[2] - bx[1]) / 1000 + (bx[2] - bx[1]) / 50)
  box.cy <- c(bx[3], bx[3])
  box.sy <- (bx[4] - bx[3]) / n
  xx <- rep(box.cx, each = 2)
  par(xpd = TRUE)
  for(i in 1:n){
    yy <- c(box.cy[1] + (box.sy * (i - 1)),
            box.cy[1] + (box.sy * (i)),
            box.cy[1] + (box.sy * (i)),
            box.cy[1] + (box.sy * (i - 1)))
    polygon(xx, yy, col = col[i], border = col[i])

  }
  mi<-box.cy[1] + (box.sy * (n))
  ma<-box.cy[1] + (box.sy * (1))
  ppp<-seq(mi,ma,len=5)
  ppp_text<-seq(min(lev),max(lev),len=5)
  dec<-decimalplaces(ppp_text[[1]])-1
  for(i in dec:1){
    if(any(diff(round(ppp_text,i))==0)){break()}
    ro<-round(ppp_text,i)
  }
  ppp_text<-rev(ro)

  text(max(xx)+.5,max(yy)+.5,    property)
  text(max(xx)+.5,ppp,    ppp_text, cex=cex)
 # axis(side = 4, las = 2, tick = FALSE, line = .25)
  par <- opar
}
#' @export
pcorr<-function(m,npic=10, indicate=c("var","cor"), col.arrow="gray80",cex.var=1, cex=1, pch=16, labels.ind=NULL,bg_palette="turbo", factor.pal="gray", points=T,ncol=1,insetx=0,insety=0,alpha.legend=0.85, predict=FALSE, pred_col="firebrick", alpha_bg=1, border="white",col.text="black", legend=T,newcolhabs,bg_vector=NULL,showtrain=T, xclus=NULL,yclus=NULL,labclus=NULL, main="", bottom_leg=NULL,classif=NULL, property=NULL,plot_property=F,uMatrix=F,bgalpha=0,col.bg.var="white"){
  indicadores<-NULL
  result<-NULL

  if(isFALSE(plot_property)&isFALSE(uMatrix)){
    bmuvalues<-as.matrix(kohonen::unit.distances(m$grid,m$grid$toroidal))[,1]
    bmuvalues<-bmuvalues+seq(0.001,0.002,length.out=length(bmuvalues))
    maxdepth<-max(bmuvalues)
    colbmu <- adjustcolor(getcolhabs(newcolhabs,bg_palette,length(bmuvalues) ), alpha_bg)
    colcodes<-colbmu[cut(bmuvalues,breaks = bmuvalues )]
    colcodes[1]<-colbmu[1]
  }

  if(isTRUE(uMatrix)&isFALSE(plot_property)){

    nhbrdist <- unit.distances(m$grid)
    cddist <- as.matrix(object.distances(m, type = "codes"))
    cddist[abs(nhbrdist - 1) > 0.001] <- NA
    neigh.dists <- colMeans(cddist, na.rm = TRUE)
    #dists<-as.matrix(kohonen::object.distances(m,"codes"))
    bmuvalues <-x <- neigh.dists
    bgcols<-newcolhabs[[bg_palette]](length(x))
    colcodes=bgcols[cut(x,length(x))]
  }
  if(isTRUE(plot_property)&isFALSE(uMatrix)){
    bmuvalues<-do.call(cbind,m$codes)[,property]
    colbmu <-getcolhabs(newcolhabs,bg_palette,10 )
    colcodes=colbmu[cut(bmuvalues,10)]
  }
  colreq<-getcolhabs(newcolhabs,factor.pal,2)
  if(colreq[1]!=colreq[2]){
    colfactors<-getcolhabs(newcolhabs,factor.pal,nlevels(labels.ind))
    col<-colfactors[labels.ind]} else {
      colfactors<-getcolhabs(newcolhabs,factor.pal,1)
      col<-colfactors
    }

  if(is.null(labels.ind)){
    colfactors<-getcolhabs(newcolhabs,factor.pal,1)
    col<-colfactors

  }

  opar<-par(no.readonly=TRUE)
  shape="straight"
  indicate=match.arg(indicate,c("var","cor"))
  if(!is.null(bg_vector)){
    colcodes<-adjustcolor(bg_vector, alpha_bg)
  }

  if(npic==0){
    par(mar=c(7,6,2,0))
    set.seed(1)
    if(isFALSE(points)){
      plot(m,"mapping",shape=shape, border=border, main=main,keepMargins =T, bgcol=colcodes,codeRendering=F, labels=labels.ind, cex=cex, pch=pch, col=col,classif=classif )}else{

        plot(m,"mapping",shape=shape, border=border, main=main,keepMargins =T, bgcol=colcodes,codeRendering=F, labels=NULL, cex=cex, pch=pch, col=col,classif=classif )
      }
    if(!isFALSE(predict)){

      if(isTRUE(showtrain)){

        cores<-c(getcolhabs(newcolhabs,factor.pal,length(m$unit.classif)),
                 getcolhabs(newcolhabs,pred_col,length(predict$unit.classif))

        )
        classif<-c(m$unit.classif,predict$unit.classif)

      } else{
        if(colreq[1]!=colreq[2]){
          colfactors<-getcolhabs(newcolhabs,factor.pal,nlevels(labels.ind))
          col<-colfactors[labels.ind]} else {
            colfactors<-getcolhabs(newcolhabs,factor.pal,1)
            col<-colfactors
          }
        cores<-col
        classif<-c(predict$unit.classif)

      }

      par(mar=c(7,6,2,0))
      set.seed(1)
      if(isFALSE(points)){

        plot(m,"mapping",shape=shape, border=border, main=main,keepMargins =T, bgcol=colcodes,codeRendering=F, labels=labels.ind, cex=cex, pch=pch, col=cores,classif=classif)}else{
          plot(m,"mapping",shape=shape, border=border, main=main,keepMargins =T, bgcol=colcodes,codeRendering=F, labels=NULL, cex=cex, pch=pch, col=cores,classif=classif)
        }

    }
    plotresult<-recordPlot()
  }


  if(isTRUE(plot_property)&isFALSE(uMatrix)){
    x<-range(m$grid$pts[,1])
    y<-range(m$grid$pts[,2])
    bx=par("usr")
    bx[c(1,2)]<-x+1
    bx[c(3,4)]<-y
    legend.col(col = getcolhabs(newcolhabs,bg_palette,100), lev = bmuvalues, bx,cex=cex, property)
  } else  if(isTRUE(uMatrix)&isFALSE(plot_property)){
    x<-range(m$grid$pts[,1])
    y<-range(m$grid$pts[,2])
    bx=par("usr")
    bx[c(1,2)]<-x+1
    bx[c(3,4)]<-y
    legend.col(col = getcolhabs(newcolhabs,bg_palette,length(bmuvalues)), lev = bmuvalues, bx,cex=cex, "Distance")
  }

  if(npic>0){
    grid.size<-nrow(m$grid$pts)
    nb <- table(factor(m$unit.classif, levels=1:grid.size))
    CORMAP <- apply(do.call(cbind,m$codes),2,weighted.correlation,w=nb,grille=m)
    sigma2  <- sqrt(apply(do.call(cbind,m$codes),2,function(x,effectif){m2<-sum(effectif*(x- weighted.mean(x,effectif))^2)/(sum(effectif)-1)},effectif=nb))

    if(indicate=="cor"){
      indicadores<-names(sort(sigma2,decreasing=T))[1:npic]
      scores<-t(CORMAP)
      Xsp<-  scores[,1]
      Ysp<- scores[,2]
      A<- rowSums(abs(data.frame(S1=scores[names(which(Xsp<0&Ysp>0)),1],S2=scores[names(which(Xsp<0&Ysp>0)),2])))
      B<-rowSums(abs(data.frame(S1=scores[names(which(Xsp>0&Ysp>0)),1],S2=scores[names(which(Xsp>0&Ysp>0)),2])))
      C<-rowSums(abs(data.frame(S1=scores[names(which(Xsp<0&Ysp<0)),1],S2=scores[names(which(Xsp<0&Ysp<0)),2])))
      D<-rowSums(abs(data.frame(S1=scores[names(which(Xsp>0&Ysp<0)),1],S2=scores[names(which(Xsp>0&Ysp<0)),2])))
      Xsp1<-names(A)[order(A, decreasing = T)][1:npic]
      Xsp2<-names(B)[order(B, decreasing = T)][1:npic]
      Ysp1<-names(C)[order(C, decreasing = T)][1:npic]
      Ysp2<-names(D)[order(D, decreasing = T)][1:npic]
      indicadores<- unlist(lapply(data.frame(t(matrix(c(Xsp1,Xsp2,Ysp1,Ysp2),ncol=4))),cbind))
      indicadores= na.omit(indicadores[1:npic])

      result<-scores[indicadores,]
      colnames(result)<-c("cor.x", "cor.y")

    }
    if(indicate=="var") { indicadores<-na.omit(names(sort(sigma2,decreasing=T))[1:npic])
    result<-data.frame(sigma2[indicadores])
    colnames(result)<-"Variance"

    }
    bp=data.frame(na.omit(as.matrix(t(CORMAP))))
    par(mar=c(7,6,2,0))
    {

      set.seed(1)
      if(isFALSE(points)){ plot(m,"mapping",shape=shape, border=border, main=main,keepMargins =T, bgcol=colcodes,codeRendering=F, labels=labels.ind, cex=cex, pch=pch, col=col)}else{
        plot(m,"mapping",shape=shape, border=border, main=main,keepMargins =T, bgcol=colcodes,codeRendering=F, labels=NULL, cex=cex, pch=pch, col=col)
      }


      set.seed(NULL)
    }

    bp[,1]<-  scales::rescale(bp[,1], c(min(m$grid$pts[,1]), max(m$grid$pts[,1])))
    bp[,2]<-  scales::rescale(bp[,2], c(min(m$grid$pts[,2]), max(m$grid$pts[,2])))
    boxtext(x =(bp[indicadores,1]), y = (bp[indicadores,2]), labels = colnames(CORMAP[,indicadores]), col.bg = adjustcolor("white", 0.5),  cex=cex.var, border.bg = col.arrow, col.text=col.text)
    colreq<-getcolhabs(newcolhabs,factor.pal,2)




  }


  if(isTRUE(points)) {
    if(colreq[1]!=colreq[2]) {
      legend("topr",pch=pch,legend=levels(labels.ind), col=getcolhabs(newcolhabs,factor.pal, nlevels(labels.ind)),border=NA, ncol=ncol, inset=c(insetx,insety),cex=cex, bg=adjustcolor('white',alpha.legend))

    }}
  if(!is.null(xclus)){
    if(!is.null(yclus)){
      if((!is.null(labclus))){
        boxtext(x =xclus, y = yclus, labels = labclus, col.bg = adjustcolor("white", 0.5),  cex=cex.var, border.bg = col.arrow, col.text=col.text)


      }
    }
  }
  if(!is.null(bottom_leg)){
    legend("bottom",bottom_leg,text.font=3, bty="n")
  }



  plotresult<-recordPlot()
  attr(plotresult,"indicadores")<-indicadores
  attr(plotresult,"result")<-result
  #plotind(m,indicadores)
  on.exit(par(opar),add=TRUE,after=FALSE)
  return(plotresult)
}
#' @export
plot.som_grid<-function (grid){
  myhexagons<-function (x, y, unitcell, col, border)
  {
    if (length(col) != length(x))
      col <- rep(col[1], length(x))
    for (idx in 1:length(x)) myHexagon(x[idx], y[idx], unitcell = unitcell,
                                       col = col[idx], border = border)
  }
  myHexagon<-function (a, b, unitcell = 1, col = "grey", border = NA)
  {
    x <- a - unitcell/2
    y <- b - unitcell/2
    polygon(c(x, x, x + unitcell/2, x + unitcell, x + unitcell,
              x + unitcell/2), c(y + unitcell * 0.2113249, y + unitcell *
                                   0.7886751, y + unitcell * 1.07735, y + unitcell * 0.7886751,
                                 y + unitcell * 0.2113249, y - unitcell * 0.07735027),
            col = col, border = border)
  }

  margins <- rep(0.6, 4)
  par(mar = margins)
  plot(grid, type="n")
  sym <- ifelse("straight" == "round", "circle", ifelse(grid$topo ==
                                                          "rectangular", "square", "hexagon"))
  switch(sym, circle = symbols(grid$pts[, 1], grid$pts[,2], circles = rep(0.5, nrow(grid$pts)), inches = FALSE,add = TRUE, fg = "black", bg = "transparent"), hexagon = myhexagons(grid$pts[,1], grid$pts[, 2], unitcell = 1, col = "transparent", border = "black"),
         square = symbols(grid$pts[, 1], grid$pts[, 2], squares = rep(1,nrow(grid$pts)), inches = FALSE, add = TRUE, fg = "black",bg = "transparent"))

  text(grid[[1]],labels=1:nrow(grid[[1]]))
}




#' @export
indsom<-function(m,indicate="var",npic=10){

  codes<-m$codes
  m$codes<-lapply(names(codes),function(i){
    x<-codes[[i]]
    res<-colnames(x)
    if(is.null(res)){
      res<-paste0(i,1:ncol(x))
    }
    colnames(x)<-    res
    x
  })
  if(npic>0) {
    if(indicate=="cor_hc"){indicate="var"}
    grid.size<-nrow(m$grid$pts)
    nb <- table(factor(m$unit.classif, levels=1:grid.size))

    CORMAP<-lapply(m$codes,function(x){
      apply(x,2,weighted.correlation,w=nb,grille=m)
    })
    names(CORMAP)<-NULL
    CORMAP<-do.call(cbind,CORMAP)
    sigma2<-lapply(m$codes,function(xx){
      sqrt(apply(xx,2,function(x,effectif){m2<-sum(effectif*(x- weighted.mean(x,effectif, na.rm=T))^2, na.rm=T)/(sum(effectif, na.rm=T)-1)},effectif=nb))
    })
    names(sigma2)<-NULL
    sigma2<-do.call(c,sigma2)


    if(indicate=="cor"){
      indicadores<-names(sort(sigma2,decreasing=T))[1:npic]
      scores<-t(CORMAP)
      Xsp<-  scores[,1]
      Ysp<- scores[,2]
      A<- rowSums(abs(data.frame(S1=scores[names(which(Xsp<0&Ysp>0)),1],S2=scores[names(which(Xsp<0&Ysp>0)),2])))
      B<-rowSums(abs(data.frame(S1=scores[names(which(Xsp>0&Ysp>0)),1],S2=scores[names(which(Xsp>0&Ysp>0)),2])))
      C<-rowSums(abs(data.frame(S1=scores[names(which(Xsp<0&Ysp<0)),1],S2=scores[names(which(Xsp<0&Ysp<0)),2])))
      D<-rowSums(abs(data.frame(S1=scores[names(which(Xsp>0&Ysp<0)),1],S2=scores[names(which(Xsp>0&Ysp<0)),2])))
      Xsp1<-names(A)[order(A, decreasing = T)][1:npic]
      Xsp2<-names(B)[order(B, decreasing = T)][1:npic]
      Ysp1<-names(C)[order(C, decreasing = T)][1:npic]
      Ysp2<-names(D)[order(D, decreasing = T)][1:npic]
      indicadores<- unlist(lapply(data.frame(t(matrix(c(Xsp1,Xsp2,Ysp1,Ysp2),ncol=4))),cbind))
      indicadores= na.omit(indicadores[1:npic])

      result<-scores[indicadores,]
      colnames(result)<-c("cor.x", "cor.y")

    }
    if(indicate=="var") {
      indicadores<-na.omit(names(sort(sigma2,decreasing=T))[1:npic])
      indicadores<-indicadores[indicadores%in%names(sigma2)]
      result<-data.frame(sigma2[indicadores])
      colnames(result)<-"Variance"


    }


    return(list(CORMAP,result,indicadores))
  }
}

#' @export
getvars_bycluster<-function(bp, m, result, hc){
  grid<-m$grid$pts
  colnames(bp)<-colnames(grid)<-c("x","y")
  bp$neu<-unlist(lapply(1:nrow(bp),function(i){
    which.min(as.matrix(dist(rbind(bp[i,],grid)))[,1][-1])
  }))

  req(nrow(grid)==length(hc))
  bp$hc<-data.frame(bmu=1:nrow(grid), hc=hc)[bp$neu,"hc"]
  bp$value<- result[,1]
  lis<-split(bp,bp$hc)
   res<-do.call(rbind,lapply(lis,function(x){
    if(nrow(x)>0){
      x$ord<-1:nrow(x)
      x$Variable<-rownames(x)
      x

    }
  }))
  res[with(res, order(ord , hc)), "Variable"]

}
#' @export
pclus<-function(somC,  cex=1, pch=16, labels.ind=NULL,bg_palette="turbo", factor.pal="gray", points=F,ncol=1,insetx=0,insety=0,alpha.legend=0.85,newcolhabs, bgalpha=0,
                indicate=NULL,
                npic=NULL,col.arrow="gray80",cex.var=1,
                col.text="black", main="",
                col.bg.var="white", col.bg.var.alpha= 0.5

                ){

  {

    if(!exists("col.arrow")){
      col.arrow<-"black"
    }

    shape="straight"
    opar<-par(no.readonly=TRUE)
    m=somC$som.model
    som_cluster_adj=somC$som.hc
    col_vector=getcolhabs(newcolhabs,bg_palette,somC$groups)
    colreq<-getcolhabs(newcolhabs,factor.pal,2)


    if(colreq[1]!=colreq[2]){
      colfactors<-getcolhabs(newcolhabs,factor.pal,nlevels(labels.ind))
      col<-colfactors[labels.ind]} else {
        colfactors<-getcolhabs(newcolhabs,factor.pal,1)
        col<-colfactors
      }
    col_vector<-adjustcolor(col_vector,bgalpha)

    set.seed(1)
    if(isFALSE(points)){
      plot(m, type="mapping", main = "", bgcol = col_vector[som_cluster_adj], pchs = pch,border="white",keepMargins =T,codeRendering=F,shape=shape,labels=labels.ind, cex=cex, col=col)
      add.cluster.boundaries(m, som_cluster_adj)}else{
        plot(m, type="mapping", main = "", bgcol = col_vector[som_cluster_adj], pchs = pch,border="white",keepMargins =T,codeRendering=F,shape=shape,labels=NULL, cex=cex, col=col)
        add.cluster.boundaries(m, som_cluster_adj)}
    colreq<-getcolhabs(newcolhabs,factor.pal,2)
    if(isTRUE(points)){
      if(colreq[1]!=colreq[2]) {
        if(!is.null(labels.ind)){
          x<-range(m$grid$pts[,1])
          y<-range(m$grid$pts[,2])

          legend(max(x)+1,max(y),pch=pch,legend=levels(labels.ind), col=getcolhabs(newcolhabs,factor.pal, nlevels(labels.ind)),border=NA, ncol=ncol,cex=cex, bg=adjustcolor('white',alpha.legend),xjust=0)}

      }
    }

  }
  if(!is.null(indicate)) {

    inds<-indsom(m,indicate=indicate,npic=ncol(do.call(cbind,m$codes)))


    CORMAP<-inds[[1]]
    result<-inds[[2]]
    bp=data.frame(na.omit(as.matrix(t(CORMAP))))
    bp[,1]<-  scales::rescale(bp[,1], c(min(m$grid$pts[,1]), max(m$grid$pts[,1])))
    bp[,2]<-  scales::rescale(bp[,2], c(min(m$grid$pts[,2]), max(m$grid$pts[,2])))


    bp<-bp[ inds[[3]],]


    if(indicate%in%c('cor_bmu','cor_hc')){
      picvar<-getvars_bycluster(bp, m, result, hc=somC$som.hc)[1:npic]

    } else{
      picvar<-inds[[3]][1:npic]
    }

    indicadores<-picvar
    coltext<-getcolhabs(newcolhabs,col.text,1)
    colbg.var<-getcolhabs(newcolhabs,col.bg.var,1)


    {



    boxtext(x =(bp[indicadores,1]),
            y = (bp[indicadores,2]),
            labels = colnames(CORMAP[,indicadores]),
            col.bg = adjustcolor(colbg.var, col.bg.var.alpha),
            cex=cex.var,
            border.bg = col.arrow,
            col.text=coltext,
            pos=NULL,
            adj=c(0,1),
            offset=1,
            padding = c(0.5, 0.5),
            font = par('font'))
    }



    colreq<-getcolhabs(newcolhabs,factor.pal,2)


  }
  x<-range(m$grid$pts[,1])
  y<-range(m$grid$pts[,2])
  legend(min(x)-1,max(y),pch=15,legend=levels(somC$somC), col=getcolhabs(newcolhabs,bg_palette, nlevels(somC$somC)),border=NA, xjust=1)

  codes.plot<- recordPlot()
  on.exit(par(opar),add=TRUE,after=FALSE)
  return(codes.plot)

}


#' @export
errors_som<-function(m){
  res<-sapply(1:length(m$data), function(i) {
    mtemp<-m
    mtemp$codes<-list(m$codes[[i]])
    aweerros<-aweSOM::somQuality(mtemp,m$data[[i]])[1:4]

  })
  colnames(res)<-names(m$data)
  weights<-table(factor(m$unit.classif, levels=1:nrow(m$grid$pts)))
  neu.uti<-  sum(weights==0)/nrow(m$grid$pts)

  list(som_quality=res,neu.uti=neu.uti)
}

#' @export
pchanges<-function(m){
  opar<-par(no.readonly=TRUE)
  m$errorsom<-aweSOM::somQuality(m,as.matrix(m$data[[1]]))[1:4]
  weights<-table(factor(m$unit.classif, levels=1:nrow(m$grid$pts)))
  m$errorsom$neu.uti<-  sum(weights==0)/nrow(m$grid$pts)

  errossom<-round(unlist(m$errorsom),3)


plot(m,"changes", keepMargins = TRUE)
  #legend("bottoml", legend=c(paste("Quantization:",errossom[1]),paste("Topographic:",errossom[3]),paste("Explain_var:",errossom[2])), bty='n',cex=.7)
  pchanges <- recordPlot()
  on.exit(par(opar),add=TRUE,after=FALSE)
  return(pchanges)

}
#' @export
pcodes<-function(somC,...){
  opar<-par(no.readonly=TRUE)
  sample.names<-gsub("Sd.*","",gsub(".*#","",rownames(somC$som.model$data[[1]])))

  m<-somC$som.model
  shape="straight"
  colhabs=somC$colhabs
  plot(somC$som.model, type="mapping", bgcol = somC$colhabs[somC$som.hc], pchs = 16,border="white",shape=shape,keepMargins =T, labels=sample.names,...)
  add.cluster.boundaries(m, somC$som.hc)
  pcodes<-recordPlot()
  on.exit(par(opar),add=TRUE,after=FALSE)
  return(pcodes)

}
#' @export
phist<-function(data){
  opar<-par(no.readonly=TRUE)
  par(mar=c(5,5,5,1))
  hist(colSums(data, na.rm=T), main="")
  phist<-recordPlot()
  on.exit(par(opar),add=TRUE,after=FALSE)
  return(phist)
}
#' @export
smwhc<-function(result,n.rand=99,ws, session=getDefaultReactiveDomain()){
  result0<-result
  yo=result$WSS
  smw<-list()
  yo<-data.frame(yo)

  withProgress(max =length(ws),session=session,{
    for (j in 1:length(ws)) {


      w1<-ws[j]
      incProgress(1,message=paste0("window size:",w1),session=session)
      DPtable<-smw.root3(yo, w1, "euclidean")
      OB<-DPtable[, 3]
      seq_yo<-1:nrow(yo)
      withProgress(max=n.rand,session=session,{
        rdp<-lapply(paste0("rand",1:n.rand),function(b){
          incProgress(1,message=paste0("Randomization:",b),session=session)
          comm.rand<-sample(unlist(yo),replace = T)
          a<-smw.root3(data.frame(comm.rand),
                       w1, "euclidean")[3]
          a
        })
        rdp<-data.frame(rdp)
      })
      Dmean<-apply(rdp, 1, mean)
      SD<-apply(rdp, 1, sd)
      oem<-sum(Dmean)/(nrow(yo) - w1)
      osd<-sum(SD)/(nrow(yo) - w1)
      Dz<-(OB - oem)/osd
      DPtable$zscore<-Dz
      DPtable$sd<-SD
      smw[[j]]<-list(dp = data.frame(DPtable), rdp =rdp)
    }
  })


  names(smw)<-paste("w", ws, sep = "")

  result<-data.frame(pos=smw[[1]]$dp$positions)
  sds<-data.frame(pos=smw[[1]]$dp$positions)
  for(i in 1:length(smw) ){
    x<-smw[[i]]
    result[x$dp$positions, names(smw)[i]]<-x$dp$diss
    sds[ rownames(sds) %in%x$dp$positions, names(smw)[i]]<-  x$dp$sd
  }


  sd<-apply(sds,1,function(x) mean(x,na.rm=T))
  dis<-apply(result,1,function(x) mean(x,na.rm=T))
  distab<-data.frame(dis,sd)
  distab[nrow(yo),]<-NA
  colnames(distab)<-c("Mean Diss","SD Diss")

  cbind(result0,distab)
}
#' @export
get_screeplot_smw_sig<-function(smw, tol=NULL){
  if(ncol(smw)==2){return(smw)} else{
    req(!is.null(tol))
  }
  smw2<-smw[!is.na(smw[,3]),]
  smw2$Sig<-NA
  pic<-which(apply(smw,1,function(x){
    x[3]>tol*x[4]
  }))
  if(length(pic)>0){
    smw2[pic,"Sig"]<-paste0("Mean Diss>",tol,"*","SD")
  }

  smw2
}
#' @export
get_ggdata_screesms<-function(smw2, tol){
  tol_level<-paste0("Mean Diss>",tol,"*","SD")
  smw3<-smw2
  wss<-data.frame(x=smw3[,1],y=smw3[,2])
  wss$color<-"WSS"
  diss<-data.frame(x=smw3[,1],y=smw3[,3])
  diss$color<-"Dissimilarity"
  sigs<-data.frame(x=smw3[,1],y=smw3[,3], color=smw3[,5])
  wss$group<-"wss"
  diss$group<-"diss"
  sigs$group<-"sigs"
  df<-rbind(wss,diss,sigs)
  df$color<-factor(df$color,levels=c("WSS","Dissimilarity",tol_level))
  df
}
#' @export
scree_smw_ggplot<-function(df){
  maxdiss<-which.max(df[which(df$color=="Dissimilarity"),"y"])

  linetype = c(1,1,1)[levels(df$color)%in%df$color]
  shape = c(16, NA,8)[levels(df$color)%in%df$color]
  values=c('#619cffff',"Seagreen","red")[levels(df$color)%in%df$color]

  dfli<-split(df,df$group)
  df1<-dfli[["wss"]]
  df2<-dfli[["diss"]]
  df3<-dfli[["sigs"]]

  breaks<-c(levels(df$color))
  df3$na = factor(cumsum(is.na(df3$color)))
  p<-ggplot(df1, aes(x=x, y=y))
   p <-p+geom_line(aes(group=color, color=color)) +
    geom_point(data=subset(df, color == "WSS"), aes(x=x, y=y, color=color))
   p<-p +geom_line(data=df2,aes(group=color, color=color))
   if(any(!is.na(df3$color))){
     p<-p+geom_line(data=df3,aes(group=na, color=color))+
       geom_point(data=subset(df3, !is.na(color)),aes(color=color), shape=8)
   }



  p<-p+guides(color = guide_legend(override.aes = list(linetype = linetype, shape = shape)))  +scale_color_manual(name="",values=values, breaks=breaks)

  p<-p+
    xlab("Number of Clusters")+ylab("Total Within Sum of Squares (WSS)")
  p<-p +
    scale_x_continuous(breaks = unique(df$x)) +
    scale_y_continuous(breaks = pretty(df$y), sec.axis = sec_axis(~ . * .9, name = "Dissimilarity"))
  if(length(maxdiss)>0){
    p<-p+ geom_vline(aes(xintercept=maxdiss, linetype="Breakpoint"), color="black")
  }
  p<-p+
    scale_linetype_manual(name="", values=c(Breakpoint=2)) +
    theme_light() +
    theme(
      axis.text.y = element_text(color = '#619cffff'),
      axis.title.y = element_text(color = '#619cffff'),
      axis.text.y.right = element_text(color = "SeaGreen"),
      axis.title.y.right = element_text(color = "SeaGreen"),
      legend.position = "right",
      panel.grid.minor=element_blank(),
      legend.box = "vertical",
      legend.box.just = "top",
      legend.margin=margin(0, 0, 0, 0)
    ) +
    guides(color = guide_legend(order=1,override.aes = list(linetype = linetype, shape = shape)),
           linetype = guide_legend(order=2, override.aes = list(color="black", shape=NA)))


  p
}
#' @export
smw.root3<-function (yo, w=4, dist="euclidean"){
  if (w%%2 == 1) {
    stop("window size should be even")
  }
  diss <- NULL
  yo <- data.frame(yo)
  nrow_yo=nrow(yo)
  yo<-data.frame(t(yo))
  i=1
  for (i in 1:(nrow_yo - w + 1)) {
    wy.ord <- yo[i:(i + (w - 1))]
    div<-length(wy.ord)/2
    half.a <- apply(wy.ord[1:(div)], 1, sum)
    half.b <- apply(wy.ord[-c(1:(div))], 1,sum)
    d <- dist(rbind(half.a, half.b))
    diss[i] <- d

  }
  k <- (w/2)
  for (i in 1:((nrow_yo - w))) {
    k[i + 1] <- (w/2) + i
  }
  positions<-k

  result<-data.frame(positions =positions, sampleID = colnames(yo)[positions],
                     diss = diss)

  return(invisible(result))
}




#' @export
smw.root<-function(yo, w){

  if (w%%2 == 1) {
    stop("window size should be even")
  }
  diss <- NULL
  yo <- data.frame(yo)
  for (i in 1:(nrow(yo) - w + 1)) {
    wy.ord <- yo[i:(i + (w - 1)), ]
    half.a <- sum(wy.ord[1:(length(wy.ord)/2) ])
    half.b <- sum(wy.ord[-c(1:(length(wy.ord)/2))])
    d <- dist(rbind(half.a, half.b))
    diss[i] <- d
    k <- (w/2)
    for (i in 1:((nrow(yo) - w))) {
      k[i + 1] <- (w/2) + i
    }
  }
  result <- data.frame(positions = k, sampleID = rownames(yo)[k],
                       diss = diss)
  return(invisible(result))
}



#' @export
pUmatrix<-function(m){  opar<-par(no.readonly=TRUE)
shape="straight"
plot(m, type="dist.neighbours", main = "U-Matrix",border="white",shape=shape,keepMargins =T, palette.name=viridis)
pUmatrix<-recordPlot()
on.exit(par(opar),add=TRUE,after=FALSE)
return(pUmatrix)

}
#' @export
pcounts<-function(m){  opar<-par(no.readonly=TRUE)
shape="straight"
plot(m, type="counts", main = "Counts",border="white",shape=shape,keepMargins =T)


pcounts<-recordPlot()
on.exit(par(opar),add=TRUE,after=FALSE)
return(pcounts)
}
#' @export
coolBlueHotRed <- function(n, alpha = 1) {
  rainbow(n, end=4/6, alpha=alpha)[n:1]
}
#' @export
pproperty<-function(m, main, pic){
  opar<-par(no.readonly=TRUE)
  shape="straight"
  plot(m,type="property",property=do.call(cbind,m$codes)[,pic],main=main,cex=0.5,shape=shape,keepMargins = TRUE, palette.name=coolBlueHotRed)
  pproperty<-recordPlot()
  on.exit(par(opar),add=TRUE,after=FALSE)
  return(pproperty)

  plot(m,"mapping",shape=shape, border=border, main=main,keepMargins =T, bgcol=colcodes,codeRendering=F, labels=labels.ind, cex=cex, pch=pch, col=col,classif=classif )
}
#' @export
pbox<-function(res, palette="viridis", coefic=1.5, lab_out=NULL ,cex.lab=1, lwd=1, main=paste0(colnames(res)[2],"~",colnames(res)[1]), ylim=range(na.omit(res[,2])), insetx=0, insety=0, show_outs=T,mar_b=5,mar_t=5,mar_l=5,mar_r=NULL, srt=0,srty="horizontal",xlab.adj=0,newcolhabs, showleg=F, xsize=1, ysize=1,tck=-0.05, linexlab=1, lineylab=1, cex_xlab=1, cex_ylab=1, font_xlab="plain", font_ylab='plain', xlab_text=colnames(res)[1], ylab_text=colnames(res)[2], horizontal=F,varwidth=F,cex_pbox=1){
  if(is.null(mar_r)){
    mar_r<-2
  }
  if(!is.null(lab_out)){show_outs=F}
  res<-data.frame(res)

  fac_o<-res[,1]
  somprev <- res[,1]


  nlevels <- nlevels(somprev)
  colhabs =  getcolhabs(newcolhabs,palette,nlevels(fac_o))
  names(colhabs)<-levels(fac_o)
  colhabs<-colhabs[levels(somprev)]
  boxout<- boxplot(
    res[, 2] ~ somprev,
    col = colhabs,
    las = 1,
    cex = cex.lab,
    bty = "n", plot=F, range=coefic
  )


  outliers<-boxout$out

  legend = levels(somprev)

  par(mar=c(mar_b,mar_l,mar_t,mar_r),fg="gray30", cex=cex_pbox)
  boxplot(
    res[, 2] ~ somprev,
    col = colhabs,
    ylab = ylab_text,
    xlab = xlab_text,
    las = 1,
    cex = 1.1,
    outline=show_outs, border=colorspace::darken(colhabs, amount=0.5),pch=16,
    cex.lab = cex.lab,
    main=main, ylim=ylim,
    horizontal=horizontal,
    lwd=lwd,
    varwidth=varwidth

  )


  if(!is.null(srty)){
  if(srty=='vertical'){
    srty<-0
  } else{
    srty<-1
  }
  } else{
    srty<-1
    }
  #axis(if(isFALSE(horizontal)){2} else{1}, col="gray23",col.axis="gray30", cex.axis=ysize, tck=tck, las=srty)

  if(length(boxout$out)>0){
    outliers<-which(res[,2] %in% c(boxout$out))

    if(length(outliers)>0){
      outs<-rownames(res)[outliers]
      if(is.data.frame(lab_out)){
        lab_outs<-as.character(lab_out[outs,])[order(as.numeric(lab_out[outs,]))]

        text(boxout$group,boxout$out, labels=as.character(lab_outs), col=  rep(colorspace::darken(colhabs,0.5), table(factor(boxout$group, levels=levels(as.factor(as.numeric(somprev)))))), cex=cex.lab)
      }

    }
  }
  if(isTRUE(showleg)){
  ncol_legend<-ceiling(length(legend)/10)
  colreq<-getcolhabs(newcolhabs,palette,2)
  if(colreq[1]!=colreq[2])
  {legend("topr",
          legend = legend,
          col = colhabs,
          pch = 15,
          bty = "n",
          cex = cex.lab, ncol=ncol_legend, xpd=T, inset=c(insetx,insety), x.intersp=0.5
  )}
  }
  plotbox<-recordPlot()
  attr(plotbox,"outliers")<-rownames(res)[outliers]
  return(plotbox)

}

getpxpy<-function(m){
  px0<-unique(m$grid$pts[,1])[1:2]
  py0<-unique(m$grid$pts[,2])[1:2]
  if(anyNA(py0)){
    py0<-c(0,py0[1])
  }
  if(anyNA(px0)){
    px0<-c(0,px0[1])
  }

  px<-diff(px0)/2
  if(length(unique(m$grid$pts[,"x"]))==2&length(unique(m$grid$pts[,"y"]))>1){
    px<-px*2
  }


  py<-diff(py0)/3
  nx=m$grid$xdim
  ny=m$grid$ydim
  rang<-range(m$grid$pts[,1])
  rang<-c(rang[1]-px,rang[2]+px)
  rang2<-range(m$grid$pts[,2])

  py2<-2*py
  rang2<-c(rang2[1]-(py2),rang2[2]+(py2))
  list(c(px,py),rang,rang2,c(nx,ny))
}
#' @export
rescale_copoints<-function(hexs,copoints,jitter=F){
  pxpy<-attr(hexs,'pxpy')
  px<-pxpy[[1]][1]
  py<-pxpy[[1]][2]
  hc<-as.character(hc_hex(hexs))
  names(hc)<-1:length(hc)
  i=names(hc)[1]
  names(hexs)<-names(hc)
  res<-lapply(names(hexs), function(i){
    res<-hexs[[i]]
    copoints_list<-split(copoints,copoints$neu)
    cop<-copoints_list[[as.character(i)]]
    if(jitter){
      cop$x_pt<-runif(length(cop$x_pt))
      cop$y_pt<-runif(length(cop$y_pt))
    }
    if(!is.null(cop)){
      newlim_x<-range(range(res$x))
      newlim_y<-range(range(res$y))
      newlim_x<-c(newlim_x[1]+.25,newlim_x[2]-.25)
      newlim_y<-  c(res$y[2]+(py/2),(res$y[3]-(py/2)))
      cop$x_pt<-scales::rescale(cop$x_pt,  newlim_x)
      cop$y_pt<-scales::rescale(cop$y_pt,   newlim_y)
      cop$hc<-hc[i]
      cop
    }

  })

  res<-data.frame(do.call(rbind,res))

  res$hc<-as.factor(res$hc)
  res

}


getcopoints<-function(m){
  pxpy<-getpxpy(m)
  rang<-pxpy[[2]]
  rang2<-pxpy[[3]]
  points_factor<-text_factor<-names(m$unit.classif)
  tt<-data.frame(as.matrix(object.distances(m)))
  which(is.na(rowSums(m$data[[1]])))
  mtemp<-m
  mtemp$data[[1]]<-na.omit(m$data[[1]])
  co<-cmdscale(object.distances(mtemp))
  rownames(co)<-rownames(mtemp$data[[1]])
  co<-data.frame(co)
  co<-co[rownames(m$data[[1]]),]
  rownames(co)<-rownames(m$data[[1]])

  copoints<-data.frame(x_pt=scales::rescale(co[,1],rang),
                       y_pt=scales::rescale(co[,2],rang2),
                       neu=m$unit.classif,
                       label=text_factor,
                       point=points_factor)



  copoints

}

gg_pcounts<-function(m){
  hex<-gethex(m)
  names(hex)<-1:length(hex)
  counts<-table(factor(m$unit.classif,1:nrow(m$grid$pts)))
  counts[counts==0]<-NA
  hex<-lapply(names(counts),function(i){
    hex[[i]]$count<-counts[[i]]
    hex[[i]]
  })
  df<-do.call(rbind,hex)
  ggplot(df)+geom_polygon(aes(x,y,group=neu,fill=count),color="white")+scale_fill_gradientn(name="",colours=heat.colors(256)[-c(256:240)])+theme_void(base_size = 12)+ coord_fixed()
}
virtualPicker<-function(id,SelectedText="IDs selected", label=NULL,choices=NULL,selected=NULL,search =T,   optionHeight='24px',styles=NULL, searchPlaceholderText="Select all  -  Search",allOptionsSelectedText="All",selectAllText="Select all",alwaysShowSelectedOptionsCount=T,multiple=T,showSelectedOptionsFirst=F,...){
  class='picker_open'


  tag_style=NULL
  if(!is.null(styles)){
    tag_style<-tags$style(
      HTML(paste(paste0(".vs-",id),".vscomp-wrapper{",styles,"}")),
      HTML(paste(paste0(".vs-",id),".vscomp-search-wrapper{",styles,"}")),
      HTML(paste(paste0(".vs-",id),".vscomp-search-container{",paste0('height:',optionHeight,";",styles),"}")),
      HTML(paste(paste0(".vs-",id),".vscomp-toggle-button{",paste0('height:',optionHeight,";",styles),"}")),
      HTML(paste(paste0(".vs-",id),".vscomp-search-input{",styles,"}"))
    )
    class=paste('picker_open',paste0("vs-",id))

  }
  div(
    tag_style,
    div(class=class,
        shinyWidgets::virtualSelectInput(
          inputId = id,
          label = label,
          optionHeight=optionHeight,
          choices = choices,
          selected=selected,
          search = search,
          keepAlwaysOpen = TRUE,
          multiple =multiple,
          hideClearButton=T,
          alwaysShowSelectedOptionsCount=alwaysShowSelectedOptionsCount,
          searchPlaceholderText=searchPlaceholderText,
          optionsSelectedText=SelectedText,
          optionSelectedText=SelectedText,
          allOptionsSelectedText=allOptionsSelectedText,
          selectAllText=selectAllText,
          showSelectedOptionsFirst=showSelectedOptionsFirst,
          ...
        )
    )
  )
}

importance_codebook<-function(m,hc=NULL,n=5,var_pie_type=c("rsquared",'top_hc','top','top_w','manual'), layer=1){
  var_pie_type=match.arg(var_pie_type,c("rsquared",'top_hc','top','top_w','manual'))

  top_indicators<-hc_imp_rel<-NULL

  codebook<-data.frame(abs(m$codes[[layer]]))
  colnames(codebook)<-colnames(m$codes[[layer]])

  n_top<-n
  if(n_top>ncol(codebook)){
    n<-n_top<-ncol(codebook)
  }
  grid<-data.frame(m$grid$pts)

  if(var_pie_type=='rsquared'){
    lis<-split(data.frame(m$data[[layer]]),m$unit.classif)

    hc_imp_rel<-impr2<-sort(sapply(colnames(m$data[[layer]]),function(i){
      as.numeric( caret::postResample(m$codes[[layer]][m$unit.classif,i],m$data[[layer]][,i])[2])
    }),decreasing=T)
    hc_imp_rel<-data.frame(R_squared=impr2)
    l_inds<-names(impr2)[1:n]
  } else  if(var_pie_type=='top_hc'){
    l_inds<-importance_som_hc(m,layer,hc,n_top)
    return(l_inds)
  } else if(var_pie_type=="top"){
    hc_imp_rel<-as.matrix(t(apply(codebook,1,function(x) x/sum(x))))
    pic<-order(colSums(hc_imp_rel),decreasing=T)[1:n_top]
    l_inds<-colnames(hc_imp_rel)[pic]
  } else if(var_pie_type=="top_w"){
    pic<-order(colSums(codebook),decreasing=T)[1:n_top]
    l_inds<-colnames(codebook)[pic]
  }
  attr(l_inds,"imp_results")<-hc_imp_rel
  attr(l_inds,"imp_layer")<-layer
  l_inds
}
add_codebook_pies<-function(p,m,hc=NULL,n=5,var_pie_type=c("rsquared",'top_hc','top','top_w','manual'),Y_palette="turbo", var_pie=F,newcolhabs=list(turbo=viridis::turbo),var_pie_transp=0.1, layer=1,pie_variables=1:2){
  var_pie_type=match.arg(var_pie_type,c("rsquared",'top_hc','top','top_w','manual'))
  if(isFALSE(var_pie)){
    return(p)
  }



  c_code<-data.frame(do.call(cbind,m$codes))


  if(var_pie_type=="manual"){
    varY<-pie_variables
    c_code<-c_code[,varY]
  } else{
    ii<-importance_codebook(m,hc,n,var_pie_type, layer)
    imp<-attr(ii,"imp_results")
    varY<-as.character(ii)
    #c_code<-data.frame(imp[,varY])
  }
  c_code<-c_code[,varY]
  c_code<-sqrt(c_code^2)
  codes2<-data.frame(id=rownames(c_code),m$grid$pts,c_code)
  df3<-reshape2::melt(codes2, c('id',"x","y"))
  colY<-c(newcolhabs[[Y_palette]] (length(varY)))
  colY<-colorspace::lighten(colY,var_pie_transp)
  df3$value2<-scales::rescale(df3$value,c(0,.45))

  df3$variable<-factor(df3$variable,levels=varY)
  p<-p +  ggforce::geom_arc_bar(data=df3,aes(x0 = x, y0 = y, r0 = 0, r = value2, amount = value,fill = variable),stat = 'pie',linewidth=0, colour=NA)+
    scale_fill_manual(name="Variables",values = colY ,drop=F)
  attr(p,"imp_results")<-attr(varY,"imp_results")
  attr(p,"imp_layer")<-attr(varY,"imp_layer")
  attr(p,"imp_vars")<-varY
  p
}


bmu_plot<-function(m,hexs,points_tomap=NULL,bp=NULL,points=T,points_size=2,points_palette="turbo",pch=16,text=F,text_factor=NULL,text_size=1.5,text_palette="turbo",bg_palette="viridis",newcolhabs= vals$newcolhabs,bgalpha=1,border="white",indicate=NULL,cex.var=1,col.text="black",col.bg.var="white",col.bg.var.alpha=.8, newdata=NULL, show_error=NULL,base_size=12,show_neucoords=T,title="", hc=NULL, plotY=F,Y_palette="turbo", property=NULL,fill_neurons=F,border_width=0.5,
                   var_pie=F,
                   var_pie_type="top",
                   var_pie_transp=0.1,
                   n_var_pie=5,
                   var_pie_layer=1,
                   pie_variables=1:2,text_repel=F,max.overlaps=10,show_legend=T,points_legend="Observations",neuron_legend=NULL,vfm_max.overlaps=10,...) {

  if(!is.factor(points_tomap$point)){
    points_tomap$point<-factor(points_tomap$point)
  }
  if(!is.factor(points_tomap$label)){
    points_tomap$label<-factor(points_tomap$label)
  }

  req(length(hexs)>0)
  hc<-do.call(c,lapply(hexs,function(x) x$group[1]))
  req(length(hc[1])>0)
  if(hc[1]=="None"){
    background_type<-NULL
  } else{
    background_type<-attr(hexs,"backtype")
  }
  grid<-data.frame(m$grid$pts)
  df<-do.call(rbind,hexs)
  pxpy<-attr(hexs,"pxpy")
  nx=pxpy[[4]][[1]]; ny=pxpy[[4]][[2]]; px=pxpy[[1]][[1]]; py=pxpy[[1]][[2]]; rang=pxpy[[2]]; rang2=pxpy[[3]];

  if(!exists("col.arrow")){
    col.arrow<-"black"
  }

  if(!is.null(background_type)&!is.null(hc)){
    if(background_type%in%"hc"){
      bg0<-bg_color<-newcolhabs[[bg_palette]](nlevels(hc))
    } else{
      bg0<-bg_color <-getcolhabs(newcolhabs,bg_palette,length(hc))
    }

  } else{

    bg0<-bg_color <-getcolhabs(newcolhabs,bg_palette,1)

  }


  bg_color<-colorspace::lighten(bg_color, bgalpha)
  if(is.null(neuron_legend)){
    leg_name<-attr(hexs,"leg_name")
  } else{
    leg_name<-neuron_legend
  }

  mybreaks<-scales::rescale(1:nx,c(min(grid$x)+px,max(grid$x)))
  mybreaksy<-scales::rescale(1:ny,c(min(grid$y),max(grid$y)))
  if(!is.null(show_error)){
    err<-show_error

    colnames(show_error)<-c("x","y","id")
    show_error$err<-colnames(err)[3]
    show_error$hc<-rownames(show_error)
  }
  if(!is.null(background_type)){
    p<-ggplot() +
      geom_polygon(data=df, mapping=aes(x=x, y=y, group=neu, fill=group), col=border,linewidth=border_width) +xlab("")+ylab("")
  } else{
    p<-ggplot() +
      geom_polygon(data=df, mapping=aes(x=x, y=y, group=neu), col=border,linewidth=border_width,fill=bg_color) +xlab("")+ylab("")
  }


  if(is.null(background_type)){
    p<-add_codebook_pies(p,m,hc=hc,n=n_var_pie,var_pie_type=var_pie_type,Y_palette=Y_palette, var_pie=var_pie,newcolhabs=newcolhabs,var_pie_transp=var_pie_transp,layer=var_pie_layer,pie_variables=pie_variables)
  }

  imp_results<-attr(p,"imp_results")
  imp_layer<-attr(p,"imp_layer")
  imp_vars<-attr(p,"imp_vars")

  if(!is.null(background_type)&!is.null(hc)&length(unique(df$group))!=1){
    if(background_type%in%"uMatrix"){
      p<- p+scale_fill_gradientn(name=leg_name,colours=bg_color)
    } else if(background_type%in%"property"){
      p<- p+scale_fill_gradientn(name=leg_name,colours=bg_color)
    } else if ( background_type%in%"hc"){
      p<-p+scale_fill_manual(name=paste0(leg_name,if(!is.null(show_error)){paste0("",colnames(err)[3])}),values=bg_color,labels=if( !is.null(show_error)){
        c(show_error$id,
          if(anyNA(df)){""})
      } else{   levels(hc) })
    }

  } else{
    if(isFALSE(var_pie))
      p<-p+guides(fill="none")
  }
  if(isTRUE(plotY)){
    codes2<-cbind(m$codes[[2]])
    rownames(codes2)<-1:nrow(codes2)
    codes2<-data.frame(id=rownames(codes2),m$grid$pts,codes2)

    df3<-reshape2::melt(codes2, c('id',"x","y"))
    df3$variable<-factor(df3$variable, levels=levels(df3$variable),labels=m$obsLevels)

    colY<-c(newcolhabs[[Y_palette]] (length(m$obsLevels)))
    df3$value2<-scales::rescale(df3$value,c(0,.45))
    p2<-p +   ggnewscale::new_scale_fill()+
      ggforce::geom_arc_bar(data=df3,aes(x0 = x, y0 = y, r0 = 0, r = value2, amount = value,fill = variable),stat = 'pie',linewidth=0, colour=NA )+
      scale_fill_manual(name=attr(m,"supervisor"),labels =  c(m$obsLevels),values = colY ,drop=F)

    p<-p2
  }





  if(isTRUE(show_legend)){
    guide="legend"
  } else{
    guide="none"
  }


  if(!is.null(points_tomap)){
    if(isTRUE(points)){
      req(length(points_tomap)>0)
      colpoints<-newcolhabs[[points_palette]](nlevels(points_tomap$point))
      namepoints<-attr(points_tomap,"namepoints")
      if(length(unique(colpoints))==1){
        points_tomap$point<-points_legend
        namepoints=""
      }

      p<- p+geom_point(data=points_tomap, aes(x_pt, y_pt,col=point), size=points_size+2, shape=pch)+
        scale_color_manual(name=namepoints,values=colpoints,guide=guide)
    }

    if(isTRUE(text)){
      req(length(points_tomap)>0)
      coltext<-newcolhabs[[text_palette]](1)
      if(isTRUE(text_repel)){
        p<- p+ggrepel::geom_text_repel(data=points_tomap, aes(x_pt, y_pt, label=label),size=text_size+3,col=coltext,max.overlaps = max.overlaps)
      } else{
        p<- p+geom_text(data=points_tomap, aes(x_pt, y_pt, label=label),size=text_size+3,col=coltext)
      }

    }

  }

  if(!is.null(indicate)) {
    validate(need(length(bp)>0,"Require variable biplot (bp)"))
    coltext<-getcolhabs(newcolhabs,col.text,1)
    colbg.var<-getcolhabs(newcolhabs,col.bg.var,1)
    colbg.var<-adjustcolor(colbg.var, col.bg.var.alpha)
    unit(1,"points")

    p<-p+ggrepel::geom_label_repel(size = cex.var+3,data=bp, aes(x,y,label=id), fill=colbg.var,colour=coltext, seed=1,max.overlaps=vfm_max.overlaps)

  }




  if(nx>1){
    p<-p+scale_x_continuous(limits=rang,breaks=mybreaks ,labels=1:nx)
  }
  p<-p+scale_y_continuous(limits=rang2,breaks=mybreaksy ,labels=1:ny)
  p<-p+ coord_fixed()
  if(isTRUE(show_neucoords)){
    p<-p+ theme_light(
      base_size=base_size
    )+theme(panel.background=element_blank())

  } else{
    p<-p+ theme_void(
      base_size=base_size
    )+theme(panel.background=element_blank())
  }

  p<-p+ggtitle(title)
  if(isTRUE(show_legend)){
    p<-p+
      guides(color = guide_legend(order=1),
             size = guide_legend(order=2),
             shape = guide_legend(order=3))
  }


  attr(p,"imp_results")<-imp_results
  attr(p,"imp_layer")<-imp_layer
  attr(p,"imp_vars")<-imp_vars
  p
}





recreate_som<-function(m,length.out,session=MockShinySession$new()){

  seed<-m$seed
  ms<-list()
  rlen=nrow(m$changes)

  seq<-    round(seq(1,rlen,length.out=length.out))





  withProgress(min=1,max=length(seq),message="Running...",session=session,{
    for( i in seq) {

      set.seed(seed)
      m2<-supersom(m$data,m$grid,
                   radius = m$radius,
                   alpha =m$alpha,
                   whatmap = m$whatmap,
                   user.weights = m$user.weights,
                   maxNA.fraction = m$maxNA.fraction,
                   dist.fcts = m$dist.fcts,
                   mode = m$mode,
                   rlen=i,
                   init=m$init,
                   normalizeDataLayers = m$normalizeDataLayers)


      ms[[length(ms)+1]]<-m2
      incProgress(1,session=session)
    }
  })


  return(ms)

}


split_vector_max_elements <- function(vec, max_length) {
  split(vec, ceiling(seq_along(vec) / max_length))
}
scatter_hexagon<-function(x, y, radius = 0.1, fill="gray") {
  # Create a scatter plot

  # Draw hexagons around each point
  for(i in 1:length(x)) {
    draw_hexagon(x[i], y[i], radius, fill[i])
  }
}
# Define draw_hexagon function
draw_hexagon<-function(x, y, radius,fill) {
  angles<-seq(0, 2*pi, length.out = 7) + pi/6  # Rotated by 90 degrees
  x_hex<-x + radius * cos(angles)
  y_hex<-y + radius * sin(angles)

  polygon(x_hex, y_hex,col=fill, border=fill)
}
plotnetwork_list2<-function(m,label=T, main="", show_points=T){
  col<-"Gray"
  mds<-lapply(m$codes,function(x){
    x<-sqrt(x^2)
    res<-m$grid$pts*apply(x,1,mean)
    # res<-m$grid$pts*apply(x,1,sum)
    res[,1]<-scales::rescale(res[,1],range(m$grid$pts[,1]))
    res[,2]<-scales::rescale(res[,2],range(m$grid$pts[,2]))
    res
  })
  matdis2<-matdis<-unit.distances(m$grid)
  dec<-decimalplaces(max(matdis2))
  matdis2<-round(matdis2,dec)
  neighs<-lapply(1:nrow(matdis2),function(i){
    which(matdis2[i,]==1)
  })


  dist<-data.frame(as.matrix(object.distances(m)))
  dist$unit<-m$unit.classif
  splited<-split(dist,dist$unit)

  mean_diss<-sapply(as.character(1:nrow(m$grid$pts)),function(i){
    x<-  splited[[i]]
    suppressWarnings(mean(unlist(x)))
  })



  cut<-cut(mean_diss,breaks=nrow(m$grid$pts))
  cols<-viridis(nlevels(cut))[cut]

  cols[   which(is.na(mean_diss))]<-"darkgray"

  res<-data.frame(do.call(rbind,mds))
  som_qualist<-errors_som(m)

  plot(res,type="n", ann=F,axes=F        )
  title(main= paste0("Interaction: ",nrow(m$changes)),"\n",
        paste("err.quant:",round(as.numeric(unlist(som_qualist$som_quality[1,1])),3)))

  for(j in 1:length(mds)){
    for(i in 1:length(neighs)) {
      nei<-neighs[[i]]
      p0<-mds[[j]][i,]
      p1<-mds[[j]][nei,]
      # points(p0[1],p0[2], pch=16, col=col[j],cex=2)
      segments(  p0[1], p0[2], p1[,1], p1[,2],col=col[j])


    }


  }


  res$unit<-NA
  res$unit[m$unit.classif]<-m$unit.classif
  req(length(res)>0)
  req(nrow(res)>0)

  points<-do.call(rbind,lapply(1:nrow(res),function(i){
    x<-res[i,]
    n<-sum(m$unit.classif%in%i)
    data.frame(x=x$x+ rnorm(n,0,0.06),
               y=x$y+ rnorm(n,0,0.06))


  }))


  if(m$grid$topo=="rectangular"){
    points(res, xlab="Distance", ylab="Distance",
           pch=15,main=main,col=cols,cex=3)
  } else{
    scatter_hexagon(res$x, res$y, radius = 0.2, fill=cols)

  }


  if(isTRUE(show_points)){
    points(points[1:2],pch=16,cex=.8)
  }
  if(isTRUE(label)){
    req(nrow(res)>0)
    text(res$x,res$y, labels=1:nrow(res), col="white")
  }
}
