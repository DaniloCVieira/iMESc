
## Copyright © 2023 [Danilo Candido Vieira]
## Licensed under the CC BY-NC-ND 4.0 license.



#' @export
decimalplaces <- function(x) {
  if ((x %% 1) != 0) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}
















#' @export
capture_log1<-function(f) {
  function(...) {
    logs <- list()
    add_log <- function(type, message) {
      new_l <- logs
      new_log <- list(
        type = type,
        message =  message)
      new_l[[length(new_l) + 1]]  <- new_log
      logs <<- new_l
    }
    res <- withCallingHandlers(
      tryCatch(f(...), error=function(e) {
        add_log("error", conditionMessage(e))
        NULL
      }), warning=function(w) {
        add_log("warning", conditionMessage(w))
        invokeRestart("muffleWarning")
      }, message = function(m) {
        add_log("message", conditionMessage(m))
        invokeRestart("muffleMessage")
      })
    list(res, logs = logs)
  }

}

#' @export
capture_log2<-function(f) {
  function(...) {
    logs <- list()
    add_log <- function(type, message) {
      new_l <- logs
      new_log <- list(
        type = type,
        message =  message)
      new_l[[length(new_l) + 1]]  <- new_log
      logs <<- new_l
    }
    res <- withCallingHandlers(
      tryCatch(f(...), error=function(e) {
        add_log("error", conditionMessage(e))
        NULL
      }), warning=function(w) {
        add_log("warning", conditionMessage(w))
        invokeRestart("muffleWarning")
      }, message = function(m) {
        add_log("message", conditionMessage(m))
        invokeRestart("muffleMessage")
      })
    result<-res
    if(is.null(result)){
      result<-FALSE
      class(result)<-"error"
    }
    logs_message<-sapply(logs,function(x) x$message)
    logs_type<-sapply(logs,function(x) x$type)
    attr(logs_message,"type")<-logs_type
    if(length(logs_message)==0){
      logs_message<-NULL
    }

    attr(result,"logs")<-logs_message
    result
  }

}

render_message<-function(messages){
  req(messages)

  mes_result<-unlist(lapply(messages,function(x){
    gsub("  |\n","",x)
  }))
  war_results<-split(mes_result,attr(messages,"type"))
  war_names<-sort(names(war_results),decreasing=T)

  lapply(war_names,function(x){
    if(x=="error"){
      div(
        style="overflow: auto; max-height: 150px; background: #e68e83",
        strong(icon("triangle-exclamation",style="color: brown"),"Errors:"),
        renderPrint({war_results[[x]]})
      )
    } else{
      div(
        class = "alert_warning",
        strong(icon("triangle-exclamation",style="color: Dark yellow3"),"Warnings:"),
        renderPrint({war_results[[x]]})
      )
    }
  })
}


render_warning<-function(...,title="Warning:",point_icon=T,icon=icon("triangle-exclamation",style="color: Dark yellow3")){
  div(style="padding: 10px",
      class = "alert_warning",
      strong(icon,title),
      if(isTRUE(point_icon)){

        lapply(...,function(x) div(span(icon("hand-point-right",style="color: gray30"),x)))
      } else{
        emgray(...)
      }

  )
}

normalize_som_whatmap<-function(m, whatmap=NULL){
  n_codes<-length(m$codes)
  code_names<-names(m$codes)

  if(is.null(whatmap)||length(whatmap)==0){
    return(NULL)
  }

  whatmap<-whatmap[!is.na(whatmap)]
  if(length(whatmap)==0){
    return(NULL)
  }

  if(is.numeric(whatmap)){
    valid<-whatmap[whatmap>=1&whatmap<=n_codes]
    if(length(valid)>0){
      return(unique(valid))
    }
    return(NULL)
  }

  whatmap<-as.character(whatmap)
  if(!is.null(code_names)&&length(code_names)==n_codes){
    valid_names<-whatmap[whatmap%in%code_names]
    if(length(valid_names)>0){
      return(unique(valid_names))
    }
  }

  numeric_whatmap<-suppressWarnings(as.integer(whatmap))
  valid_numeric<-numeric_whatmap[!is.na(numeric_whatmap)&numeric_whatmap>=1&numeric_whatmap<=n_codes]
  if(length(valid_numeric)>0){
    return(unique(valid_numeric))
  }

  NULL
}

get_distsom_list<-function(m,whatmap){
  whatmap<-normalize_som_whatmap(m,whatmap)
  if(is.null(whatmap)){
    whatmap<-seq_along(m$codes)
  }

  lapply(whatmap,function(x){
    object.distances(m,"codes",x)
  })
}

get_somdist_weighted<-function(m,whatmap=NULL,weights=NULL){
  whatmap<-normalize_som_whatmap(m,whatmap)
  if(is.null(whatmap)){
  return(object.distances(m,"codes"))
  }
  dist_list<-get_distsom_list(m,whatmap)
  if(is.null(weights)){
    weights<-m$user.weights
    pic_w<-names(m$codes)%in%whatmap
    weights<-weights[pic_w]
  }

  if(length(weights)!=length(dist_list)){
    weights<-rep(1,length(dist_list))
  }
  if(length(weights)==1){
    weights<-1
  }
  wei_mean_dist(dist_list,weights)

}

wei_mean_dist <- function(dist_list, weights) {
  weighted_dists <- mapply(function(d, w) as.matrix(d) * w, dist_list, weights, SIMPLIFY = FALSE)
  mean_dist_matrix <- Reduce(`+`, weighted_dists)
  return(as.dist(mean_dist_matrix))
}

imesc_hclutering<-function(data, k,hc_fun,hc_method,distance_metric=NULL, target="som codebook", model_name=1,whatmap=NULL,use_weights=F){
  pred=NULL
  hcut_result<-NULL
  h_message<-NULL
  h_message_type<-d_message_type<-NULL
  hc<-NULL
  if(target=="som codebook"){
    m<-attr(data,"som")[[model_name]]
  } else{
    m<-data
  }

  d_message<-NULL

  if(class(m)[1]=="kohonen"){
    codes<-do.call(cbind,m$codes)
    rownames(codes)<-1:nrow(codes)
    weights<-NULL
    if(!is.null(whatmap)){
      weights<-rep(1,length(whatmap))
      if(isTRUE(use_weights)){
        weights<-NULL
      }
    }

    d=get_somdist_weighted(m,weights=weights,whatmap)
   # d=object.distances(m,"codes")

  } else{
    if(is.null(distance_metric)){
      distance_metric="euclidean"
    }
    if(distance_metric=="bray"){
      #validate(need(anyNA(m)==F, "Missing are not allowed in Bray method. Uke the pre-processing tools to remove or make an imputation"))
    }
    dlog<-capture_log1(vegan::vegdist)(m,distance_metric)
    d<-dlog[[1]]
    d_message<-sapply(dlog$logs,function(x) x$message)
    d_message_type<-sapply(dlog$logs,function(x) x$type)
    if(length(d_message)==0){
      d_message<-NULL
    }
  }

  hc_kerror<-NULL
  if(length(d)>0){
    # number of objects: a 'dist' object (Numeric-Attribute) has no nrow
    n_obj<-if(inherits(d,"dist")) attr(d,"Size") else nrow(d)
    if(isTRUE(k>(n_obj-1))){
      hc_kerror<-paste("K must be between 1 and",n_obj-1)
    }
  }

  if(length(d)>0){
    hclog<-capture_log1(factoextra::hcut)(d,k,hc_func =hc_fun ,hc_method =hc_method  ,isdiss =T)
    hc<-hclog[[1]]
    h_message<-sapply(hclog$logs,function(x) x$message)
    h_message_type<-sapply(hclog$logs,function(x) x$type)
    hcut_result<-hc$cluster

  }



  if(!is.null(hcut_result)){
    if(inherits(m,"kohonen")){
      names(hcut_result)<-1:nrow(m$codes[[1]])
    } else {
      names(hcut_result)<-rownames(m)
    }

  }

  if(class(m)[1]=="kohonen"){
    if(!is.null(hcut_result)){

      dfcut<-data.frame(neu=names(hcut_result),hcut_result)
      list<-split(data.frame(id=names(m$unit.classif),neu=m$unit.classif),m$unit.classif)
      res<-do.call(rbind,lapply(names(list),function(i){
        x<-list[[i]]
        x$hc<- dfcut[i,"hcut_result"]
        x
      }))
      newclass<-res$hc
      names(newclass)<-rownames(res)
      pred<-newclass

    }
    } else{
    m=list()
    m[[1]]<-m
    pred=hcut_result

  }


  somC<-list(somC=pred,
             # colhabs=colhabs,
             som.model=m,som.hc=hcut_result, groups=k,  hc.object=hc)
  if(!inherits(m,"kohonen")){
    if(!is.null(somC$somC)){
      somC$somC<-factor(somC$somC,levels=unique(somC$somC))
      somC$som.hc<- somC$somC
    }

  }


  logs<-c(hc_kerror,d_message,h_message)
  attr(logs,"type")<-c(d_message_type,h_message_type)
  if(length(logs)>0)
  attr(somC,'logs')<-logs
  class(somC)<-"somC"
  return(somC)
}

cutsom_new<-function(m, k,hc_fun,hc_method,distance_metric=NULL,use_weights=F,whatmap=NULL){
  if(class(m)[1]=="kohonen"){
    codes<-do.call(cbind,m$codes)
    rownames(codes)<-1:nrow(codes)
    weights<-rep(1,length(whatmap))
    if(isTRUE(use_weights)){
      weights<-NULL
    }
    d=get_somdist_weighted(m,weights=weights,whatmap)

    hc<-factoextra::hcut(d,k,hc_func =hc_fun ,hc_method =hc_method  ,method=hc_method,isdiss =T)

    hcut<-hc$cluster
    names(hcut)<-rownames(codes)
  } else{
    if(is.null(distance_metric)){
      distance_metric="euclidean"
    }
    if(distance_metric=="bray"){
      validate(need(anyNA(m)==F, "Missing are not allowed in Bray method. Uke the pre-processing tools to remove or make an imputation"))
    }

    dlog<-capture_log1(vegan::vegdist)(m,distance_metric, na.rm=T)
    d<-dlog[[1]]
    d_message<-sapply(dlog$logs,function(x) x$message)
    if(length(d_message)==0){
      d_message<-NULL
    }

    hclog<-capture_log1(imesc_fviz_nbclust)(d,k,hc_func =hc_fun ,hc_method =hc_method  ,isdiss =T)
    hc<-hclog[[1]]
    h_message<-sapply(hclog$logs,function(x) x$message)

    hcut<-hc$cluster
    if(!is.null(hcut)){
      names(hcut)<-rownames(m)
    }

    m=list()
    m[[1]]<-m
  }


 if(class(m)[1]=="kohonen"){
   dfcut<-data.frame(neu=names(hcut),hcut)
   list<-split(data.frame(id=names(m$unit.classif),neu=m$unit.classif),m$unit.classif)
   res<-do.call(rbind,lapply(names(list),function(i){
     x<-list[[i]]
     x$hc<- dfcut[i,"hcut"]
     x
   }))
   newclass<-res$hc
   names(newclass)<-rownames(res)
   pred<-newclass
 } else{
   pred=hc$cluster
 }

  somC<-list(somC=pred,
             # colhabs=colhabs,
             som.model=m,som.hc=hcut, groups=k,  hc.object=hc)
  class(somC)<-"somC"
  return(somC)

}
#' @export
cutsom<-function(m,groups, members=NULL, method.hc="ward.D2", palette="turbo",newcolhabs=NULL, dataX,weighted=F){
  m1<-m
  codes<-do.call(cbind,m1$codes)
  rownames(codes)<-1:nrow(codes)

  weights=table(factor(m1$unit.classif, levels=1:nrow(codes)))
  #weights[weights>0]<-1
  if(weighted==T){
    members=weights
  }


  d=object.distances(m1,"codes")
  hc<-hclust(d,method=method.hc,members=members)
  hcut<-dendextend::cutree( hc, groups)
  names(hcut)<-rownames(codes)

  dfcut<-data.frame(neu=names(hcut),hcut)
  list<-split(data.frame(id=names(m1$unit.classif),neu=m1$unit.classif),m1$unit.classif)
  res<-do.call(rbind,lapply(names(list),function(i){
    x<-list[[i]]
    x$hc<- dfcut[i,"hcut"]
    x
  }))
  newclass<-res$hc
  names(newclass)<-rownames(res)
  pred<-newclass
  #colhabs<-getcolhabs(newcolhabs,palette,groups)
  col_vector2=c(colors_bmu(m1))
  res<-list(groups,pred,result=NULL, hcut )
  somC<-list(somC=as.factor(pred),
             # colhabs=colhabs,
             som.model=m1,som.hc=hcut, groups=groups, colunits=col_vector2,cluster.result=NULL, hc.object=hc)
  class(somC)<-"somC"
  return(somC)

}




#' @export
topology<-function(df,dist="BrayCurtis",topo=T) {

  pic<-which(unlist(lapply(data.frame(df),function(x) var(x)))==0)
  if(length(pic)>0){
    df<-df[,-pic]
  }

  df=data.frame(na.omit(df))
  rem<-which(colSums(df)==0)
  if(length(rem)>0){df<-df[,-rem]}

  if(length(attr(df,"scaled:scale"))<1)
  {
    df=scale(df)
  }
  N=floor(5*sqrt(nrow(df)))
  pr<-prcomp(df)
  ratio_y<-pr$x[order(pr$x, decreasing = T)[2]]/abs(pr$x[order(pr$x, decreasing = T)[1]])
  ratio_x<-pr$x[order(pr$x, decreasing = T)[1]]/pr$x[order(pr$x, decreasing = T)[2]]
  y=floor(sqrt(N/ratio_y))
  x=ceiling(sqrt(N/ratio_x))
  res<-c(units=N,x=y,y=x)
  input<-res
  units<-input[1]
  gridtopo=input[2:3]
  Ydoble<-input[3]*2
  names(gridtopo)<-NULL
  prop<-gridtopo[1]/gridtopo[2]
  res
}



#' @export
weighted.correlation <- function(v,w,grille, i=NULL)
{

  x <- grille$grid$pts[,"x"]
  y <- grille$grid$pts[,"y"]
  mx <- weighted.mean(x,w, na.rm=T)
  my <- weighted.mean(y,w, na.rm=T)
  mv <- weighted.mean(v,w, na.rm=T)
  numx <- sum(w*(x-mx)*(v-mv), na.rm=T)
  denomx <- sqrt(sum(w*(x-mx)^2, na.rm=T))*sqrt(sum(w*(v-mv)^2, na.rm=T))
  numy <- sum(w*(y-my)*(v-mv), na.rm=T)
  denomy <- sqrt(sum(w*(y-my)^2, na.rm=T))*sqrt(sum(w*(v-mv)^2, na.rm=T)) #correlation for the two axes
  res <- c(numx/denomx,numy/denomy)
  return(res)
}







#' @export
boxtext <- function(x, y, labels = NA, col.text = NULL, col.bg = NA,
                    border.bg = NA, adj = 1, pos = 4, offset = 0,
                    padding = c(0.5, 0.5), cex = 1, font = par('font')){

  ## The Character expansion factro to be used:
  theCex <- par('cex')*cex

  ## Is y provided:
  if (missing(y)) y <- x

  ## Recycle coords if necessary:
  if (length(x) != length(y)){
    lx <- length(x)
    ly <- length(y)
    if (lx > ly){
      y <- rep(y, ceiling(lx/ly))[1:lx]
    } else {
      x <- rep(x, ceiling(ly/lx))[1:ly]
    }
  }

  ## Width and height of text
  textHeight <- strheight(labels, cex = theCex, font = font)
  textWidth <- strwidth(labels, cex = theCex, font = font)

  ## Width of one character:
  charWidth <- strwidth("e", cex = theCex, font = font)

  ## Is 'adj' of length 1 or 2?
  if (!is.null(adj)){
    if (length(adj == 1)){
      if(length(adj)==2){
      adj <- c(adj[1], adj[2])} else{
        c(adj[1], 0.5)
      }
    }
  } else {
    adj <- c(0.5, 0.5)
  }

  ## Is 'pos' specified?
  if (!is.null(pos)){
    if (pos == 1){
      adj <- c(0.5, 1)
      offsetVec <- c(0, -offset*charWidth)
    } else if (pos == 2){
      adj <- c(1, 0.5)
      offsetVec <- c(-offset*charWidth, 0)
    } else if (pos == 3){
      adj <- c(0.5, 0)
      offsetVec <- c(0, offset*charWidth)
    } else if (pos == 4){
      adj <- c(0, 0.5)
      offsetVec <- c(offset*charWidth, 0)
    } else {
      stop('Invalid argument pos')
    }
  } else {
    offsetVec <- c(0, 0)
  }

  ## Padding for boxes:
  if (length(padding) == 1){
    padding <- c(padding[1], padding[1])
  }

  ## Midpoints for text:
  xMid <- x + (-adj[1] + 1/2)*textWidth + offsetVec[1]
  yMid <- y + (-adj[2] + 1/2)*textHeight + offsetVec[2]


  ## Draw rectangles:
  rectWidth <- textWidth + 2*padding[1]*charWidth
  rectHeight <- textHeight + 2*padding[2]*charWidth
  rect(xleft = xMid - rectWidth/2,
       ybottom = yMid - rectHeight/2,
       xright = xMid + rectWidth/2,
       ytop = yMid + rectHeight/2,
       col = col.bg, border = border.bg)

  ## Place the text:
  text(xMid, yMid, labels, col = col.text, cex = theCex, font = font,
       adj = c(0.5, 0.5))

  points(xMid, yMid,  pch=17)

  ## Return value:
  if (length(xMid) == 1){
    invisible(c(xMid - rectWidth/2, xMid + rectWidth/2, yMid - rectHeight/2,
                yMid + rectHeight/2))
  } else {
    invisible(cbind(xMid - rectWidth/2, xMid + rectWidth/2, yMid - rectHeight/2,
                    yMid + rectHeight/2))
  }
}


#' @export
train.summary_fun<- function(m){
  {

    traindata <- data.frame(m$data[[1]])
    mean = round(mean(unlist(traindata)), 2)
    n.obs = nrow(traindata)
    n.variables = ncol(traindata)
    summ <- m$grid[-1]
    summ$neighbourhood.fct <- as.character(summ$neighbourhood.fct)
    summ <- do.call(rbind, summ)

    mode <-attr(m,"mode")
    alpha0 = m$alpha[1]
    alpha1 =m$alpha[2]
    radius0 =m$radius[1]
    radius1 =m$radius[2]
    user.weights = m$user.weights
    maxNA.fraction = m$maxNA.fraction
    dist.fcts = m$dist.fcts[[1]]

    summ[,1]<-tolower(summ[,1])

    som_qual<-errors_som(m)

    Parameters <- rbind(
        som_qual,
        n.obs,
        n.variables,
        summ,
        alpha0,
        alpha1,
        radius0,
        radius1,

        maxNA.fraction,
        dist.fcts,
        mode

      )

    data.frame(Parameters)

  }
}

#' @export
BrayCurtis<-function(){
  maxNA.fraction<-0L
  prefab.idx<-1
  dist.fcts<-prefabDists <- c("BrayCurtis")
  dist.ptrs <- vector(length(dist.fcts), mode = "list")
  dist.ptrs[prefab.idx]<-kohonen:::CreateStdDistancePointers(factor(dist.fcts[prefab.idx],
                                                                    levels = prefabDists), maxNA.fraction > 0L)

  dist.ptrs[[prefab.idx]]
}
