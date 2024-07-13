
## Copyright © 2023 [Danilo Candido Vieira]
## Licensed under the CC BY-NC-ND 4.0 license.

#' @export
plotnetwork_list3<-function(m, palette='turbo',newcolhabs, label=T, main=""){
  col<-newcolhabs[[palette]](length(m$data))


  mds<-lapply(m$whatmap,function(x){
    data.frame(cmdscale(object.distances(m,"codes", whatmap = x)))
  })
  res_mds<-mds
  names(res_mds)<-names(m$data)



  res<-data.frame(do.call(rbind,lapply(1:nrow( res_mds[[1]]),function(i){
    distlay<-dist(do.call(rbind,lapply(res_mds, function(x){x[i,]})))
    labs<-attr(distlay,"Labels")
    distlay<-as.vector(distlay)
    names(distlay)<-labs
    distlay
  })))

  resz<-as.list(res)

  i<-names(res_mds)[1]
  names(col)<-names(res_mds)
  mds3d<-lapply(names(res_mds),function(i){
    res_mds[[i]]$z<- resz[[i]]
    res_mds[[i]]$col<-col[i]
    res_mds[[i]]
  })

  mds3d_df<-do.call(rbind,mds3d)

  xyz <- mds3d_df
  x<-mds3d_df[,1]
  y<-mds3d_df[,2]
  z<-mds3d_df[,3]
  colz<-mds3d_df[,4]

  matdis2<-matdis<-unit.distances(m$grid)
  dec<-decimalplaces(max(matdis2))
  matdis2<-round(matdis2,dec)
  neighs<-lapply(1:nrow(matdis2),function(i){
    which(matdis2[i,]==1)
  })


  lines_list<-lapply(1:length(mds3d),function(j){
    cat("\n",j)
    do.call(rbind,lapply(1:length(neighs),function(i){
      {
        cat("\n",i)
        nei<-neighs[[i]]
        p0<-mds3d[[j]][i,1:3]
        p1temp<-mds3d[[j]][nei,1:3]
        do.call(rbind,lapply(1: nrow(p1temp),function(jj){
          p1<-p1temp[jj,]
          c (x0=p0[[1]], y0=p0[[2]], z0=p0[[3]], x1 = p1[[1]], y1 = p1[[2]], p1[[3]])
        }))
      }
    }))
  })

  scatter3D(x, y, z,  colvar = NULL, col = colz, add = FALSE, pch=16)
  lapply(1:length(lines_list),function(i){
    xx<-lines_list[[i]]
    segments3D(x0=xx[,1], y0=xx[,2], z0=xx[,3], x1 = xx[,4], y1 = xx[,5], xx[,6],add=T, col=col[i])


  })





}
#' @export
dtable_som<-function (kohobj, whatmap, data=NULL, classif = NULL){

  if (is.null(classif)) {
    if (is.null(kohobj$unit.classif)) {
      stop("No classification information present")
    }    else {
      classif <- kohobj$unit.classif
    }
  }
  if (is.null(data)) {
    if (!is.null(kohobj$data)) {
      data <- kohobj$data
      if(class(data)!='list'){
        data<-list(kohobj$data)
      }
    }    else {
      stop("No data present")
    }
  }
  if (is.null(whatmap)) {
    whatmap <- kohobj$whatmap
  }  else {
    whatmap <- check.whatmap(kohobj, whatmap)
  }
  weights <- kohobj$user.weights[whatmap] * kohobj$distance.weights[whatmap]
  maxNA.fraction <- kohobj$maxNA.fraction
  distanceFunctions <- kohobj$dist.fcts[whatmap]
  dist.ptrs <- kohonen:::getDistancePointers(distanceFunctions, maxNA.fraction = maxNA.fraction)
  data <- data[whatmap]
  codes <- kohobj$codes[whatmap]
  if (any(factor.idx <- sapply(data, is.factor))){
    data[factor.idx] <- lapply(data[factor.idx], classvec2classmat)}
  nvars <- sapply(data, ncol)
  nobjects <- nrow(data[[1]])
  nNA <- kohonen:::getnNA(data, maxNA.fraction, nobjects)
  datamat <- matrix(unlist(data), ncol = nobjects, byrow = TRUE)
  codemat <- matrix(unlist(codes), ncol = nunits(kohobj), byrow = TRUE)
  units<-lapply(1:nrow(kohobj$grid$pts),function(x) rep(x,length(kohobj$unit.classif)))
  res<-lapply(1:length(units),function(i){
    u1<-units[[i]]
    kohobj_temp<- kohobj
    kohobj_temp$unit.classif<-u1
    kohobj_temp$unit.classif -  1

    d2wus<-kohonen:::LayerDistances(data = datamat, codes = codemat, uclassif = kohobj_temp$unit.classif -1, numVars = nvars, numNAs = nNA, distanceFunctions = dist.ptrs,weights = weights)
    matrix(d2wus,dimnames = list(rownames(kohobj_temp$data[[1]]),i))
  })

  res2<-do.call(cbind,res)
  result<-t(apply(res2,1,order))
  result

}

#' @export
my_check_som<-function (pkg) {
  requireNamespace("kohonen")
  current <- packageDescription("kohonen")$Version
  expected <- "3.0.0"
  if (compareVersion(current, expected) < 0)
    stop("This modeling workflow requires kohonen version ",
         expected, "or greater.", call. = FALSE)
}
#' @export
plotnetwork<-function(m, col='red', label=T){
  mds<-cmdscale(object.distances(m,"codes"))
  matdis2<-matdis<-unit.distances(m$grid)
  dec<-decimalplaces(max(matdis2))
  matdis2<-round(matdis2,dec)
  neighs<-lapply(1:nrow(matdis2),function(i){
    which(matdis2[i,]==1)
  })
  plot(mds, xlab="Distance", ylab="Distance")
  for(i in 1:length(neighs)){
    nei<-neighs[[i]]
    p0<-mds[i,]
    p1<-mds[nei,]
    points(p0[1],p0[2], pch=16, col=col)
    segments(  p0[1], p0[2], p1[,1], p1[,2])
    if(isTRUE(label)){
      text(p0[1],p0[2], i)
    }

  }
}
#' @export
decimalplaces <- function(x) {
  if ((x %% 1) != 0) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}


#' @export
plotnetwork_list<-function(m, palette='turbo',newcolhabs, label=T, main=""){
  col<-newcolhabs[[palette]](length(m$data))


  mds<-lapply(m$whatmap,function(x){
    cmdscale(object.distances(m,"codes", whatmap = x))
  })


  matdis2<-matdis<-unit.distances(m$grid)
  dec<-decimalplaces(max(matdis2))
  matdis2<-round(matdis2,dec)
  neighs<-lapply(1:nrow(matdis2),function(i){
    which(matdis2[i,]==1)
  })

  for(j in 1:length(mds))

    plot(do.call(rbind,mds), xlab="Distance", ylab="Distance", pch=16,main=main)
  for(j in 1:length(mds)){
    for(i in 1:length(neighs)) {
      nei<-neighs[[i]]
      p0<-mds[[j]][i,]
      p1<-mds[[j]][nei,]
      points(p0[1],p0[2], pch=16, col=col[j])
      segments(  p0[1], p0[2], p1[,1], p1[,2],col=col[j])
      if(isTRUE(label)){
        text(p0[1],p0[2], i)
      }

    }
  }

  legend("topl",pch=16,col=col,legend=names(m$data), bty ="n")

}



#' @export
distsom<-function(m){
  switch(m$dist.fcts,
         "BrayCurtis"={(m$codes[[1]])},
         "euclidean"={dist(m$codes[[1]])},
         "sumofsquares"={dist(m$codes[[1]])^2},
         "manhattan"={dist(m$codes[[1]],method="manhattan")}
  )
}


#' @export
somQuality4<-function (som, classes){
  if (is.null(som))
    return(NULL)
  ok.dist <- aweSOM::somDist(som)
  bmu <- som$unit.classif
  traindat<-data.frame(do.call(cbind,som$data))
  names(bmu)<-rownames(traindat)
  sqdist <- rowSums((traindat - som$codes[[1]][bmu, ])^2, na.rm = TRUE)


  err.quant <- tapply(sqdist,as.factor(classes[names(sqdist)]),function(x) mean(x,na.rm=T))
  tot_vars<-rowSums(t(t(traindat) - colMeans(traindat,
                                             na.rm = TRUE))^2, na.rm = TRUE)
  totalvar <- tapply(tot_vars,as.factor(classes[names(sqdist)]),mean)


  err.varratio <- lapply(1:length(totalvar),function(x){
    100 - round(100 * err.quant[[x]]/totalvar[[x]], 2)
  })
  bmu2 <- apply(traindat, 1, function(row) {
    dist <- colMeans((t(som$codes[[1]]) - row)^2, na.rm = TRUE)
    order(dist)[2]
  })


  err.topo <-   tapply(!ok.dist$neigh.matrix[cbind(bmu, bmu2)],as.factor(classes[names(sqdist)]),mean)

  err.kaski <- e1071::allShortestPaths(ok.dist$proto.data.dist.neigh)$length[cbind(bmu,
                                                                                   bmu2)]

  err.kaski <-  tapply(err.kaski + sqrt(sqdist),as.factor(classes),mean)


  res4<-mapply(c,err.quant = err.quant, err.varratio = unlist(err.varratio),
               err.topo = err.topo, err.kaski = err.kaski, SIMPLIFY = F)
  do.call(rbind,res4)

}



#' @export
som_dist<-function (som) {

  if (is.null(som))
    return(NULL)
  proto.gridspace.dist <- kohonen::unit.distances(som$grid,
                                                  F)
  proto.dataspace.dist <- as.matrix(object.distances(som,"codes"))
  neigh <- round(proto.gridspace.dist, 3) == 1
  proto.dataspace.dist.neigh <- proto.dataspace.dist
  proto.dataspace.dist.neigh[!neigh] <- NA
  list(proto.grid.dist = proto.gridspace.dist, neigh.matrix = neigh,
       proto.data.dist = proto.dataspace.dist, proto.data.dist.neigh = proto.dataspace.dist.neigh)
}



#' @export
match_col<-function(x,ref){
  sum(colnames(x)%in%colnames(ref))==ncol(x)
}
#' @export
cutdata1<-function(data,  method.hc="ward.D2", dist="bray", col=NULL){
  #validate(need(anyNA(data)==F, "The selected data contains missing values. Please remove them in Upload>Transform."))
  if(dist=="bray"){
    validate(need(anyNA(data)==F, "Missing are not allowed in Bray method. Uke the pre-processing tools to remove or make an imputation"))
  }


  dlog<-capture_log1(vegdist)(data, method=dist, na.rm=T)
  d<-dlog[[1]]
  d_message<-sapply(dlog$logs,function(x) x$message)
  if(length(d_message)==0){
    d_message<-NULL
  }
  hclog<-capture_log1(hclust)(d,method=method.hc)
  hc<-hclog[[1]]
  h_message<-sapply(hclog$logs,function(x) x$message)
  if(length(h_message)==0){
    h_message<-NULL
  }
  if(is.null(hc)){
    hc<-FALSE
  }
  attr(hc,"logs")<-c(d_message,h_message)
  return(hc)
}


cutdata_back<-function(data,  method.hc="ward.D2", dist="bray", col=NULL){
  #validate(need(anyNA(data)==F, "The selected data contains missing values. Please remove them in Upload>Transform."))
  if(dist=="bray"){
    validate(need(anyNA(data)==F, "Missing are not allowed in Bray method. Uke the pre-processing tools to remove or make an imputation"))
  }



  d<-vegdist(data, method=dist, na.rm=T)
  hc<-hclust(d,method=method.hc)


  # req(!inherits(hc,"try-error"))

  hc


  return(hc)
}
#' @export
cutm<-function(m,  method.hc="ward.D2"){
  codes<-kohonen::getCodes(m)
  d=object.distances(m,"codes")
  hc<-hclust(d,method=method.hc)
  return(hc)
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




imesc_hclutering<-function(data, k,hc_fun,hc_method,distance_metric=NULL, target="som codebook", model_name=1){
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
    d=object.distances(m,"codes")

  } else{
    if(is.null(distance_metric)){
      distance_metric="euclidean"
    }
    if(distance_metric=="bray"){
      #validate(need(anyNA(m)==F, "Missing are not allowed in Bray method. Uke the pre-processing tools to remove or make an imputation"))
    }
    dlog<-capture_log1(vegdist)(m,distance_metric)
    d<-dlog[[1]]
    d_message<-sapply(dlog$logs,function(x) x$message)
    d_message_type<-sapply(dlog$logs,function(x) x$type)
    if(length(d_message)==0){
      d_message<-NULL
    }
  }

  hc_kerror<-NULL
  if(length(d)>0){
    if(k>(nrow(d)-1)){
      hc_kerror<-paste("K must be between 1 and",nrow(d)-1)
    }
  }

  if(length(d)>0){
    hclog<-capture_log1(hcut)(d,k,hc_func =hc_fun ,hc_method =hc_method  ,isdiss =T)
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

cutsom_new<-function(m, k,hc_fun,hc_method,distance_metric=NULL){
  if(class(m)[1]=="kohonen"){
    codes<-do.call(cbind,m$codes)
    rownames(codes)<-1:nrow(codes)
    d=object.distances(m,"codes")

    hc<-hcut(d,k,hc_func =hc_fun ,hc_method =hc_method  ,method=hc_method,isdiss =T)
    hcut<-hc$cluster
    names(hcut)<-rownames(codes)
  } else{
    if(is.null(distance_metric)){
      distance_metric="euclidean"
    }
    if(distance_metric=="bray"){
      validate(need(anyNA(m)==F, "Missing are not allowed in Bray method. Uke the pre-processing tools to remove or make an imputation"))
    }

    dlog<-capture_log1(vegdist)(m,distance_metric, na.rm=T)
    d<-dlog[[1]]
    d_message<-sapply(dlog$logs,function(x) x$message)
    if(length(d_message)==0){
      d_message<-NULL
    }

    hclog<-capture_log1(hcut)(d,k,hc_func =hc_fun ,hc_method =hc_method  ,isdiss =T)
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
  hcut<-cutree( hc, groups)
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
rect.dendrogram3<-function (tree, k = NULL,  horiz = FALSE,upper_rect = 0, prop_k_height = 0.5) {


  tree_heights <- heights_per_k.dendrogram(tree)[-1]
  tree_order <- order.dendrogram(tree)

  cluster <- cutree(tree, k = k)
  clustab <- table(cluster)[unique(cluster[tree_order])]
  m <- c(0, cumsum(clustab))
  which <- 1L:k

  retval <- list()
  result<-list()
  for (n in seq_along(which)) {
    next_k_height <- tree_heights[names(tree_heights) ==
                                    k + 1]
    if (length(next_k_height) == 0) {
      next_k_height <- 0
      prop_k_height <- 1
    }
    if (!horiz) {


      ytop <- tree_heights[names(tree_heights) == k] *
        prop_k_height + next_k_height * (1 - prop_k_height) +
        upper_rect
    }    else {

      ytop <- m[which[n] + 1] + 0.33

    }
    result[[n]]<- ytop

  }
  res<-data.frame(do.call(rbind,result))
  colnames(res)<-c('ytop')
  res
}
#' @export
ggplot.ggdend2<-function (data = NULL, mapping = aes(), ..., segments = TRUE,labels = TRUE, nodes = TRUE, horiz = FALSE, theme = theme_dendro(),
                          offset_labels = 0, na.rm = TRUE, environment = parent.frame(),lbls, col,legend=c("outside","inside"),dend1,text_size=5, lwd=0.5) {
  legend<-match.arg(legend,c("outside","inside"))

  data <- as.ggdend(dend1)
  data <- prepare.ggdend(data)
  angle <- ifelse(horiz, 0, 90)
  hjust <- ifelse(horiz, 0, 1)
  p <- ggplot(mapping = mapping)###...
  col_sequence<-col
  names(col_sequence)<-c(as.character(lbls))
  if (segments) {
    p<- p + geom_segment(data = data$segments, na.rm = na.rm,
                         aes(x = x, y = y, xend = xend, yend = yend,
                             colour = col, linetype = lty),linewidth = lwd,
                         lineend = "square")

    if(legend=="outside"){
      p<-p+
        scale_colour_identity(name="Clusters",guide="legend",breaks=col, labels=levels(lbls))
    } else{
      p<-p+
        scale_colour_identity()
    }

    p<-p+scale_size_identity() +
      scale_linetype_identity()
  }

  if (labels) {
    data$labels$cex <- 5 * data$labels$cex
    data$labels$y <- data$labels$y + offset_labels
    p <- p + geom_text(data = data$labels, aes(x = x,
                                               y = y, label = label, colour = col, size = text_size),
                       hjust = hjust, angle = angle, show.legend = FALSE)
  }
  if (horiz) {
    p <- p + coord_flip() + scale_y_reverse(expand = c(0.2,0))
  }
  if (!is.null(theme)) {
    p <- p + theme
  }
  p
}
get_dend_labposition<-function(dend,groups){

  segments<-as.ggdend(dend)$segments
  dhei<-rect.dendrogram3(dend,k=groups)
  dhei$group<-rownames(dhei)
  tre<-segments[segments$y!=segments$yend,]
  tre2<-tre[tre$y>max(dhei$ytop),]
  dhei$x<-tre2[!is.na(tre2$col),"x"]
  dhei
}
hc_plot<-function(somC, col=NULL, labels=NULL, lwd=2, main="", xlab="Observations", ylab="Height", base_size=12, theme='theme_grey',offset_labels=-.1,xlab_adj=20, legend=c("outside","inside")){
  legend<-match.arg(legend,c("outside","inside"))
  groups<-somC$groups
  opar<-par(no.readonly=TRUE)
  hc <- as.hclust(as.dendrogram(somC$hc.object))
  hc.dendo <- as.dendrogram(somC$hc.object)
  my_5_cluster <-somC$som.hc
  clust.cutree <- cutree(hc.dendo, k=groups, order_clusters_as_data = FALSE)
  clust.cutree<-factor(clust.cutree,levels=levels(somC$somC),labels=levels(somC$somC))
  idx <- order(as.vector(names(clust.cutree)))
  clust.cutree <- clust.cutree[idx]
  df.merge <- merge(my_5_cluster,clust.cutree,by='row.names')
  df.merge.sorted <- df.merge[order(df.merge$y),]
  lbls<-unique(df.merge.sorted$x)
  if(is.null(col)){ color=somC$colhabs[lbls]
  } else {color=col[lbls]}
  dend1 <- color_branches(dend=hc.dendo, col=color,k = groups, groupLabels = lbls)
  if(is.null(col)){
    colors_dend1<- somC$colhabs[my_5_cluster[labels(dend1)]]
  } else {colors_dend1="black" }

  if(!is.null(labels)){
    lab_dend<-labels(dend1)
    names(labels)<-names(somC[[1]])
    labels(dend1)<-labels[labels(dend1)]
  }
  labels_colors(dend1)<-colors_dend1
  dend1<-dendextend::highlight_branches_lwd(dend1, lwd)


  p<-ggplot.ggdend2(dend1,theme=NULL, offset_labels  = offset_labels, labels=T,col=col,lbls=lbls,legend=legend,text_size=base_size*.3,dend1=dend1,lwd=lwd)+xlab(xlab)+ylab(ylab)+ggtitle(main)
  if(legend!="outside"){
    try({
      dhei<-get_dend_labposition(dend1,groups)
      dhei$group<-lbls
      p<-p+geom_label(aes(x,y=ytop, label=group),data=dhei, show.legend = F,
                      size=base_size*.4,
                      color=unique(get_leaves_branches_col(dend1)))
    },silent =T)


  }


  p<-switch(theme,
            'theme_grey'={p+theme_grey(base_size)},
            'theme_bw'={p+theme_bw(base_size)},
            'theme_linedraw'={p+theme_linedraw(base_size)},
            'theme_light'={p+theme_light(base_size)},
            'theme_dark'={p+theme_dark(base_size)},
            'theme_minimal'={p+theme_minimal(base_size)},
            'theme_classic'={p+theme_classic(base_size)},
            'theme_void'={p+theme_void(base_size)})
  lab_dend<-labels(dend1)
  if(theme=='theme_minimal'){
    p<-p+theme(panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               axis.line.y=element_line()
    )
  }
  p<-p+theme(axis.text.x=element_blank(),

             axis.title.x = element_text(margin = ggplot2::margin(t =xlab_adj)))
  p<-p + coord_cartesian(clip = 'off', ylim=c((offset_labels*.9999)/2.5,max(hc$height)*1.05), expand = FALSE, xlim=c(0,length(lab_dend)+1))+theme(plot.margin = unit(c(1,1,1,1), "cm"))+
    scale_x_discrete(labels=lab_dend,limits=lab_dend)
  p

}


#' @export
Kdata<-function(x){
  res<-ceiling((5*sqrt(nrow(x)))/3)
  if(res>15){res<-15}
  res
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
s_bmu<-function(m)
{
  fun_xy <- function(x, y){
    R <- (x+1)/2
    G <- (1-x)/2
    B <- (y+1)/2
    A <- 1- 0.5*exp(-(x^2+y^2)/0.2)
    rgb(R, G, B, A)}
  z <- outer(seq(-1,1,length=m$grid$xdim), seq(-1,1,length=m$grid$ydim), FUN = fun_xy)
  return(z)
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
brayCurtisDissim<-function(data, codes, n, nNA) {
  if (nNA > 0) return(NA)
  num <- 0.0
  denom <- 0.0
  for (i in seq_len(n)) {
    num <- num + abs(data[i] - codes[i])
    denom <- denom + data[i] + codes[i]
  }

  return(num/denom)
}
#' @export
BrayCurtis<-function() {
  return(brayCurtisDissim)
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
