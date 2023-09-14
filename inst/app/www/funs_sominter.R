
## Copyright © 2023 [Danilo Candido Vieira]
## Licensed under the CC BY-NC-ND 4.0 license.
##
kohonen_predict<-function(m, newdata=NULL, whatmap = NULL,default=F){
  newdata3<-if(!is.null(whatmap)){
    #newdata<-args$newdata
    newdata2=list(as.matrix(newdata))
    names(newdata2)<-whatmap
    newdata2
  } else{
    as.matrix(newdata)
  }

  if(length(m$data)==1){
    whatmap=NULL
    pred0<-pred <-   kohonen:::predict.kohonen(m, as.matrix(newdata),whatmap=whatmap)
  } else{
    pred0<-pred <- kohonen:::predict.kohonen(m, newdata3,whatmap=whatmap)

  }


  if(isFALSE(default)){
    return(pred0)
  }




  map0<-kohonen::map(m, newdata3,whatmap=whatmap)


  if(is.null(newdata)){
    newdata<-m$data

  } else{
    if(!is.null(whatmap)){
      newdata=as.matrix(newdata)
    }

  }

  dtable_train<-dtable_som(m,whatmap)
  mtemp<-m
  #
  whatmap2<-NULL
  if(!is.null(whatmap)){
    mtemp$codes<-mtemp$codes[whatmap]
    mtemp$data<-mtemp$data[whatmap]
    mtemp$distances<-map0$distances
    mtemp$data[[1]]<-newdata
    names(mtemp$data)<-whatmap
    mtemp$unit.classif<-map0$unit.classif

    whatmap
    whatmap2<-1
  }

  i=j=1
  nmax<-length(which(is.na(rowSums(pred$predictions[[1]]))))*length(m$data)

  dtable_new<-dtable_som(mtemp,whatmap=whatmap2)


  if(any(unlist(lapply(pred$predictions, function(x) anyNA(x))))) {
    for(j in 1:length(pred$predictions)){
      newpred<-pred$predictions[[j]]

      for(i in 2:nrow(m$grid$pts)){
        pic<-which(is.na(rowSums(newpred)))

        pic2<-which(is.na(rowSums(pred$unit.predictions[[j]])))
        temp_pred<-predict.kohonen2(m, newdata, which_best=i,dtable_train=dtable_train,dtable_new=dtable_new,whatmap=whatmap)
        temp_unit<-temp_pred$unit.predictions[[j]]
        temp_pred<-temp_pred$predictions[[j]]
        newpred[pic,]<-temp_pred[pic,]
        pred$predictions[[j]]<-newpred
        pred$unit.predictions[[j]][pic2,]<-temp_unit[pic2,]
        #incProgress(nmax-length(which(is.na(rowSums(pred$predictions[[1]])))), message=paste0("Solving layer",j))

        if(isFALSE(anyNA(rowSums( pred$predictions[[j]])))){break()}
      }
      if(isFALSE(any(unlist(lapply(pred$predictions, function(x) anyNA(x)))))){break()}
    }
  }


  pred
}

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


kohonen_predict2<-function (m, newdata, whatmap = NULL,default=F) {
  pred0<-pred <- kohonen:::predict.kohonen(m, newdata,whatmap=whatmap)
  if(isFALSE(default)){

    return(pred0)
  }
  dtable_train<-dtable_som(m,whatmap=whatmap)
  mtemp<-m
  mtemp$data<-newdata
  pred <-pre0<- kohonen:::predict.kohonen(m, newdata)
  pred$maxNA.fraction<-m$maxNA.fraction
  names(pred)[which(names(pred)=="predictions")]<-"data"
  names(pred)[which(names(pred)=="unit.predictions")]<-"codes"

  mtemp$data=pred$data
  mtemp$codes=pred$codes
  mtemp$unit.classif=pred$unit.classif
  dtable_new<-dtable_som(mtemp,whatmap=whatmap)
  i=j=1
  nmax<-length(which(is.na(rowSums(pred$data[[1]]))))*length(m$data)


  withProgress(min=0,max=nmax,message="Correcting predictions",{
    if(any(unlist(lapply(pred$data, function(x) anyNA(x))))) {
      for(j in 1:length(pred$data)) {
        newpred<-pred$data[[j]]

        for(i in 2:nrow(m$grid$pts)){
          pic<-which(is.na(rowSums(newpred)))

          pic2<-which(is.na(rowSums(pred$codes[[j]])))
          temp_pred<-predict.kohonen2(m, newdata, which_best=i,dtable_train=dtable_train,dtable_new=dtable_new,whatmap=whatmap)
          temp_unit<-temp_pred$unit.predictions[[j]]
          temp_pred<-temp_pred$predictions[[j]]
          newpred[pic,]<-temp_pred[pic,]
          pred$data[[j]]<-newpred
          pred$codes[[j]][pic2,]<-temp_unit[pic2,]

          incProgress(nmax-length(which(is.na(rowSums(pred$data[[1]])))), message=paste0("Solving layer",j))



          if(isFALSE(anyNA(rowSums( pred$data[[j]])))){break()}
        }
        if(isFALSE(any(unlist(lapply(pred$data, function(x) anyNA(x)))))){break()}
      }
    }
  })

  pred
}


ko_predict<-function (modelFit, newdata, submodels = NULL) {
  if(modelFit$problemType=="Regression"){
    pred <- predict(modelFit, list(X = as.matrix(newdata)), whatmap = "X")$predictions$Y
    pred

  } else{
    dtable_train<-dtable_som(modelFit)
    mtemp<-modelFit
    mtemp$data<-list(X=as.matrix(newdata))
    mtemp$codes<-mtemp$codes[1]
    dtable_new<-dtable_som(mtemp,whatmap=1)
    pred <- kohonen:::predict.kohonen(modelFit, list(X = as.matrix(newdata)), whatmap = "X")$predictions$Y
    if(anyNA(pred)){
      newpred<-pred
      for(i in 1:nrow(modelFit$grid$pts)){
        pic<-which(is.na(pred))
        temp_pred<-predict.kohonen2(modelFit, list(X = as.matrix(newdata)), whatmap = 1,which_best=i,dtable_train=dtable_train,dtable_new=dtable_new)
        newpred[pic]<-as.character(temp_pred$predictions$Y[pic])
        if(isFALSE(anyNA(newpred))){break()}
      }
      pred<-newpred

    }
    if (is.factor(pred)) {
      pred <- as.character(pred)
    }
    pred
  }


}
#kohobj<-mtemp
#data=newdata
dtable_som<-function (kohobj, whatmap, data=NULL, classif = NULL)
{

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

predict.kohonen2<-function (object, newdata = NULL, unit.predictions = NULL, trainingdata = NULL,whatmap = NULL, threshold = 0, maxNA.fraction = object$maxNA.fraction,which_best=2,dtable_train,dtable_new,...) {
  codeNcols <- sapply(object$codes, ncol)
  ncodes <- nrow(object$codes[[object$whatmap[1]]])
  if (is.null(whatmap)) {
    whatmap <- object$whatmap
  }  else {
    whatmap <- my_check.whatmap(object, whatmap)
  }
  if (!is.null(unit.predictions)) {
    if (!is.list(unit.predictions))
      unit.predictions <- list(unit.predictions)
    if (any(isFactorUnPred <- sapply(unit.predictions, is.factor)))
      unit.predictions[isFactorUnPred] <- lapply(unit.predictions[isFactorPred],
                                                 classvec2classmat)
  }  else {
    if (is.null(trainingdata)) {
      trainingdata <- object$data
    }    else {
      if (is.null(trainingdata))
        stop("Missing trainingdata argument, no data available in kohonen object either")
      trainingdata <- my_check.data(trainingdata)
    }
    whatmap.tr <- my_check.whatmap(trainingdata, whatmap)
    if (!all(whatmap.tr == whatmap))
      stop("Data layer mismatch between map and trainingdata")
    narows <- my_check.data.na(trainingdata[whatmap], maxNA.fraction)
    trainingdata <- my_remove.data.na(trainingdata, narows)
    if (any(is.null(object$codes[whatmap])))
      stop("Attempt to map training data on the basis of data layers not present in the SOM")
    if (!my_checkListVariables(trainingdata[whatmap], codeNcols[whatmap]))
      stop("Number of columns of trainingdata do not match ",
           "codebook vectors")
    object$data <- trainingdata

    mappingX<-dtable_train[,which_best]

    unit.predictions.tmp <- lapply(trainingdata, function(trd) {
      aggregate(x = trd,by = list(mappingX), FUN = mean, drop = FALSE)
    })


    unit.predictions <- lapply(unit.predictions.tmp, function(x) {
      varnames <- colnames(x)[-1]
      x <- as.matrix(x)
      res <- matrix(NA, ncodes, ncol(x) - 1)
      colnames(res) <- varnames
      res[x[, 1], ] <- x[, -1, drop = FALSE]
      res
    })

  }
  if (is.null(newdata)) {
    newdata <- object$data
    if (is.null(newdata))
      stop("Missing newdata argument, no data available in kohonen object either")
    newrownames <- rownames(newdata[[1]])
    if (is.null(newrownames))
      newrownames <- 1:nrow(newdata[[1]])
    nnewrows <- length(newrownames)
    newmapping <- object$unit.classif
    if (any(factorNew <- sapply(newdata, is.factor)))
      newdata[factorNew] <- lapply(newdata[factorNew],
                                   classvec2classmat)
  }  else {
    newdata <- my_check.data(newdata)
    if (is.null(newnames <- names(newdata))) {
      whatmap.new <- whatmap
    }    else {
      whatmap.new <- intersect(newnames, names(object$codes)[whatmap])
      dummy <- my_check.whatmap(newdata, whatmap.new)
    }
    if (!my_checkListVariables(newdata[whatmap.new], codeNcols[whatmap.new]))
      stop("Number of columns of newdata do not match codebook vectors")
    newrownames <- rownames(newdata[[1]])
    if (is.null(newrownames))
      newrownames <- 1:nrow(newdata[[1]])
    nnewrows <- length(newrownames)
    narows <- my_check.data.na(newdata[whatmap.new], maxNA.fraction = maxNA.fraction)
    newdata <- my_remove.data.na(newdata, narows)
    newmapping<-dtable_new[,which_best]


  }
  if (length(narows) > 0) {
    rowidx <- (1:nnewrows)[-narows]
  }  else {
    rowidx <- 1:nnewrows
  }
  nonNA <- rowidx[which(!is.na(newmapping))]
  predictions <- lapply(unit.predictions, function(x) {
    pred <- matrix(NA, nnewrows, ncol(x))
    pred[nonNA, ] <- x[newmapping[nonNA], , drop = FALSE]
    pred
  })
  for (i in seq(along = predictions)) dimnames(predictions[[i]]) <- list(newrownames,
                                                                         colnames(unit.predictions[[i]]))
  if (any(isFactorPred <- sapply(predictions, is.factor.matrix)))
    predictions[isFactorPred] <- lapply(predictions[isFactorPred],
                                        classmat2classvec, threshold = threshold)
  list(predictions = predictions, unit.classif = newmapping,
       unit.predictions = unit.predictions, whatmap = whatmap)
}

my_check_som<-function (pkg) {
  requireNamespace("kohonen")
  current <- packageDescription("kohonen")$Version
  expected <- "3.0.0"
  if (compareVersion(current, expected) < 0)
    stop("This modeling workflow requires kohonen version ",
         expected, "or greater.", call. = FALSE)
}


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



plotnetwork_list2<-function(m, palette='turbo',newcolhabs, label=T, main=""){
  col<-newcolhabs[[palette]](length(m$data))
  mds<-lapply(m$whatmap,function(x){
    res<-data.frame(cmdscale(object.distances(m,"codes", whatmap = x)))
    res$fac<-paste0(names(m$data)[x])
    res
  })
  names(mds)<-names(m$data)
  res<-do.call(rbind,mds)
  mds2<-data.frame(cmdscale(dist(scale(res[,1:2]))))
  mds<-split(mds2,res$fac)
  matdis2<-matdis<-unit.distances(m$grid)
  dec<-decimalplaces(max(matdis2))
  matdis2<-round(matdis2,dec)
  neighs<-lapply(1:nrow(matdis2),function(i){
    which(matdis2[i,]==1)
  })
  mds<-mds[names(m$data)]

  plot(do.call(rbind,mds), xlab="Distance", ylab="Distance", pch=16, main=main)
  for(j in 1:length(mds)){
    for(i in 1:length(neighs)) {
      nei<-neighs[[i]]
      p0<-mds[[j]][i,]
      p1<-mds[[j]][nei,]
      points(p0[1],p0[2], pch=16, col=col[j])
      segments( unlist( p0[1]), unlist(p0[2]), p1[,1], p1[,2],col=col[j])
      if(isTRUE(label)){
        text(p0[1],p0[2], i)
      }

    }
  }

  legend("topl",pch=16,col=col,legend=names(mds), bty ="n")

}

"geosom" <- function(data, coords, k, norm, ...) {
  # subset spatial data
  geodata = data.matrix(data[,(names(data) %in% coords)])
  # subset non-spatial data
  data = data.matrix(data[,!(names(data) %in% coords)])
  spatial.weight = c(1, k)
  # if data to be normalised, we do it here
  # xyz cannot be scaled disproportionately
  if (isTRUE(norm)) {
    data = scale(data)
  }
  # disregard geography if spatial weight is zero
  if (k==0) {
    supersom(list(data), normalizeDataLayers=FALSE, ...)
  } else {
    supersom(list(data, geodata), user.weights=spatial.weight, normalizeDataLayers=FALSE,...)
  }
}

remove_var0<-function(data){
  pic<- which(apply(data,2,var,na.rm=T)==0)
  if(length(pic)>0){
    attrs<-attributes(data)
    data<-data[,-pic,drop=F]
    attrs$row.names<-rownames(data)
    attrs$names<-colnames(data)
    for(i in names(attrs)){
      attr(data,i)<-attrs[[i]]
    }
  }
  data
}


#' @export
distsom<-function(m){
  switch(m$dist.fcts,
         "BrayCurtis"={vegdist(m$codes[[1]])},
         "euclidean"={dist(m$codes[[1]])},
         "sumofsquares"={dist(m$codes[[1]])^2},
         "manhattan"={dist(m$codes[[1]],method="manhattan")}
  )
}
#' @export
som_quality_class<-function(m,newdata,somC){
  #somC<-readRDS("somC_araca.rds")
  #newdata<-vals$saved_data[["nema_c1_mean"]]
  neu_hc<-as.factor(somC$som.hc)
  obs_hc<-somC$somC
  i=2
  lev_hc<-levels(neu_hc)
  res<-do.call(rbind,lapply(1:somC$groups, function(i){
    som_temp<-m
    ob<-which(obs_hc==lev_hc[i])
    ne<-which(neu_hc==lev_hc[i])
    som_temp$grid[[1]]<-m$grid[[1]][ne,]
    som_temp$unit.classif<-m$unit.classif[ob]
    som_temp$codes[[1]]<-som_temp$codes[[1]][ne,]
    names(som_temp$unit.classif)<-paste0("V",som_temp$unit.classif)
    traindat=newdata[ob,]
    som<-som_temp

    somQuality2(som, traindat )
  }))
  rownames(res)<-as.factor(levels(somC$som.hc))
  res

}

#' @export
somQuality2<-function (som, traindat) {
  if (is.null(som))
    return(NULL)
  ok.dist <- som_dist2(som)

  bmu <- som$unit.classif
  dim(som$codes[[1]])
  sqdist <- rowSums((traindat - som$codes[[1]][names(bmu), ])^2, na.rm = TRUE)
  err.quant <- mean(sqdist)
  totalvar <- mean(rowSums(t(t(traindat) - colMeans(traindat,
                                                    na.rm = TRUE))^2, na.rm = TRUE))
  err.varratio <- 100 - round(100 * err.quant/totalvar, 2)

  for(i in 1:4){
    rownames(ok.dist[[i]])<-colnames(ok.dist[[i]])<-  rownames(som$codes[[1]])
  }

  row=1
  bmu2 <- apply(traindat, 1, function(row) {

    dist <- colMeans((t(som$codes[[1]]) - row)^2, na.rm = TRUE)
    names(dist)[order(dist)[2]]

  })


  err.topo <- mean(!ok.dist$neigh.matrix[cbind(names(bmu), bmu2)])

  reskaski<-e1071::allShortestPaths(ok.dist$proto.data.dist.neigh)
  for(i in 1:length(reskaski)){
    rownames(reskaski[[i]])<-colnames(reskaski[[i]])<-rownames(som$codes[[1]])
  }

  err.kaski <- reskaski$length[cbind(names(bmu),
                                     bmu2)]
  err.kaski <- mean(err.kaski + sqrt(sqdist))
  cellpop <- table(factor(som$unit.classif, levels = 1:nrow(som$grid$pts)))
  res <- c(err.quant = err.quant, err.varratio = err.varratio,
           err.topo = err.topo, err.kaski = err.kaski, cellpop = cellpop)
  res4<-mapply(c,err.quant = err.quant, err.varratio = err.varratio,
               err.topo = err.topo, err.kaski = err.kaski, SIMPLIFY = F)

  res4[[1]]

}
#' @export
somQuality3<-function (som, traindat, somC)
{
  if (is.null(som))
    return(NULL)
  ok.dist <- somDist(som)
  bmu <- som$unit.classif
  sqdist <- rowSums((traindat - som$codes[[1]][bmu, ])^2, na.rm = TRUE)


  err.quant <- tapply(sqdist,as.factor(somC$somC[names(sqdist)]),mean)
  tot_vars<-rowSums(t(t(traindat) - colMeans(traindat,
                                             na.rm = TRUE))^2, na.rm = TRUE)
  totalvar <- tapply(tot_vars,as.factor(somC$somC[names(sqdist)]),mean)


  err.varratio <- lapply(1:length(totalvar),function(x){
    100 - round(100 * err.quant[[x]]/totalvar[[x]], 2)
  })
  bmu2 <- apply(traindat, 1, function(row) {
    dist <- colMeans((t(som$codes[[1]]) - row)^2, na.rm = TRUE)
    order(dist)[2]
  })


  err.topo <-   tapply(!ok.dist$neigh.matrix[cbind(bmu, bmu2)],as.factor(somC$somC[names(sqdist)]),mean)

  err.kaski <- e1071::allShortestPaths(ok.dist$proto.data.dist.neigh)$length[cbind(bmu,
                                                                                   bmu2)]

  err.kaski <-  tapply(err.kaski + sqrt(sqdist),as.factor(somC$somC),mean)


  res4<-mapply(c,err.quant = err.quant, err.varratio = unlist(err.varratio),
               err.topo = err.topo, err.kaski = err.kaski, SIMPLIFY = F)
  do.call(rbind,res4)

}

somQuality4<-function (som, classes)
{
  if (is.null(som))
    return(NULL)
  ok.dist <- somDist(som)
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

supersomQuality<-function (som)
{
  #it uses cbind
  codes<-do.call(cbind,som$codes)
  traindat<-do.call(cbind,som$data)
  if (is.null(som))
    return(NULL)
  ok.dist <- somDist(som)
  bmu <- som$unit.classif
  sqdist <- rowSums((traindat - codes[bmu, ])^2,
                    na.rm = TRUE)
  err.quant <- mean(sqdist)
  totalvar <- mean(rowSums(t(t(traindat) - colMeans(traindat,
                                                    na.rm = TRUE))^2, na.rm = TRUE))
  err.varratio <- 100 - round(100 * err.quant/totalvar, 2)
  bmu2 <- apply(traindat, 1, function(row) {
    dist <- colMeans((t(codes) - row)^2, na.rm = TRUE)
    order(dist)[2]
  })
  err.topo <- mean(!ok.dist$neigh.matrix[cbind(bmu, bmu2)])
  err.kaski <- e1071::allShortestPaths(ok.dist$proto.data.dist.neigh)$length[cbind(bmu,
                                                                                   bmu2)]
  err.kaski <- mean(err.kaski + sqrt(sqdist))
  cellpop <- table(factor(som$unit.classif, levels = 1:nrow(som$grid$pts)))
  res <- list(err.quant = err.quant, err.varratio = err.varratio,
              err.topo = err.topo, err.kaski = err.kaski, cellpop = cellpop)
  class(res) <- "somQual"
  res
}


supersomQuality_classes<-function (som,classes)
{
  #it uses cbind
  codes<-do.call(cbind,som$codes)

  if (is.null(som))
    return(NULL)
  ok.dist <- somDist(som)
  bmu <- som$unit.classif
  traindat<-data.frame(do.call(cbind,som$data))
  names(bmu)<-rownames(traindat)
  sqdist <- rowSums((traindat - codes[bmu, ])^2, na.rm = TRUE)


  err.quant <- tapply(sqdist,as.factor(classes[names(sqdist)]),function(x) mean(x,na.rm=T))
  tot_vars<-rowSums(t(t(traindat) - colMeans(traindat,
                                             na.rm = TRUE))^2, na.rm = TRUE)
  totalvar <- tapply(tot_vars,as.factor(classes[names(sqdist)]),mean)


  err.varratio <- lapply(1:length(totalvar),function(x){
    100 - round(100 * err.quant[[x]]/totalvar[[x]], 2)
  })
  bmu2 <- apply(traindat, 1, function(row) {
    dist <- colMeans((t(codes) - row)^2, na.rm = TRUE)
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


supersomQuality_whatmap<-function (som,whatmap,classes)
{
  #it uses cbind

  codes<-som$codes[[whatmap]]
  traindat<-som$data[[whatmap]]


  if (is.null(som))
    return(NULL)
  ok.dist <- somDist(som)
  bmu <- som$unit.classif

  names(bmu)<-rownames(traindat)
  sqdist <- rowSums((traindat - codes[bmu, ])^2, na.rm = TRUE)


  err.quant <- tapply(sqdist,as.factor(classes[names(sqdist)]),function(x) mean(x,na.rm=T))
  tot_vars<-rowSums(t(t(traindat) - colMeans(traindat,
                                             na.rm = TRUE))^2, na.rm = TRUE)
  totalvar <- tapply(tot_vars,as.factor(classes[names(sqdist)]),mean)


  err.varratio <- lapply(1:length(totalvar),function(x){
    100 - round(100 * err.quant[[x]]/totalvar[[x]], 2)
  })
  bmu2 <- apply(traindat, 1, function(row) {
    dist <- colMeans((t(codes) - row)^2, na.rm = TRUE)
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


somquality_weig<-function(m,classes,wei=F){


  user.weights<-data.frame(wei=m$user.weights)
  rownames(user.weights)<-names(m$data)

  res_layers<-do.call(rbind,lapply(names(m$data),function(i){
    re1<-supersomQuality_whatmap(m, i,classes)

    if(isTRUE(wei)){
      re1<-re1*(user.weights[i,]+1)
    }
    res<-data.frame(layer=i,class=rownames(re1),re1)
    res
  })
  )


  result<-aggregate(res_layers[-c(1:2)],res_layers[c("class")],mean)
  result<-result[c('err.quant',
                   'err.varratio',
                   'err.topo',
                   'err.kaski')]
  result
}
i=12
supersomQuality_final<-function(som,somC,session=MockShinySession$new()){

  classes_neu<-somC$som.hc
  classes_obs<-somC$somC
  cc=split(classes_obs,classes_obs)
  cn=split(classes_neu,classes_neu)
  max=length(som$data)*length(unique(classes_neu))
  mess<-paste0(round(1:max/max,2)*100,"%")
  split_vec <- split(mess, ceiling(seq_along(mess)/ (length(mess)/length(som$data))))
  names(split_vec)<-names(som$data)
i=1

withProgress(min=1,max=max,message="Running",session=session,{
  result<- lapply(names(som$data),function(whatmap){

    df<-sapply(seq_along(cc),function(i){
     message(i)
      newdata=lapply(names(som$data),function(whatmap){
        df0<-df<-som$data[[whatmap]][names(cc[[i]]),]
        df<-matrix(df, ncol=ncol(som$data[[whatmap]]))

        rownames(df)<-names(cc[[i]])
        colnames(df)<-colnames(som$data[[whatmap]])
      df
      })
      names(newdata)<-names(som$data)


      som2<-som
      som2$unit.classif<-som$unit.classif[names(cc[[i]])]
      som2$codes[[whatmap]][as.numeric(names(cn[[i]])),]<-som2$codes[[whatmap]][as.numeric(names(cn[[i]])),]
      som2$codes<- som2$codes[whatmap]
      qual<-somQuality(som2,   newdata[[whatmap]])

      incProgress(1,session=session,message=  split_vec[[whatmap]][[i]])
     # message(  split_vec[[whatmap]][[i]])

      unlist(qual[1:4])
    })
    data.frame(t(df))

  })
})

  names(result)<-names(som$data)
  result
}



supersomQualityClassWeighted<-function(som_qualist,classes){


  wei<-data.frame(table(classes)/length(classes))[2]

  som_qualist_weighted<-data.frame(t(sapply(1:nrow(som_qualist),function(i){
   unlist( som_qualist[i,]*wei[i,])
  })
  ))
  som_qualist_weighted
}


#' @export
som_dist2<-function (som) {

  if (is.null(som))
    return(NULL)
  proto.gridspace.dist <-as.matrix(dist(som$grid[[1]]))
  proto.dataspace.dist <- as.matrix(distsom(som))
  neigh <- round(proto.gridspace.dist, 3) == 1
  proto.dataspace.dist.neigh <- proto.dataspace.dist
  proto.dataspace.dist.neigh[!neigh] <- NA
  list(proto.grid.dist = proto.gridspace.dist, neigh.matrix = neigh,
       proto.data.dist = proto.dataspace.dist, proto.data.dist.neigh = proto.dataspace.dist.neigh)
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
Dcodesamples<-function(m,newdata){
  start_time <- Sys.time()
  codes<-kohonen::getCodes(m)
  data<-newdata
  res<-apply(data,1,function(x){
    apply(codes,1,function(xx){
      switch(m$dist.fcts,
             "BrayCurtis"={vegdist(t(data.frame(x,xx)))},
             "euclidean"={dist(t(data.frame(x,xx)))},
             "sumofsquares"={dist(t(data.frame(x,xx)))^2},
             "manhattan"={dist(t(data.frame(x,xx)),method="manhattan")}
      )
    })
  })



  return(res)
}

#' @export
quality_som<-function(som, som_pred) {

  traindat=som_pred$predictions
  if (is.null(som))
    return(NULL)
  ok.dist <- som_dist(som)


  bmu <- som_pred$unit.classif
  codes=som$codes


  res<-mapply(list,codes=codes,traindat=traindat, SIMPLIFY = F)
  sqdist <-lapply(res,function(x){
    rowSums((x[[2]] -x[[1]][bmu, ])^2,
            na.rm = TRUE)
  })
  err.quant<-lapply(sqdist,mean)
  x<-traindat[[1]]

  totalvar <- lapply(traindat,function(x){
    mean(rowSums(t(t(x) - colMeans(x,
                                   na.rm = TRUE))^2, na.rm = TRUE))
  })

  res2<-mapply(c,err.quant,totalvar, SIMPLIFY = F)

  err.varratio <-  lapply(res2,function(x){
    100 - round(100 * x[[1]]/x[[2]], 2)
  })

  bmu2 <- lapply(res,function(x){
    apply(x$traindat, 1, function(row) {
      dist <- colMeans((t(x$codes) - row)^2, na.rm = TRUE)
      order(dist)[2]
    })
  })


  err.topo <- lapply(bmu2,function(x){
    mean(!ok.dist$neigh.matrix[cbind(bmu, x)])
  })

  err.kaski <-lapply(bmu2,function(x){
    e1071::allShortestPaths(ok.dist$proto.data.dist.neigh)$length[cbind(bmu,
                                                                        x)]
  })

  res3<-mapply(list,sqdist=sqdist,err.kaski=err.kaski, SIMPLIFY = F)



  err.kaski <- lapply(res3,function(x){
    mean(x$err.kaski + sqrt(x$sqdist))
  })

  cellpop <- table(factor(som$unit.classif, levels = 1:nrow(som$grid$pts)))

  res4<-mapply(c,err.quant = err.quant, err.varratio = err.varratio,
               err.topo = err.topo, err.kaski = err.kaski, SIMPLIFY = F)

  res5<-do.call(rbind,res4)
  if(nrow(res5)==2){
    rownames(res5)<-c("X","Y")
  } else{ c("X")}

  res5
}


#' @export
fun_correctsom<-function(newdata,m){
  newdata=as.matrix(newdata)
  pred_som<-predict(m,newdata=newdata, whatmap = 1)
  res<-get_correct_predsom(m,pred_som,newdata)
  res
}
#' @export
fun_errors_somX<-function(correctsom,newdata){
  predX<- correctsom$predX
  observed<-newdata
  res0<-data.frame(get_unit_errors(data.frame(predX),observed))
  res0
}
#' @export
fun_errors_somY<-function(correctsom,newdata){
  predX<- correctsom$predY
  observed<-newdata
  res0<-data.frame(get_unit_errors(data.frame(predX),observed))
  res0
}
#' @export
fun_errors_somX_sp<-function(correctsom,newdata){
  predX<- correctsom$predX
  observed<-newdata

  rmse_res<- mse_res<-mape_res<-mae_res<-list()
  for(i in 1:ncol(observed))
  {
    mape_res[[i]]<-mape(observed[,i],predX[,i])
    mse_res[[i]]<-mse(observed[,i],predX[,i])
    rmse_res[[i]]<-rmse(observed[,i],predX[,i])
    mae_res[[i]]<-mae(observed[,i], predX[,i])
  }
  rmse<-data.frame(RMSE=do.call(c,rmse_res))
  mae<-data.frame(MAE=do.call(c,mae_res))
  mape<-data.frame(MAPE=do.call(c,mape_res))
  mse<-data.frame(MSE=do.call(c,mse_res))
  rownames(mape)<- rownames(mse)<-rownames(rmse)<-  rownames(mae)<-colnames(observed)
  data.frame(mae=mae,mse=mse,rmse=rmse)

}
#' @export
fun_errors_somY_sp<-function(correctsom,newdata_somY){
  predY<- correctsom$predY
  observed<-newdata_somY
  rmse_res<- mse_res<-mape_res<-mae_res<-list()
  for(i in 1:ncol(observed))
  {
    mape_res[[i]]<-mape(observed[,i],predY[,i])
    mse_res[[i]]<-mse(observed[,i],predY[,i])
    rmse_res[[i]]<-rmse(observed[,i],predY[,i])
    mae_res[[i]]<-mae(observed[,i], predY[,i])
  }
  rmse<-data.frame(RMSE=do.call(c,rmse_res))
  mae<-data.frame(MAE=do.call(c,mae_res))
  mape<-data.frame(MAPE=do.call(c,mape_res))
  mse<-data.frame(MSE=do.call(c,mse_res))
  rownames(mape)<- rownames(mse)<-rownames(rmse)<-  rownames(mae)<-colnames(observed)
  data.frame(mae=mae,mse=mse,rmse=rmse)

}

#' @export
get_somERR<-function(predsom_results=c('BMUs',
                                       'Data predictions (X)',
                                       'Neuron predictions (X)',
                                       'Data predictions (Y)',
                                       "Neuron predictions (Y)",
                                       "Obs errors (X)",
                                       "Obs errors (Y)",
                                       "Var errors (X)",
                                       "Var errors (Y)","SOM quality"),m,newdata,newdataY,som_pred){
  correctsom<-fun_correctsom(newdata, m)
  predX<- correctsom$predX
  predY<- correctsom$predY
  res<-switch(predsom_results,
              'BMUs'={
                bmu<-data.frame(bmu_test=som_pred$unit.classif)
                rownames(bmu)<-rownames(predX)
                bmu
              },
              'Data predictions (X)'={predX},
              'Neuron predictions (X)'={som_pred$unit.predictions[[1]]},
              'Data predictions (Y)'={predY},
              "Neuron predictions (Y)"={ neuY<-data.frame(do.call(cbind,som_pred$unit.predictions[-1]))},
              "Obs errors (X)"={
                data.frame(fun_errors_somX(correctsom,newdata))
              },
              "Obs errors (Y)"={
                try(data.frame(fun_errors_somY(correctsom,newdata)),silent =T)
              },
              "Var errors (X)"={
                data.frame(fun_errors_somX_sp(correctsom,newdata))
              },
              "Var errors (Y)"={
                try(data.frame(fun_errors_somY_sp(correctsom,newdataY)),silent =T)
              },
              "Quality measures"={data.frame(quality_som(som=m,som_pred=som_pred))}

  )
  return(res)
}

#' @export
match_col<-function(x,ref)
{
  sum(colnames(x)%in%colnames(ref))==ncol(x)
}


#' @export
cutdata1<-function(data,  method.hc="ward.D2", dist="bray", col=NULL)
{
  #validate(need(anyNA(data)==F, "The selected data contains missing values. Please remove them in Upload>Transform."))
  d<-vegdist(data, method=dist)
  hc<-hclust(d,method=method.hc)
  hc

  return(hc)
}

#' @export
cutm<-function(m,  method.hc="ward.D2")
{
  codes<-kohonen::getCodes(m)
  d=object.distances(m,"codes")
  hc<-hclust(d,method=method.hc)
  return(hc)
}


#' @export
#vals<-readRDS("savepoint.rds")
#args=vals$args
#m<-args$m
#groups=7
#members=NULL
#weighted=T
#newcolhabs=vals$newcolhabs
#method.hc="ward.D2"
#plot(m,type ="mapping",shape="straight", pch=16,bgcol=turbo(72))

cutsom_new<-function(m, k,hc_func,hc_method,distance_metric=NULL)
{
  if(class(m)[1]=="kohonen"){
    codes<-do.call(cbind,m$codes)
    rownames(codes)<-1:nrow(codes)
    d=object.distances(m,"codes")

    hc<-hcut(d,k,hc_func =hc_func ,hc_method =hc_method  ,method=hc_method,isdiss =T)
    hcut<-hc$cluster
    names(hcut)<-rownames(codes)
  } else{
    if(is.null(distance_metric)){
      distance_metric="euclidean"
    }
    d<-vegdist(m,distance_metric)

    hc<-hcut(d,k,hc_func =hc_func ,hc_method =hc_method  ,isdiss =T)
    hcut<-hc$cluster
    names(hcut)<-rownames(data)
    m=list()
    m[[1]]<-data
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

cutsom<-function(m,groups, members=NULL, method.hc="ward.D2", palette="turbo",newcolhabs=NULL, dataX,weighted=F)
{
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
cutdata2<-function(data, groups, method.hc="ward.D2", dist="bray", palette="turbo",newcolhabs)
{
  #validate(need(anyNA(data)==F, "The selected data contains missing values. Please remove them in Upload>Transform."))
  d<-vegdist(data, method=dist)
  hc<-hclust(d,method=method.hc)
  pred<-hcut<-cutree( hc, groups)
  #names(pred)<-rownames(data)
  if(is.null(newcolhabs)){
    colhabs<-tubro(groups)
  } else{

    colhabs<-getcolhabs(newcolhabs,palette,groups)
  }
  m=list()
  m[[1]]<-data
  somC<-list(somC=pred, colhabs=colhabs,som.model=m,som.hc=hcut, groups=groups,hc.object=hc)
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


ggplot.ggdend2<-function (data = NULL, mapping = aes(), ..., segments = TRUE,labels = TRUE, nodes = TRUE, horiz = FALSE, theme = theme_dendro(),
offset_labels = 0, na.rm = TRUE, environment = parent.frame(),lbls, col,legend=c("outside","inside"),dend1,text_size=5) {
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
                             colour = col, linetype = lty, size = lwd),
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

hc_plot<-function(somC, col=NULL, labels=NULL, lwd=2, main="", xlab="Observations", ylab="Height", base_size=12, theme='theme_grey',offset_labels=-.1,xlab_adj=20, legend=c("outside","inside"))
{
  legend<-match.arg(legend,c("outside","inside"))
  groups<-somC$groups
  opar<-par(no.readonly=TRUE)
  hc <- as.hclust(as.dendrogram(somC$hc.object))
  hc.dendo <- as.dendrogram(somC$hc.object)
  my_5_cluster <-somC$som.hc
  clust.cutree <- dendextend::cutree(hc.dendo, k=groups, order_clusters_as_data = FALSE)
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
  dend1<-highlight_branches_lwd(dend1, lwd)


  p<-ggplot.ggdend2(dend1,theme=NULL, offset_labels  = offset_labels, labels=T,col=col,lbls=lbls,legend=legend,text_size=base_size*.3,dend1=dend1)+xlab(xlab)+ylab(ylab)+ggtitle(main)
  if(legend!="outside"){
    dhei<-get_dend_labposition(dend1,groups)
    dhei$group<-lbls
    p<-p+geom_label(aes(x,y=ytop, label=group),data=dhei, show.legend = F,
                    size=base_size*.4,
                    color=unique(dendextend::get_leaves_branches_col(dend1)))

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

             axis.title.x = element_text(margin = margin(t =xlab_adj)))
  p<-p + coord_cartesian(clip = 'off', ylim=c((offset_labels*.9999)/2.5,max(hc$height)*1.05), expand = FALSE, xlim=c(0,length(lab_dend)+1))+theme(plot.margin = unit(c(1,1,1,1), "cm"))+
    scale_x_discrete(labels=lab_dend,limits=lab_dend)
  p

}



#attach(args)
#args<-readRDS('args.rds')

#do.call(hc_plot,args)
#attach(args)


#' @export
Kdata<-function(x){
  res<-ceiling((5*sqrt(nrow(x)))/3)
  if(res>15){res<-15}
  res
}



#' @export
myKmodel<-function(x){
  res<-ceiling(nrow(getCodes(x))/3)
  if(res>15){res<-15}
  res
}



#' @export
topology_back<-function(df,dist="BrayCurtis",topo=T) {
  if(isTRUE(topo)){

    df=data.frame(na.omit(df))
    rem<-which(colSums(df)==0)
    if(length(rem)>0){df<-df[,-rem]}

    if(length(attr(df,"scaled:scale"))<1)
    {
      df=scale(df)
    }
    #m<-supersom(as.matrix(df), grid=somgrid(2,2), rlen=1, dist.fcts=dist)
    df<-data.frame(df)*100
    N=floor(5*sqrt(nrow(df)))
    #ev <- eigen(object.distances(m,"data"))
    ev <- eigen(cov(as.matrix(df)))
    ratio_y<-ev$values[order(ev$values, decreasing = T)[2]]/abs(ev$values[order(ev$values, decreasing = T)[1]])
    ratio_x<-ev$values[order(ev$values, decreasing = T)[1]]/ev$values[order(ev$values, decreasing = T)[2]]
    y=floor(sqrt(N/ratio_y))
    x=ceiling(sqrt(N/ratio_x))
    res<-c(units=N,x=y,y=x)
    input<-res
    units<-input[1]
    gridtopo=input[2:3]
    Ydoble<-input[3]*2
    names(gridtopo)<-NULL
    prop<-gridtopo[1]/gridtopo[2]


  } else{return( res<-c(units=25,x=5,y=5))}
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


