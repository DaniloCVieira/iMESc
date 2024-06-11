## Copyright © 2023 [Danilo Candido Vieira]
## Licensed under the CC BY-NC-ND 4.0 license.

#' @export
som_sqdist<-function(m,whatmap,wins=m$unit.classif){
  codewins<-m$codes[[whatmap]][wins,]
  datawins<-m$data[[whatmap]][names(wins),]
  sqdist<-(codewins-datawins)^2
  if(!is.null(nrow(sqdist))){
    rowSums(sqdist)
  } else{
    sqdist
  }

}
#' @export
intra_var<-function(m,whatmap,somC,wins=m$unit.classif){
  bmus<-split(wins,somC$somC)
  intra0<-mean(som_sqdist(m,whatmap))
  q_intra<-sapply(bmus,function(wins){
    sqdist<-som_sqdist(m,whatmap, wins)
    #sum(((intra0[names(sqdist)])-sqdist)^2)/length(((intra0[names(sqdist)])-sqdist))
    mean(sqdist)
  })
  q_intra*intra0/sum(q_intra)
}
#' @export
qe_class<-function(m,somC){
  qes<-sapply(names(m$data),function(whatmap){
    intra_var(m,whatmap,somC)
  })
  # apply(qes,1,function(x) weighted.mean(x,m$distance.weights))
  qes
}
#' @export
screeplot_som<-function(m,k.max,hc_fun,hc_method, session = getDefaultReactiveDomain()) {
  t0<-Sys.time()
  withProgress(min=2,max=k.max,session=session,{
    rel<-lapply(1:k.max,function(k) {
      incProgress(1,message=paste0(round(k/k.max,2)*100,'%'), session=session)
      message(k)
      somC_temp<-cutsom_new(m,k,hc_fun,hc_method)
      if(k==1){ q<-colMeans(data.frame(as.list(qe_class(m,somC_temp))))} else{
        q<-colMeans(qe_class(m,somC_temp))
      }

      qe<-weighted.mean(q,m$distance.weights)
      data.frame(k=k,qe)
    })
  })

  t1<-Sys.time()
  message(t1-t0)
  data.frame(do.call(rbind,rel))
}


#' @export
predictY_kohonen<-function (object, newdata = NULL, unit.predictions = NULL, trainingdata = NULL,whatmap = NULL, threshold = 0, maxNA.fraction = object$maxNA.fraction,...){
  newdata_o=newdata
  codeNcols <- sapply(object$codes, ncol)
  ncodes <- nrow(object$codes[[object$whatmap[1]]])
  if (is.null(whatmap)) {
    whatmap <- object$whatmap
  }  else {
    whatmap <- kohonen:::check.whatmap(object, whatmap)}

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
      trainingdata <- kohonen:::check.data(trainingdata)
    }
    whatmap.tr <- kohonen:::check.whatmap(trainingdata, whatmap)
    if (!all(whatmap.tr == whatmap))
      stop("Data layer mismatch between map and trainingdata")
    narows <- kohonen:::check.data.na(trainingdata[whatmap], maxNA.fraction)
    trainingdata <- kohonen:::remove.data.na(trainingdata, narows)
    if (any(is.null(object$codes[whatmap])))
      stop("Attempt to map training data on the basis of data layers not present in the SOM")
    if (!kohonen:::checkListVariables(trainingdata[whatmap], codeNcols[whatmap]))
      stop("Number of columns of trainingdata do not match ",
           "codebook vectors")
    object$data <- trainingdata
    mappingX <- kohonen:::map(object, whatmap = whatmap, maxNA.fraction = maxNA.fraction,
                              ...)$unit.classif

    unit.predictions.tmp <- lapply(
      trainingdata,
      function(trd){
        aggregate(x = trd,
                  by = list(mappingX), FUN = mean, drop = FALSE)
      }
    )

    unit.predictions <- lapply(unit.predictions.tmp, function(x) {
      varnames <- colnames(x)[-1]
      x <- as.matrix(x)
      res <- matrix(NA, ncodes, ncol(x) - 1)
      colnames(res) <- varnames
      res[x[, 1], ] <- x[, -1, drop = FALSE]
      res
    })

    if(!is.null(newdata_o)){
      dtable2<-dtable<-dtable_newdata(object,newdata=newdata_o)
      re<-dtable[,1]
      dtable2[!dtable%in%mappingX]<-NA

      renew<-rel<-do.call(c,lapply(as.list(data.frame(t(dtable2))), function(x) x[!is.na(x)][1]))

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


      newdata <- kohonen:::check.data(newdata)
      if (is.null(newnames <- names(newdata))) {
        whatmap.new <- whatmap
      }    else {
        whatmap.new <- intersect(newnames, names(object$codes)[whatmap])
        dummy <- kohonen:::check.whatmap(newdata, whatmap.new)
      }
      if (!kohonen:::checkListVariables(newdata[whatmap.new], codeNcols[whatmap.new]))
        stop("Number of columns of newdata do not match codebook vectors")
      newrownames <- rownames(newdata[[1]])
      if (is.null(newrownames))
        newrownames <- 1:nrow(newdata[[1]])
      nnewrows <- length(newrownames)
      narows <- kohonen:::check.data.na(newdata[whatmap.new], maxNA.fraction = maxNA.fraction)
      newdata <- kohonen:::remove.data.na(newdata, narows)
      newmapping <- kohonen:::map(object, newdata = newdata, whatmap = whatmap.new,...
      )$unit.classif#
    }
    if(!is.null(newdata_o)){
      newmapping=renew
    }

    if (length(narows) > 0) {
      rowidx <- (1:nnewrows)[-narows]
    }  else {
      rowidx <- 1:nnewrows
    }
    nonNA <- rowidx[which(!is.na(newmapping))]
  }

  predictions <- lapply(unit.predictions, function(x) {
    pred <- matrix(NA, nnewrows, ncol(x))
    pred[nonNA, ] <- x[newmapping[nonNA], , drop = FALSE]
    pred
  })
  for (i in seq(along = predictions)) {
    dimnames(predictions[[i]]) <- list(newrownames,colnames(unit.predictions[[i]]))
  }
  if (any(isFactorPred <- sapply(predictions, is.factor.matrix)))
    predictions[isFactorPred] <- lapply(predictions[isFactorPred],
                                        classmat2classvec, threshold = threshold)
  list(predictions = predictions, unit.classif = newmapping,
       unit.predictions = unit.predictions, whatmap = whatmap)
}
