require("kohonen")
require("caret")
my_check.data.na<-kohonen:::check.data.na
my_check.whatmap<-kohonen:::check.whatmap
my_check.data<-kohonen:::check.data
my_remove.data.na<-kohonen:::remove.data.na
my_checkListVariables<-kohonen:::checkListVariables


dtable_som_mysom<-function (kohobj, whatmap, data, classif = NULL)
{
  if (is.null(classif)) {
    if (is.null(kohobj$unit.classif)) {
      stop("No classification information present")
    }    else {
      classif <- kohobj$unit.classif
    }
  }
  if (missing(data)) {
    if (!is.null(kohobj$data)) {
      data <- kohobj$data
    }    else {
      stop("No data present")
    }
  }
  if (missing(whatmap)) {
    whatmap <- kohobj$whatmap
  }  else {
    whatmap <- check.whatmap(kohobj, whatmap)
  }
  weights <- kohobj$user.weights[whatmap] * kohobj$distance.weights[whatmap]
  maxNA.fraction <- kohobj$maxNA.fraction
  distanceFunctions <- kohobj$dist.fcts[whatmap]
  dist.ptrs <- mygetDistancePointers(distanceFunctions, maxNA.fraction = maxNA.fraction)
  data <- data[whatmap]
  codes <- kohobj$codes[whatmap]
  if (any(factor.idx <- sapply(data, is.factor)))
    data[factor.idx] <- lapply(data[factor.idx], classvec2classmat)
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

parameters2=data.frame(
  parameter=c("xdim","ydim","user.weights","topo","neighbourhood.fct",'toroidal','rlen'),
  class=c("numeric","numeric","numeric","character","character","logical",'numeric'),
  label=c("Rows","Columns","Layer Weight","Topology","Neighbourhoods","Toroidal","rlen")
)



make_grid<-function (x, y, len = NULL, search = "grid", rlen=NULL){
  if (search == "grid") {
    out <- expand.grid(xdim = 1:len,
                       ydim = 2:(len + 1),
                       topo = "hexagonal",
                       user.weights = seq(0.2, 0.8, length = len),
                       neighbourhood.fct="bubble",
                       toroidal=F,
                       rlen=500
                       )
    out <- subset(out, xdim <= ydim)
  }  else {
    out <- data.frame(xdim = sample(1:20, size = len * 10,replace = TRUE),
                      ydim = sample(1:20, size = len *10, replace = TRUE),
                      topo = sample(c("rectangular","hexagonal"),
                                    size = len * 10, replace = TRUE),
                      user.weights = runif(len *10, min = 0.01, max = 0.99),
                      neighbourhood.fct = sample(c("bubble","gaussian"),
                                                 size = len * 10, replace = TRUE),
                      toroidal = sample(c(TRUE,FALSE),
                                        size = len * 10, replace = TRUE),
                      rlen=sample(c(seq(100,500,by=50)), size = len * 10,replace = TRUE)


    )
    out <- subset(out, xdim <= ydim & xdim * ydim < nrow(x))
    out <- out[1:min(nrow(out), len), ]

    }

  out
}
my_fit<-function (x, y, wts, param, lev, last, classProbs, ...){


  layer_wts <- c(1 - param$user.weights, param$user.weights)
  layer_wts <- layer_wts/sum(layer_wts)
  if (is.numeric(y))
    y <- as.matrix(y, ncol = 1)
  kohonen::supersom2(list(X = as.matrix(x), Y = y), user.weights = layer_wts,
                    grid = kohonen::somgrid(param$xdim, param$ydim, as.character(param$topo),as.character(param$neighbourhood.fct),as.logical(param$toroidal)),rlen=param$rlen,...)



}


my_prob<-function (modelFit, newdata, submodels = NULL){


  predsom<-predict(modelFit$finalModel,type="prob",whatmap ="X")
  res<-predsom$unit.predictions$Y[predsom$unit.classif,]
  rownames(res)<-rownames(newdata)
  res

}
my_levels<-function (x) {
  x$obsLevels
}
my_tags<-c("Self-Organising Maps")
my_sort<-function (x){
  x[order(x$xdim, x$ydim), ]}
#library(kohonen)
ldaModelInfo <- getModelInfo(model = "xyf", regex = FALSE)[[1]]

# m modelo kohonen
# contem
#  - m$data - treinamento
#  - m$codes - codeboos
#  - m$unit.classif - bmus
#

# newdata novos dados

ldaModelInfo$predict


ko_predict<-function (modelFit, newdata, submodels = NULL) {
  if(modelFit$problemType=="Regression"){
    ## não faço substituicao do codebook. pois quero ele completo, pela lista de especie
    pred<-predict(modelFit, list(X = as.matrix(newdata)), whatmap = "X")$predictions$Y
    pred
    ?predict.kohonen

  } else{
    dtable_train<-dtable_som_mysom(modelFit)
    mtemp<-modelFit
    mtemp$data<-list(X=as.matrix(newdata))
    mtemp$codes<-mtemp$codes[1]
    dtable_new<-dtable_som_mysom(mtemp,whatmap=1)
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
ko_predict<-ldaModelInfo$predict


imesc_xyf= list(
  label="Self-Organizing Maps",
  library = "kohonen",
  fit=my_fit,
  check=my_check_som,
  loop=NULL,
  type = c("Classification",'Regression'),
  parameters=parameters2,
  grid=make_grid,
  predict=ko_predict,
  prob=my_prob,
  levels=my_levels,
  tags=my_tags,
  sort=my_sort,
  notes=NULL
)


