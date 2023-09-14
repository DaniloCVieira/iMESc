#' @export



flattenlist <- function(x){
  morelists <- sapply(x, function(xprime) class(xprime)[1]=="list")
  out <- c(x[!morelists], unlist(x[morelists], recursive=FALSE))
  if(sum(morelists)){
    Recall(out)
  }else{
    return(out)
  }
}

# list packages in an R file
#' @export
check_model<-check_model0<-function(data,attr="svm"){
  names_attr<-names(attr(data,attr))
  unsaved<-paste0('new ',attr,' (unsaved)')
  if(length(names_attr>0)){
    if(any(names_attr==unsaved)){
      pic<-which(names_attr==unsaved)
      names_attr<-names_attr[-pic]
      if(length(names_attr)>0){return(T)} else{
        return(F)
      }
    } else{ return(T)}
  } else{F}
}

check_model_caret<-function(data,attr="svm"){
  names_attr<-names(attr(data,attr))
  unsaved<-paste0('new ',attr,' (unsaved)')
  if(length(names_attr>0)){
    if(any(names_attr==unsaved)){
      pic<-which(names_attr==unsaved)
      names_attr<-names_attr[-pic]
      if(length(names_attr)>0){return(T)} else{
        return(F)
      }
    } else{ return(T)}
  } else{F}
}

#' @export
EBNB<-function(abund, envi, PC=1)
{

  EBNB.table<-data.frame(matrix(NA, ncol(abund),3 ))
  colnames(EBNB.table)<-c("EB","NB","NP")
  pca<-dudi.pca(envi, scale=F, scannf=F, center=F)
  omi<-niche(pca,abund, scannf=FALSE)
  omi_scores<-omi$ls[,PC]
  df <- sweep(abund, 2, apply(abund, 2, sum), "/")
  pa<-abund
  pa[pa>0]<-1
  for(i in 1:ncol(abund))
  {
    NP <- sum(df[, i] * omi_scores)
    sd <- sqrt(sum(df[, i] * (omi_scores - NP)^2))
    NB<-abs(diff(c((NP+sd),(NP-sd) )))
    EB<-abs(diff(range(omi_scores[pa[,i]>0])))
    EBNB.table[i,1:3]<-c(EB,NB,NP)}
  rownames(EBNB.table)<-colnames(abund)
  return(EBNB.table)
}


#' @export
getfunc_script<-function(filename){
  #filename <- "C:/Data/Documents/R/myFile.R"
  tmp <- getParseData(parse(filename, keep.source=TRUE))
  crit <- quote(token == "SYMBOL_FUNCTION_CALL")
  tmp <- dplyr::filter(tmp, .dots = crit)
  tmp <- unique(sort(tmp$text))
  src <- paste(as.vector(sapply(tmp, find)))
  outlist <- tapply(tmp,factor(src),c)
  outlist
}




#' @export
data.rares_fun<-function(saved_data,cur_data,filter_data,cutlevel_obs,filter_datalist,selecvar,selecobs,seltree,na.omit,transf,scale,center,rareabund,pct_rare,rarefreq,pct_prev,raresing,obs_match,                     obs_match_datalist,cor_filter=NULL,nzv=NULL){
  try({
req(cur_data)
    data <-  saved_data[[cur_data]]
   # pic<-  which(apply(data,2,function(x) (mean(x, na.rm=T)==0 )& sd(x, na.rm=T)==0 ))
    #if(length(pic)>0){    data<-data[,-pic]}

    try({
      data0<-data
      data=data.frame(retype(data0))
      rownames(data)<-rownames(data0)
      if (isTRUE(na.omit)) {data <- na.omit(data)}
      factors<-attr(data,"factors")
      ## Filter columns
      if(length(selecvar) >0){
        if (any(selecvar%in%colnames(data))) {
          pic<-which(colnames(data)%in%selecvar)
          if(length(pic)>0)
            data <- data[,pic, drop=F]
        }
      }
    })
    if(length(rareabund)>0){
      if(isTRUE(rareabund)){
        data = pctRares(data, pct_rare/100)
      }
    }
    if(length(rarefreq)>0){
      if(isTRUE(rarefreq)){
        data = pctPrev(data, pct_prev/100)
      }
    }
    if(length(raresing)>0){
      if(isTRUE(raresing)){
        data = singles(data)
      }
    }
    if(!is.null(cor_filter)){
      data<-data[cor_filter]
    }
    if(!is.null(nzv)){
      if(length(nzv)>0){
      data[nzv]<-NULL}
    }
    ### Filter observations
    if(length(filter_data)>0) {
      if (filter_data != "none")
      { if(length(cutlevel_obs)>0){

        pic <-which(as.character(factors[, filter_data]) %in% as.character(cutlevel_obs))
        data = data[pic,, drop=F ]}}
    }

    if(length(filter_datalist)>0){
      if(filter_datalist != "none")
      {
        pic<-rownames(saved_data[[filter_datalist]])
        data=na.omit(data[pic,,drop=F])

      }
    }
    if (length(selecobs) > 0) {data <- data[selecobs, ]}
    if(length(seltree)>0) {
      if(any(seltree %in% rownames(data))){
        data<-data[   seltree,,drop=F]}}
    if(isTRUE(obs_match)){
      req(length(obs_match_datalist)>0)
      data<-data[rownames(saved_data[[obs_match_datalist]]),,drop=F]
      data<-data[na.omit(rownames(data)),,drop=F]
    }
    remove_IDs<-which(rowSums(is.na(data))==ncol(data))
    if(length(remove_IDs)>0){
      data<-data[-remove_IDs,,drop=F]}
    if (length(transf) > 0) {
      if(isTRUE(scale)){
        if(isTRUE(center)){
          data= data.frame(scale(data, center = T))
        } else{
          data = data.frame(scale(data, center = F))
        }
      }
    }
    if (length(transf) > 0) {
      if (transf == "log2") {data = decostand(data, "log", na.rm = T, logbase = 2)}
      if (transf == "log10") {data = decostand(data, "log", na.rm = T, logbase = 10)}
      if (transf == "total") {data = decostand(data, "total", na.rm = T)}
      if (transf == "max") {data = decostand(data, "max", na.rm = T)}
      if (transf == "frequency") {data = decostand(data, "frequency", na.rm = T)}
      if (transf == "range") {data = decostand(data, "range", na.rm = T)}
      if (transf == "pa") {data = decostand(data, "pa", na.rm = T)}
      if (transf == "chi.square") {data = decostand(data, "chi.square", na.rm = T)}
      if (transf == "hellinger") {data = decostand(data, "hellinger", na.rm = T)}
      if (transf == "sqrt2") {data = sqrt(data)}
      if (transf == "sqrt4") {data = sqrt(sqrt(data))}
      if (transf == "log2(x+1)") {data = log2(data+1)}
      if (transf == "log10(x+1)") {data = log10(data+1)}
      if (transf == "BoxCox") {
        imp <- preProcess(data, method = "BoxCox")
        data <- predict(imp, data)
      }
      if (transf == "YeoJohnson") {
        imp <- preProcess(data, method = "YeoJohnson")
        data <- predict(imp, data)
      }
      if (transf == "expoTrans") {
        imp <- preProcess(data, method = "expoTrans")
        data <- predict(imp, data)
      }
      data = data.frame(data)
    }
    data<-data_migrate(saved_data[[cur_data]],data,cur_data)
    nrow_o<-attr(data,'nobs_ori')
    nrow_g<-nrow(data)
    ncol_o<-attr(data,'nvar_ori')
    ncol_g<-ncol(data)
    newattribs<-if(is.null(attr(data,"transf"))){
      data.frame(matrix(c(rep(NA,8)), dimnames = list(c('Subobs','Subvars','Transf','Scale','Center','NA.omit','Data_imp','Factor_imp'),"current")))
    }else{
      data.frame(attr(data, "transf"))
    }
    try({

      if(length(nrow_g)>0&length(nrow_o)>0){
        if(nrow_g<nrow_o){ newattribs[1,'current']<- paste(nrow_o,nrow_g,sep="::")}
      }
      if(length(ncol_g)>0&length(ncol_o)>0){
        if(ncol_g<ncol_o){newattribs[2,'current']<- paste(ncol_o,ncol_g,sep="::")}
      }
      newattribs[3,'current']<- if(is.null(transf)){'None'}else{transf}
      newattribs[5,'current']<- if(is.null(scale)){'FALSE'}else{scale}
      newattribs[6,'current']<- if(is.null(center)){'FALSE'}else{center}
      newattribs[7,'current']<- if(is.null(na.omit)){'FALSE'}else{na.omit}
    },silent =T)
    attr(data, "transf") <-newattribs
    attr(data, "nobs_ori") <-nrow_o
    attr(data, "nvar_ori") <-ncol_o
    #if(nrow(attr(data, "transf"))==2){attr(data, "transf")<-attr(data, "transf")[-1,,drop=F]}
    attr(data,"factors")<-attr(data,"factors")[rownames(data),,drop=F]
    attr(data,"coords")<-attr(data,"coords")[rownames(data),,drop=F]
    factors<-attr(data,"factors")
  })
  return(data)
}

#' @export
nadata<-function(data,na_method, data_old=NULL,data_name=NULL, k=NULL){
  transf<-attr(data,"transf")
  if(na_method=="missForest"){
    require("missForest")
    data<-data.frame(missForest(data)$ximp)
  }
  if(na_method=="mice"){
    require("mice")
    data<-complete(mice(data))
  }
  if(na_method=="median/mode"){
    data<-imputeMissings::impute(data, method = "median/mode")}
  if(na_method=="knn"){
    imp <- preProcess(data, method = "knnImpute", k = k)
    data <- predict(imp, data)
    if(!is.null(data_old)){
      transf["Scale",ncol(transf)]<-TRUE
      transf["Center",ncol(transf)]<-TRUE
    }

  }
  if(na_method=="bagImpute"){
    imp <- preProcess(data, method = "bagImpute")
    data <- predict(imp, data)
  }
  if(na_method=="medianImpute"){
    imp <- preProcess(data, method = "medianImpute")
    data <- predict(imp, data)
  }
if(!is.null(data_old)){
  data<-data_migrate(data_old,data,data_name)
}

  attr(data,"transf")<-transf
  return(data)
}


#' @export
nafactor<-function(data,na_method, k=NULL){
  factor<-attr(data,"factors")
  if(na_method=="median/mode"){
    factor<-imputeMissings::impute(factor, method = "median/mode")
  }
  attr(data,"factors")<-factor
  return(data)
}


#' @export
singles<-function(data){
  validate(need(sum(apply(data,2, is.integer), na.rm=T)==0,'"The Singletons option requires a counting data"'))


  data0<-decostand(data,"pa", na.rm=T)
  remove<-colSums(data0, na.rm=T)==1
  if(sum(remove, na.rm=T)>0){return(  data[,-  which(remove)]) } else {return(data)}


}

#' @export
pctRares<-function(data, pct=1){
  pct=pct/100
  remove<-colSums(data, na.rm=T)<(sum(data, na.rm=T)*pct)
  if(sum(remove, na.rm=T)>0){return(  data[,-  which(remove)]) } else {return(data)}
}

#' @export
pctPrev<-function(data, pct)
{

  data0<-decostand(data,"pa", na.rm=T)
  remove<-colSums(data0, na.rm=T)<= round(nrow(data0)*pct)
  if(sum(remove, na.rm=T)>0){return(  data[,-  which(remove)]) } else {return(data)}


}



supersom2<-function (data, grid = somgrid(), rlen = 100, alpha = c(0.05,
                                                                   0.01), radius = quantile(nhbrdist, 2/3), whatmap = NULL,user.weights = 1, maxNA.fraction = 0L, keep.data = TRUE,dist.fcts = NULL, mode = c("online", "batch", "pbatch"),cores = -1, init, normalizeDataLayers = TRUE) {
  data <- kohonen:::check.data(data)
  narows <- kohonen:::check.data.na(data, maxNA.fraction = maxNA.fraction)
  data <- kohonen:::remove.data.na(data, narows)
  full.data <- data
  nmat <- length(data)
  whatmap <- kohonen:::check.whatmap(data, whatmap)
  nmap <- length(whatmap)
  data <- data[whatmap]
  grid <- kohonen:::check.somgrid(grid)
  nhbrdist <- kohonen:::unit.distances(grid)
  if (length(radius) == 1)
    radius <- c(radius, 0)
  nobjects <- nrow(data[[1]])
  nvar <- sapply(data, ncol)
  data.matrix <- matrix(unlist(data), ncol = nobjects, byrow = TRUE)
  nNA <- kohonen:::getnNA(data, maxNA.fraction, nobjects)
  if (length(dist.fcts) == length(full.data)) {
    orig.dist.fcts <- dist.fcts
    dist.fcts <- dist.fcts[whatmap]
  }  else {
    if (length(dist.fcts) == 1) {
      orig.dist.fcts <- rep(dist.fcts, length(full.data))
      dist.fcts <- orig.dist.fcts[whatmap]
    }    else {
      defaultDist <- "sumofsquares"
      defaultClassDist <- "tanimoto"
      factorLayers <- sapply(full.data, function(datamat) is.factor(datamat) ||
                               kohonen:::is.factor.matrix(datamat))
      default.dist.fcts <- rep(defaultDist, length(full.data))
      if (any(factorLayers))
        default.dist.fcts[which(factorLayers)] <- defaultClassDist
      if (length(dist.fcts) == 0) {
        orig.dist.fcts <- default.dist.fcts
        dist.fcts <- orig.dist.fcts[whatmap]
      }      else {
        if (length(dist.fcts) == length(whatmap)) {
          orig.dist.fcts <- default.dist.fcts
          orig.dist.fcts[whatmap] <- dist.fcts
        }else {
          stop("Wrong number of distances defined")
        }
      }
    }
  }
  if (any(tanidists <- dist.fcts == "tanimoto")) {
    minvals <- sapply(data[which(tanidists)], min, na.rm = TRUE)
    maxvals <- sapply(data[which(tanidists)], max, na.rm = TRUE)
    if (any(minvals < 0 | maxvals > 1))
      stop("Layers for which the Tanimoto distance is used should have data within the [0,1] range")
  }
  dist.ptrs <- mygetDistancePointers(dist.fcts, maxNA.fraction = maxNA.fraction)
  ncodes <- nrow(grid$pts)
  if (missing(init)) {
    starters <- sample(1:nobjects, ncodes, replace = FALSE)
    init <- lapply(data, function(x) x[starters, , drop = FALSE])
  }  else {
    if (!is.list(init) & (is.vector(init) | is.factor(init) |
                          is.matrix(init)))
      init <- list(init)
    if (length(init) != nmap)
      stop("Incorrect number of initialization matrices")
    if (!all(sapply(init, nrow) == ncodes))
      stop("Incorrect number of objects in initalization matrices")
    if (!all(sapply(init, ncol) == nvar)) {
      if (maxNA.fraction == 0L) {
        stop("Incorrect number of variables in initialization matrices")
      }      else {
        stop("Incorrect number of variables in initialization matrices, ",
             "maybe due to the removal of columns because of NAs")
      }
    }
  }
  if (maxNA.fraction > 0L) {
    for (jj in seq(along = init)) {
      nastarters <- which(is.na(init[[jj]]), arr.ind = TRUE)
      if (nrow(nastarters) > 0) {
        for (i in unique(nastarters[, 1])) {
          idx <- which(nastarters[, 1] == i)
          imputers <- sample(data[[jj]][i, !is.na(data[[jj]][i,
          ])], length(idx), replace = TRUE)
          init[[jj]][i, nastarters[idx, 2]] <- imputers
        }
      }
    }
  }
  init.matrix <- matrix(unlist(init), ncol = ncodes, byrow = TRUE)
  distance.weights <- orig.user.weights <- rep(0, nmat)
  if (length(whatmap) == 1) {
    weights <- user.weights <- 1
    distance.weights[whatmap] <- orig.user.weights[whatmap] <- 1
  }  else {
    if (length(user.weights) == 1) {
      user.weights <- rep(user.weights, length(whatmap))
    }    else {
      if (length(user.weights) == nmat)
        user.weights <- user.weights[whatmap]
    }
    if (any(user.weights == 0))
      stop("Incompatibility between whatmap and user.weights")
    if (abs(sum(user.weights)) < .Machine$double.eps)
      stop("user.weights sum to zero")
    user.weights <- user.weights/sum(user.weights)
    orig.user.weights[whatmap] <- user.weights
    if (normalizeDataLayers) {
      meanDistances <- lapply(seq(along = init), function(ii) object.distances(list(data = init[ii],
                                                                                    whatmap = 1, user.weights = 1, distance.weights = 1,
                                                                                    maxNA.fraction = maxNA.fraction, dist.fcts = dist.fcts[ii]),
                                                                               type = "data"))
      if (any(sapply(meanDistances, mean) < .Machine$double.eps))
        stop("Non-informative layers present: mean distance between objects zero")
      distance.weights[whatmap] <- 1/sapply(meanDistances,
                                            mean)
    }    else {
      distance.weights <- rep(1, length(data))
    }
    weights <- user.weights * distance.weights[whatmap]
    weights <- weights/sum(weights)
  }
  mode <- match.arg(mode)
  switch(mode, online = {
    res <- kohonen:::RcppSupersom(data = data.matrix, codes = init.matrix,
                                  numVars = nvar, weights = weights, distanceFunctions = dist.ptrs,
                                  numNAs = nNA, neighbourhoodDistances = nhbrdist,
                                  neighbourhoodFct = as.integer(grid$neighbourhood.fct),
                                  alphas = alpha, radii = radius, numEpochs = rlen)
  }, batch = {
    res <- kohonen:::RcppBatchSupersom(data = data.matrix, codes = init.matrix,
                                       numVars = nvar, weights = weights, distanceFunctions = dist.ptrs,
                                       numNAs = nNA, neighbourhoodDistances = nhbrdist,
                                       neighbourhoodFct = as.integer(grid$neighbourhood.fct),
                                       radii = radius, numEpochs = rlen)
  }, pbatch = {
    res <- kohonen:::RcppParallelBatchSupersom(data = data.matrix,
                                               codes = init.matrix, numVars = nvar, weights = weights,
                                               distanceFunctions = dist.ptrs, numNAs = nNA, neighbourhoodDistances = nhbrdist,
                                               neighbourhoodFct = as.integer(grid$neighbourhood.fct),
                                               radii = radius, numEpochs = rlen, numCores = cores)
  })
  changes <- matrix(res$changes, ncol = nmap, byrow = TRUE)
  colnames(changes) <- names(data)
  mycodes <- res$codes
  layerID <- rep(1:nmap, nvar)
  mycodes2 <- split(as.data.frame(mycodes), layerID)
  mycodes3 <- lapply(mycodes2, function(x) t(as.matrix(x)))
  codes <- vector(length(full.data), mode = "list")
  names(codes) <- names(full.data)
  codes[whatmap] <- mycodes3
  for (ii in seq(along = whatmap)) colnames(codes[[whatmap[ii]]]) <- colnames(data[[ii]])
  if (keep.data) {
    mapping <- my_map.kohonen(structure(list(codes = codes,
                                             distance.weights = distance.weights, dist.fcts = orig.dist.fcts,
                                             data = full.data), class = "kohonen"), whatmap = whatmap,
                              user.weights = orig.user.weights, maxNA.fraction = maxNA.fraction)
    structure(list(data = full.data, unit.classif = mapping$unit.classif,
                   distances = mapping$distances, grid = grid, codes = codes,
                   changes = changes, alpha = alpha, radius = radius,
                   na.rows = narows, user.weights = orig.user.weights,
                   distance.weights = distance.weights, whatmap = whatmap,
                   maxNA.fraction = maxNA.fraction, dist.fcts = orig.dist.fcts),
              class = "kohonen")
  }  else {
    structure(list(grid = grid, codes = codes, changes = changes,
                   alpha = alpha, radius = radius, na.rows = narows,
                   user.weights = orig.user.weights, distance.weights = distance.weights,
                   whatmap = whatmap, maxNA.fraction = maxNA.fraction,
                   dist.fcts = orig.dist.fcts), class = "kohonen")
  }
}



mygetDistancePointers<-function (dist.fcts, prefabDists = c("sumofsquares", "euclidean","manhattan", "tanimoto","BrayCurtis"), maxNA.fraction) {
  dist.ptrs <- vector(length(dist.fcts), mode = "list")
  if (any(prefab.idx <- dist.fcts %in% prefabDists)) {
    dist.ptrs[prefab.idx] <- kohonen:::CreateStdDistancePointers(factor(dist.fcts[prefab.idx],
                                                                        levels = prefabDists), maxNA.fraction > 0L)
  }
  rdx=1
  if (any(rest.idx <- !prefab.idx)) {
    rest.idx <- which(rest.idx)
    for (rdx in rest.idx) {
      if (!exists(dist.fcts[rdx])) {
        stop(paste("Cannot find (custom) distance function: ",
                   dist.fcts[rdx], sep = ""))
      }
      dist.ptrs[[rdx]] <- eval(call(dist.fcts[rdx]))
    }
  }
  dist.ptrs
}

my_map.kohonen<-function (x, newdata, whatmap = NULL, user.weights = NULL, maxNA.fraction = x$maxNA.fraction,...) {
  codes <- x$codes
  nlayers <- length(codes)
  if (missing(newdata) & !is.null(x$data)) {
    newdata <- x$data
  }  else {
    newdata <- check.data(newdata)
  }
  if (is.null(user.weights)) {
    user.weights <- x$user.weights
    useTrainingWeights <- TRUE
  }  else {
    useTrainingWeights <- FALSE
  }
  if (length(user.weights) == 1)
    user.weights <- rep(1, nlayers)
  dist.ptrs <- mygetDistancePointers(x$dist.fcts, maxNA.fraction = maxNA.fraction)
  if (is.null(whatmap)) {
    whatmap <- whatmap.tr <- x$whatmap
  }  else {
    whatmap.tr <- kohonen:::check.whatmap(x, whatmap)
    whatmap <- kohonen:::check.whatmap(newdata, whatmap)
  }
  newdata <- newdata[whatmap]
  nachecks <- kohonen:::check.data.na(newdata, maxNA.fraction = maxNA.fraction)
  newdata <- kohonen:::remove.data.na(newdata, nachecks)
  if (useTrainingWeights & any(user.weights[whatmap.tr] < 1e-08))
    warning("Mapping new data using data layers not involved in training")
  dist.ptrs <- dist.ptrs[whatmap.tr]
  codes <- codes[whatmap.tr]
  user.weights.orig <- user.weights
  user.weights <- user.weights[whatmap.tr]
  if (length(whatmap.tr) == 1) {
    user.weights <- 1
  }  else {
    if (sum(user.weights >= 1e-08) == 0)
      stop("Only user.weights of zero given")
  }
  nvars <- sapply(codes, ncol)
  ncodes <- nrow(codes[[1]])
  nobjects <- nrow(newdata[[1]])
  nNA <- kohonen:::getnNA(newdata, maxNA.fraction, nobjects)
  newdata <- matrix(unlist(newdata), ncol = nobjects, byrow = TRUE)
  codes <- matrix(unlist(codes), ncol = ncodes, byrow = TRUE)
  weights <- user.weights * x$distance.weights[whatmap.tr]
  weights <- weights/sum(weights)
  res <- kohonen:::RcppMap(data = newdata, numVars = nvars, numNAs = nNA,
                           codes = codes, weights = weights, distanceFunctions = dist.ptrs)
  if (length(nachecks) > 0) {
    unit.classif <- distances <- rep(NA, nobjects + length(nachecks))
    unit.classif[-nachecks] <- res$winners + 1
    distances[-nachecks] <- res$unitdistances
    list(unit.classif = unit.classif, distances = distances,
         whatmap = whatmap, user.weights = user.weights.orig)
  }  else {
    list(unit.classif = res$winners + 1, distances = res$unitdistances,
         whatmap = whatmap, user.weights = user.weights.orig)
  }
}


BrayCurtis<-function() {
  return(brayCurtisDissim)
}

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


