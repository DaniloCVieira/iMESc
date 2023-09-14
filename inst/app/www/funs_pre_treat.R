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
