#' @export
#'
bin_Freedman<-function(x){
  #Freedman-Diaconis rule
  iqr <- IQR(x)
  bin_width <- 2 * iqr / (length(x)^(1/3))
  n_bins <- ceiling(diff(range(x)) / bin_width)
}
bin_Sturges <- function(x) {
  # Sturges' rule
  n_bins <- ceiling(log2(length(x)) + 1)
  return(n_bins)
}
bin_Scott <- function(x) {
  # Scott's rule
  bin_width <- 3.5 * sd(x) / (length(x)^(1/3))
  n_bins <- ceiling(diff(range(x)) / bin_width)
  return(n_bins)
}
validate_transf<-function(dfrom,dto){

  message<-FALSE
  tfrom<-paste0(attr(dfrom,"name")," > ",
                attr(dfrom,"attr"))

  tto<-paste0(attr(dto,"name")," > ",
              attr(dto,"attr"))
  req(tfrom)
  req(tto)


  if(!any(rownames(dfrom)%in%
          rownames(dto))){
    logs<-paste0("No ID from '",tfrom,"' matches any ID from '",tto,"'")
    attr(logs,"type")<-"error"
    attr(message,"logs")<-logs
    return(message)
  }
  message<-TRUE

  if(nrow(dfrom)!=nrow(dto)){
    logs<-paste0(
      "'",tfrom,"' and '",tto,"' contain different number of observations. Data convertion will be done using matched IDs."
    )
    attr(logs,"type")<-"alert_warning"
    attr(message,"logs")<-logs


  }
  return(message)

}

# list packages in an R file
#' @export
check_model0<-function(data,attr="svm"){
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
check_model<-check_model0
#' @export
check_model

#' @export
EBNB<-function(abund, envi, PC=1){

  EBNB.table<-data.frame(matrix(NA, ncol(abund),3 ))
  colnames(EBNB.table)<-c("EB","NB","NP")
  pca<-ade4::dudi.pca(envi, scale=F, scannf=F, center=F)
  omi<-ade4::niche(pca,abund, scannf=FALSE)
  omi_scores<-omi$ls[,PC]
  df <- sweep(abund, 2, apply(abund, 2, sum), "/")
  pa<-abund
  pa[pa>0]<-1
  sds<-c()
  ebs<-list()
  for(i in 1:ncol(abund))
  {
    NP <- sum(df[, i] * omi_scores)
    sds[i]<-sd <- sqrt(sum(df[, i] * (omi_scores - NP)^2))
    NB<-abs(diff(c((NP+sd),(NP-sd) )))
    ebs[[i]]<-range(omi_scores[pa[,i]>0])
    EB<-abs(diff(ebs[[i]]))
    EBNB.table[i,1:3]<-c(EB,NB,NP)}
  rownames(EBNB.table)<-colnames(abund)
  attr(EBNB.table,"omi")<-omi
  attr(EBNB.table,"sds")<-sds
  attr(EBNB.table,"ebs")<-ebs
  return(EBNB.table)
}


#' @export
mice_impute<-function(data,na_method=c("pmm","rf","cart")){
  emp<-data==""|data=="NA"|is.na(data)
  if(any(emp)){
    data[emp]<-NA
  }
  method<-match.arg(na_method,c("pmm","rf","cart"))
  imputed_data <- suppressWarnings(mice::mice(data, method = method, m = 1,printFlag=T))
  res<-mice::complete(imputed_data, 1)
  rownames(res)<-rownames(data)
  res
}
#' @export

nadata<-function(data,na_method,k=NULL, data_old=NULL,data_name=NULL,attr="Numeric-Attribute"){
  if(attr!="Numeric-Attribute"){
    data<-attr(data,"factors")
  }



  fi<-find_na(data)
  fi<-find_na(data)
  y<-unique(names(which(colSums(fi)>0)))
  x<-unique(names(which(rowSums(fi)>0)))

  if(na_method%in%c("pmm","rf","cart")){
    pred<-mice_impute(data,na_method)
  } else{
    if(na_method=="knn"){
      imp <- caret::preProcess(data, method = "knnImpute", k = k)
      pred <- predict(imp, data)
      pred<-scale_back_imp(imp,pred)


    } else if(na_method=="bagImpute"){
      imp <- caret::preProcess(data, method = "bagImpute")
      pred <- predict(imp, data)


    } else if(na_method=="medianImpute"){
      imp <- caret::preProcess(data, method = "medianImpute")
      pred <- predict(imp, data)
    }

  }
  rownames(pred)<-rownames(data)
  attr(pred,"xy")<-data.frame(cbind(x,y))
  attr(pred,"data0")<-data


  return(pred)
}

#' @export
is_binary_df <- function(df) {
  # Ensure the input is a data frame
  if (!is.data.frame(df)) {
    stop("Input must be a data frame.")
  }

  # Function to check if a single column is binary
  is_binary_column <- function(column) {
    unique_values <- unique(column)
    length(unique_values) == 2
  }

  # Apply is_binary_column to each column and check if all are TRUE
  all(sapply(df, is_binary_column))
}

#' @export
pp_singles<-function(data){
  validate(need(is_binary_df(data),'The Singletons option requires a counting data'))
  data0<-vegan::decostand(data,"pa", na.rm=T)
  remove<-colSums(data0, na.rm=T)==1
  if(length(remove)>0){
    return( names(which(remove)))
  } else{
    NULL
  }
}
#' @export
pp_pctAbund<-function(data, pct=1){
  pct=pct/100
  remove<-colSums(data, na.rm=T)<(sum(data, na.rm=T)*pct)

  if(length(remove)>0){
    return( names(which(remove)))
  } else{
    NULL
  }
}
#' @export
pp_pctFreq<-function(data, pct){
  data0<-vegan::decostand(data,"pa", na.rm=T)
  remove<-colSums(data0, na.rm=T)<= round(nrow(data0)*pct)
  if(length(remove)>0){
   return( names(which(remove)))
  } else{
    NULL
  }

}

#' @export
singles<-function(data){
  validate(need(sum(apply(data,2, is.integer), na.rm=T)==0,'"The Singletons option requires a counting data"'))


  data0<-vegan::decostand(data,"pa", na.rm=T)
  remove<-colSums(data0, na.rm=T)==1
  if(sum(remove, na.rm=T)>0){return(  data[,-  which(remove)]) } else {return(data)}


}


