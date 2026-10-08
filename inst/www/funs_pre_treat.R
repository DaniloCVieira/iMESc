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



# ---- Temporal-Attribute parsing ------------------------------------------------
# Shared by Create Datalist, Exchange Attributes and Replace Attributes.
# Text is parsed only when it fully matches the format, so "01/02/2021" is never
# read as year 0001 by an unrelated format.

time_date_formats<-c("%Y-%m-%d","%Y/%m/%d","%d/%m/%Y","%d-%m-%Y","%m/%d/%Y","%Y%m%d")
time_datetime_formats<-c(
  "%Y-%m-%d %H:%M:%S","%Y-%m-%dT%H:%M:%S","%Y-%m-%d %H:%M","%Y/%m/%d %H:%M:%S",
  "%d/%m/%Y %H:%M:%S","%d/%m/%Y %H:%M","%d-%m-%Y %H:%M:%S","%m/%d/%Y %H:%M:%S"
)
time_format_choices<-c(
  "Auto / already formatted"="auto",
  "YYYY-MM-DD"="%Y-%m-%d",
  "DD/MM/YYYY"="%d/%m/%Y",
  "DD-MM-YYYY"="%d-%m-%Y",
  "MM/DD/YYYY"="%m/%d/%Y",
  "YYYY/MM/DD"="%Y/%m/%d",
  "YYYY-MM-DD HH:MM:SS"="%Y-%m-%d %H:%M:%S",
  "DD/MM/YYYY HH:MM:SS"="%d/%m/%Y %H:%M:%S",
  "DD-MM-YYYY HH:MM:SS"="%d-%m-%Y %H:%M:%S",
  "YYYY/MM/DD HH:MM:SS"="%Y/%m/%d %H:%M:%S",
  "HH:MM:SS"="%H:%M:%S",
  "HH:MM"="%H:%M",
  "Custom"="custom"
)

#' @export
time_format_regex<-function(fmt,allow_time_suffix=FALSE){
  r<-gsub("([.^$|()*+?{}\\[\\]\\\\])","\\\\\\1",fmt)
  r<-gsub("%Y","\\\\d{4}",r)
  r<-gsub("%[mdHMS]","\\\\d{1,2}",r)
  r<-gsub("%[A-Za-z]",".+?",r)
  paste0("^",r,if(isTRUE(allow_time_suffix)) "([ T].*)?" else "","$")
}

#' @export
strict_parse_time<-function(x_chr,fmt,type="date"){
  n<-length(x_chr)
  ok<-!is.na(x_chr)&grepl(time_format_regex(fmt,allow_time_suffix=identical(type,"date")),x_chr)
  if(identical(type,"date")){
    out<-rep(as.Date(NA),n)
    if(any(ok)) out[ok]<-as.Date(substr(x_chr[ok],1,regexpr("[ T]",paste0(x_chr[ok]," "))-1),format=fmt)
    return(out)
  }
  out<-as.POSIXct(rep(NA_real_,n),origin="1970-01-01",tz="UTC")
  if(any(ok)) out[ok]<-as.POSIXct(x_chr[ok],format=fmt,tz="UTC")
  out
}

# Format (among 'formats') that parses most values; ties keep the order of 'formats'
#' @export
best_time_format<-function(x_chr,formats,type="date"){
  n_ok<-vapply(formats,function(fmt) sum(!is.na(strict_parse_time(x_chr,fmt,type))),numeric(1))
  if(!length(n_ok)||max(n_ok)==0) return(NULL)
  formats[which.max(n_ok)]
}

# "keep" + an explicit format means the user wants that format applied
#' @export
effective_time_type<-function(type,format=NULL,custom_format=NULL){
  if(!is.null(format)&&identical(format,"custom")) format<-custom_format
  if(!is.null(type)&&!identical(type,"keep")) return(type)
  if(is.null(format)||!nzchar(format)||identical(format,"auto")) return("keep")
  has_time<-grepl("%H",format)
  has_date<-grepl("%[dYmjy]",format)
  if(has_time&&has_date) return("datetime")
  if(has_time) return("time")
  "date"
}

#' @export
convert_time_column<-function(x,type,format,custom_format=NULL){
  type<-effective_time_type(type,format,custom_format)
  if(identical(type,"keep")){
    return(x)
  }
  if(!is.null(format)&&format=="custom"){
    format<-if(!is.null(custom_format)&&nzchar(custom_format)) custom_format else NULL
  }
  if(is.null(format)||format=="auto"){
    format<-NULL
  }
  x_chr<-trimws(as.character(x))
  x_chr[!is.na(x_chr)&!nzchar(x_chr)]<-NA

  if(type=="date"){
    if(is.null(format)){
      if(inherits(x,"Date")) return(as.Date(x))
      if(inherits(x,c("POSIXct","POSIXlt"))) return(as.Date(format(x,"%Y-%m-%d")))
      format<-best_time_format(x_chr,time_date_formats,"date")
      if(is.null(format)) return(rep(as.Date(NA),length(x)))
    }
    return(strict_parse_time(x_chr,format,"date"))
  }

  if(type=="datetime"){
    if(is.null(format)){
      if(inherits(x,c("POSIXct","POSIXlt"))) return(as.POSIXct(x,tz="UTC"))
      if(inherits(x,"Date")) return(as.POSIXct(format(x,"%Y-%m-%d"),tz="UTC"))
      format<-best_time_format(x_chr,time_datetime_formats,"datetime")
      if(is.null(format)){
        date_format<-best_time_format(x_chr,time_date_formats,"date")
        if(is.null(date_format)) return(as.POSIXct(rep(NA_real_,length(x)),origin="1970-01-01",tz="UTC"))
        return(as.POSIXct(format(strict_parse_time(x_chr,date_format,"date"),"%Y-%m-%d"),tz="UTC"))
      }
    }
    return(strict_parse_time(x_chr,format,"datetime"))
  }

  if(type=="time"){
    if(is.null(format)){
      format<-if(all(is.na(x_chr)|grepl("^\\d{1,2}:\\d{2}$",x_chr))) "%H:%M" else "%H:%M:%S"
    }
    out<-suppressWarnings(strptime(x_chr,format=format,tz="UTC"))
    return(format(out,"%H:%M:%S"))
  }

  if(type%in%c("year","month","day")){
    return(suppressWarnings(as.integer(x_chr)))
  }

  x
}

# Default type/format shown in the "Format Temporal-Attribute" pages
#' @export
guess_time_settings<-function(x){
  res<-list(type="keep",format="auto",custom="")
  if(inherits(x,"Date")){
    res$type<-"date"
    return(res)
  }
  if(inherits(x,c("POSIXct","POSIXlt"))){
    res$type<-"datetime"
    return(res)
  }
  x_chr<-trimws(as.character(x))
  x_chr<-x_chr[!is.na(x_chr)&nzchar(x_chr)]
  if(!length(x_chr)) return(res)
  set_format<-function(res,fmt){
    if(fmt%in%time_format_choices){
      res$format<-fmt
    }else{
      res$format<-"custom"
      res$custom<-fmt
    }
    res
  }
  if(all(grepl("^\\d{4}$",x_chr))){
    res$type<-"year"
    return(res)
  }
  if(all(grepl("^\\d{1,2}:\\d{2}(:\\d{2})?$",x_chr))){
    res$type<-"time"
    return(set_format(res,if(all(grepl("^\\d{1,2}:\\d{2}$",x_chr))) "%H:%M" else "%H:%M:%S"))
  }
  fmt<-best_time_format(x_chr,time_datetime_formats,"datetime")
  if(!is.null(fmt)&&all(!is.na(strict_parse_time(x_chr,fmt,"datetime")))){
    res$type<-"datetime"
    return(set_format(res,fmt))
  }
  fmt<-best_time_format(x_chr,time_date_formats,"date")
  if(!is.null(fmt)&&all(!is.na(strict_parse_time(x_chr,fmt,"date")))){
    res$type<-"date"
    return(set_format(res,fmt))
  }
  res
}
