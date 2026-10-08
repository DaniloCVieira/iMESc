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
mice_impute<-function(data,na_method=c("pmm","rf","cart"),ignore=NULL){
  emp<-data==""|data=="NA"|is.na(data)
  if(any(emp)){
    data[emp]<-NA
  }
  method<-match.arg(na_method,c("pmm","rf","cart"))
  # ignore: rows imputed but not used to fit the imputation models
  imputed_data <- suppressWarnings(mice::mice(data, method = method, m = 1,printFlag=T,ignore=ignore))
  res<-mice::complete(imputed_data, 1)
  rownames(res)<-rownames(data)
  res
}

# Imputation fitted on fit_rows (all rows when NULL) and applied to every row of data
#' @export
impute_core<-function(data,na_method,k=NULL,fit_rows=NULL){
  if(na_method%in%c("pmm","rf","cart")){
    if(is.null(fit_rows)){
      pred<-mice_impute(data,na_method)
    } else{
      # 1) the fitting rows are imputed alone, so the other rows cannot change them
      #    (not even through the order of the random draws);
      # 2) the other rows are then imputed with models fitted only on the completed
      #    fitting rows (mice 'ignore').
      completed<-data
      completed[fit_rows,]<-mice_impute(data[fit_rows,,drop=FALSE],na_method)
      if(any(find_na(completed[!fit_rows,,drop=FALSE]))){
        pred<-mice_impute(completed,na_method,ignore=!fit_rows)
        pred[fit_rows,]<-completed[fit_rows,]
      } else{
        pred<-completed
      }
    }
  } else{
    fit<-if(is.null(fit_rows)) data else data[fit_rows,,drop=FALSE]
    if(na_method=="knn"){
      imp <- caret::preProcess(fit, method = "knnImpute", k = k)
      pred <- predict(imp, data)
      pred<-scale_back_imp(imp,pred)
    } else if(na_method=="bagImpute"){
      imp <- caret::preProcess(fit, method = "bagImpute")
      pred <- predict(imp, data)
    } else if(na_method=="medianImpute"){
      imp <- caret::preProcess(fit, method = "medianImpute")
      pred <- predict(imp, data)
    }
  }
  pred<-data.frame(pred,check.names=FALSE)
  rownames(pred)<-rownames(data)
  pred
}
#' @export

# group: factor (one value per row) for imputation by group, e.g. a partition column.
# group_mode "reference": fit only on rows of ref_level and apply to all rows (no test
# information enters the imputation); "separate": impute each level with its own rows.
nadata<-function(data,na_method,k=NULL, data_old=NULL,data_name=NULL,attr="Numeric-Attribute",
                 group=NULL,group_mode=c("reference","separate"),ref_level=NULL,group_name="group"){
  if(attr!="Numeric-Attribute"){
    data<-attr(data,"factors")
  }
  group_mode<-match.arg(group_mode)

  fi<-find_na(data)
  y<-unique(names(which(colSums(fi)>0)))
  x<-unique(names(which(rowSums(fi)>0)))

  no_obs_cols<-function(rows){
    names(which(colSums(!find_na(data[rows,,drop=FALSE]))==0))
  }
  run_core<-function(sub,level,fit_rows=NULL){
    tryCatch(impute_core(sub,na_method,k,fit_rows=fit_rows),error=function(e){
      stop(paste0("Imputation failed for ",group_name," = '",level,"': ",conditionMessage(e)),call.=FALSE)
    })
  }

  if(is.null(group)){
    pred<-impute_core(data,na_method,k)
  } else{
    group<-as.character(group)
    validate(need(length(group)==nrow(data)&&!anyNA(group),paste0("The grouping factor '",group_name,"' must have a value for every observation.")))
    if(group_mode=="separate"){
      pred<-data
      for(g in unique(group)){
        rows<-group==g
        if(!any(fi[rows,])) next
        empty<-no_obs_cols(rows)
        validate(need(!length(empty),paste0("Level '",g,"' of '",group_name,"' has no observed values in: ",paste(empty,collapse=", "),".")))
        pred[rows,]<-run_core(data[rows,,drop=FALSE],g)
      }
    } else{
      validate(need(length(ref_level)==1&&ref_level%in%group,paste0("Choose the reference level of '",group_name,"'.")))
      fit_rows<-group==ref_level
      empty<-no_obs_cols(fit_rows)
      validate(need(!length(empty),paste0("Level '",ref_level,"' of '",group_name,"' has no observed values in: ",paste(empty,collapse=", "),".")))
      pred<-run_core(data,ref_level,fit_rows=fit_rows)
    }
    attr(pred,"group_info")<-list(group=group_name,mode=group_mode,ref_level=ref_level)
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

# ---- Outlier handling (Options > Outlier Handling) -----------------------------------------
# Detection works on (optionally transformed) values, by variable and group; limits are
# reported back in the original units so that values can be capped.

#' @export
out_method_choices<-list(
  "Univariate"=c("Robust z (median/MAD) - recommended"="mad","IQR (Tukey fences)"="iqr","Z-score (mean/SD)"="z","Percentiles"="pct","Generalized ESD (Rosner)"="gesd"),
  "Temporal"=c("Hampel filter (rolling median/MAD)"="hampel"),
  "Multivariate"=c("Mahalanobis distance"="maha","Robust Mahalanobis (trimmed)"="maha_robust")
)
out_multivariate<-c("maha","maha_robust")

#' @export
out_transform<-function(v,transf="none"){
  suppressWarnings(switch(transf,
                          log10=ifelse(v>0,log10(v),NA),
                          log1p=ifelse(v>-1,log1p(v),NA),
                          sqrt=ifelse(v>=0,sqrt(v),NA),
                          v))
}
#' @export
out_back<-function(v,transf="none"){
  switch(transf,log10=10^v,log1p=expm1(v),sqrt=ifelse(v<0,0,v^2),v)
}

# generalized extreme studentized deviate test (Rosner 1983): indices of the outliers
#' @export
out_gesd<-function(v,max_out=10,alpha=0.05){
  n<-length(v)
  r<-min(max_out,floor((n-1)/2))
  if(r<1) return(integer(0))
  idx<-seq_len(n)
  x<-v
  removed<-integer(0)
  R<-numeric(0)
  lam<-numeric(0)
  for(i in seq_len(r)){
    s<-stats::sd(x)
    if(!is.finite(s)||s==0) break
    dev<-abs(x-mean(x))/s
    j<-which.max(dev)
    R<-c(R,dev[j])
    removed<-c(removed,idx[j])
    x<-x[-j]
    idx<-idx[-j]
    nn<-n-i+1
    t<-stats::qt(1-alpha/(2*nn),nn-2)
    lam<-c(lam,(nn-1)*t/sqrt((nn-2+t^2)*nn))
  }
  if(!length(R)) return(integer(0))
  k<-max(c(0,which(R>lam)))
  if(k==0) integer(0) else removed[seq_len(k)]
}

# limits, score and flag of one vector (NA kept as not flagged)
#' @export
out_univariate<-function(x,method="iqr",k=1.5,q=c(0.25,0.75),p=c(0.01,0.99),alpha=0.05,max_out=10,ord=NULL,window=5){
  n<-length(x)
  lower<-upper<-score<-rep(NA_real_,n)
  flag<-rep(FALSE,n)
  ok<-!is.na(x)
  v<-x[ok]
  if(length(v)<3) return(list(lower=lower,upper=upper,score=score,flag=flag))
  if(method%in%c("iqr","z","mad","pct")){
    if(identical(method,"iqr")){
      Q<-stats::quantile(v,q,names=FALSE)
      I<-Q[2]-Q[1]
      lim<-c(Q[1]-k*I,Q[2]+k*I)
      score<-if(I>0) ifelse(x<Q[1],(Q[1]-x)/I,ifelse(x>Q[2],(x-Q[2])/I,0)) else rep(NA_real_,n)
    } else if(identical(method,"z")){
      s<-stats::sd(v)
      lim<-mean(v)+c(-1,1)*k*s
      score<-if(s>0) abs(x-mean(v))/s else rep(NA_real_,n)
    } else if(identical(method,"mad")){
      s<-stats::mad(v)
      lim<-stats::median(v)+c(-1,1)*k*s
      score<-if(s>0) abs(x-stats::median(v))/s else rep(NA_real_,n)
    } else{
      lim<-stats::quantile(v,p,names=FALSE)
    }
    lower[]<-lim[1]
    upper[]<-lim[2]
    flag<-ok&(x<lim[1]|x>lim[2])
  }
  if(identical(method,"gesd")){
    idx<-which(ok)
    fl<-out_gesd(x[idx],max_out,alpha)
    flag[idx[fl]]<-TRUE
    s<-stats::sd(v)
    score<-if(s>0) abs(x-mean(v))/s else rep(NA_real_,n)
    keep<-x[ok&!flag]
    lower[]<-min(keep)
    upper[]<-max(keep)
  }
  if(identical(method,"hampel")){
    if(is.null(ord)) ord<-seq_len(n)
    o<-order(ord)
    xs<-x[o]
    w<-max(1,round(window))
    lo<-hi<-sc<-rep(NA_real_,n)
    for(i in seq_len(n)){
      win<-xs[max(1,i-w):min(n,i+w)]
      win<-win[!is.na(win)]
      if(length(win)<3) next
      m<-stats::median(win)
      s<-stats::mad(win)
      lo[i]<-m-k*s
      hi[i]<-m+k*s
      if(s>0&&!is.na(xs[i])) sc[i]<-abs(xs[i]-m)/s
    }
    lower[o]<-lo
    upper[o]<-hi
    score[o]<-sc
    flag<-ok&!is.na(lower)&(x<lower|x>upper)
  }
  list(lower=lower,upper=upper,score=score,flag=flag)
}

# squared Mahalanobis distances; the robust version trims the most distant observations
# iteratively and rescales the covariance (consistency with the chi-square median)
#' @export
out_mahalanobis<-function(X,robust=FALSE,alpha=0.01,trim=0.25){
  X<-as.matrix(X)
  p<-ncol(X)
  n<-nrow(X)
  if(n<=p+1) stop("needs more complete observations than variables")
  center<-colMeans(X)
  S<-stats::cov(X)
  inv<-tryCatch(solve(S),error=function(e) NULL)
  if(is.null(inv)) stop("the covariance matrix is singular (constant or collinear variables)")
  if(isTRUE(robust)){
    for(it in seq_len(20)){
      d<-stats::mahalanobis(X,center,S)
      keep<-d<=stats::quantile(d,1-trim)
      if(sum(keep)<=p+1) break
      c2<-colMeans(X[keep,,drop=FALSE])
      S2<-stats::cov(X[keep,,drop=FALSE])
      if(is.null(tryCatch(solve(S2),error=function(e) NULL))) break
      done<-max(abs(c2-center))<1e-8
      center<-c2
      S<-S2
      if(done) break
    }
    d<-stats::mahalanobis(X,center,S)
    S<-S*stats::median(d)/stats::qchisq(0.5,p)
  }
  d2<-stats::mahalanobis(X,center,S)
  cut<-stats::qchisq(1-alpha,p)
  list(d2=d2,cutoff=cut,flag=d2>cut)
}

#' @export
out_detect<-function(d,vars,method="iqr",k=1.5,q=c(0.25,0.75),p=c(0.01,0.99),alpha=0.05,max_out=10,window=5,
                     group=NULL,time=NULL,direction="both",transf="none"){
  ids<-rownames(d)
  g<-if(is.null(group)) factor(rep("All",nrow(d))) else factor(group)
  if(method%in%out_multivariate){
    X<-sapply(vars,function(v) out_transform(d[[v]],transf))
    X<-matrix(X,nrow=nrow(d),dimnames=list(ids,vars))
    errors<-character(0)
    obs<-do.call(rbind,lapply(split(seq_len(nrow(d)),g),function(ii){
      Xi<-X[ii,,drop=FALSE]
      cc<-stats::complete.cases(Xi)
      r<-tryCatch(out_mahalanobis(Xi[cc,,drop=FALSE],robust=identical(method,"maha_robust"),alpha=alpha),error=function(e) conditionMessage(e))
      gl<-as.character(g[ii[1]])
      if(is.character(r)){
        errors<<-c(errors,paste0(gl,": ",r))
        return(NULL)
      }
      data.frame(id=ids[ii][cc],group=gl,index=ii[cc],distance=sqrt(r$d2),cutoff=sqrt(r$cutoff),
                 d2=r$d2,chisq_cutoff=r$cutoff,p=length(vars),flag=r$flag,stringsAsFactors=FALSE)
    }))
    if(!is.null(obs)){
      rownames(obs)<-NULL
      if(!is.null(time)) obs$time<-time[obs$index]
    }
    flags<-if(is.null(obs)) NULL else obs[obs$flag,,drop=FALSE]
    if(!is.null(flags)&&nrow(flags)){
      flags$variable<-"(all variables)"
      flags$side<-"multivariate"
      flags$score<-flags$distance
    }
    summ<-if(is.null(obs)) NULL else do.call(rbind,lapply(split(obs,obs$group),function(s)
      data.frame(Group=s$group[1],Variables=length(vars),Complete_obs=nrow(s),Cutoff_distance=s$cutoff[1],
                 Flagged_obs=sum(s$flag),Percent=100*mean(s$flag),Max_distance=max(s$distance))))
    return(list(type="multivariate",method=method,obs=obs,flags=flags,summary=summ,errors=errors,vars=vars,transf=transf))
  }
  cells<-do.call(rbind,lapply(vars,function(v){
    do.call(rbind,lapply(split(seq_len(nrow(d)),g),function(ii){
      xo<-d[[v]][ii]
      xt<-out_transform(xo,transf)
      ord<-if(!is.null(time)) as.numeric(time[ii]) else ii
      r<-out_univariate(xt,method,k,q,p,alpha,max_out,ord,window)
      side<-ifelse(!is.na(xt)&!is.na(r$lower)&xt<r$lower,"low",ifelse(!is.na(xt)&!is.na(r$upper)&xt>r$upper,"high",""))
      fl<-r$flag&switch(direction,low=side=="low",high=side=="high",TRUE)
      out<-data.frame(id=ids[ii],index=ii,variable=v,group=as.character(g[ii]),value=xo,value_t=xt,
                      lower_t=r$lower,upper_t=r$upper,lower=out_back(r$lower,transf),upper=out_back(r$upper,transf),
                      score=r$score,side=side,flag=fl,stringsAsFactors=FALSE)
      if(!is.null(time)) out$time<-time[ii]
      out
    }))
  }))
  rownames(cells)<-NULL
  cells$variable<-factor(cells$variable,levels=vars)
  flags<-cells[cells$flag,,drop=FALSE]
  rolling<-identical(method,"hampel")
  summ<-do.call(rbind,lapply(split(cells,list(cells$variable,cells$group),drop=TRUE),function(s){
    data.frame(Variable=as.character(s$variable[1]),Group=s$group[1],n=sum(!is.na(s$value_t)),
               Low=sum(s$flag&s$side=="low"),High=sum(s$flag&s$side=="high"),Flagged=sum(s$flag),
               Percent=100*sum(s$flag)/max(1,sum(!is.na(s$value_t))),
               Lower_limit=if(rolling) NA else s$lower[1],Upper_limit=if(rolling) NA else s$upper[1],
               Min=suppressWarnings(min(s$value,na.rm=TRUE)),Max=suppressWarnings(max(s$value,na.rm=TRUE)),
               stringsAsFactors=FALSE)
  }))
  rownames(summ)<-NULL
  list(type="univariate",method=method,cells=cells,flags=flags,summary=summ,errors=character(0),vars=vars,transf=transf)
}

# treatment of the selected flags: NA, capping at the limits, median, or removal of the observations
#' @export
out_treat<-function(d,flags,action="na",vars=NULL,group=NULL){
  if(is.null(flags)||!nrow(flags)) return(d)
  if(identical(action,"remove")) return(d[!rownames(d)%in%unique(flags$id),,drop=FALSE])
  if(identical(flags$variable[1],"(all variables)")){
    d[rownames(d)%in%flags$id,vars]<-NA
    return(d)
  }
  ri<-match(flags$id,rownames(d))
  for(v in unique(as.character(flags$variable))){
    f<-flags[as.character(flags$variable)==v,,drop=FALSE]
    r<-match(f$id,rownames(d))
    new<-switch(action,
                cap=ifelse(f$side=="low",f$lower,f$upper),
                median={
                  # median of the group, computed without the flagged values
                  x<-d[[v]]
                  x[r]<-NA
                  g<-if(is.null(group)) rep("All",nrow(d)) else as.character(group)
                  med<-tapply(x,g,stats::median,na.rm=TRUE)
                  unname(med[g[r]])
                },
                rep(NA_real_,nrow(f)))
    d[r,v]<-new
  }
  d
}

# ---- Outlier plots
out_theme<-function(base_size=12) ggplot2::theme_bw(base_size=base_size)

#' @export
gg_out_box<-function(cells,base_size=12,ncol=NULL,point_size=1.6){
  grouped<-length(unique(cells$group))>1
  cells<-cells[!is.na(cells$value_t),,drop=FALSE]
  cells$Status<-factor(ifelse(cells$flag,"Flagged","Kept"),levels=c("Kept","Flagged"))
  cells$x<-if(grouped) cells$group else ""
  p<-ggplot2::ggplot(cells,ggplot2::aes(x=x,y=value_t))+
    ggplot2::geom_boxplot(outlier.shape=NA,fill="gray95",width=0.5)+
    ggplot2::geom_jitter(ggplot2::aes(color=Status,size=Status),width=0.15,height=0,alpha=0.7)+
    ggplot2::scale_color_manual(values=c(Kept="gray45",Flagged="#D7301F"),drop=FALSE)+
    ggplot2::scale_size_manual(values=c(Kept=point_size*0.7,Flagged=point_size*1.3),drop=FALSE)
  lim<-unique(cells[!is.na(cells$lower_t),c("variable","group","x","lower_t","upper_t")])
  if(nrow(lim)&&nrow(lim)==nrow(unique(cells[,c("variable","group")]))){
    p<-p+ggplot2::geom_errorbar(data=lim,ggplot2::aes(x=x,ymin=lower_t,ymax=upper_t),inherit.aes=FALSE,width=0.7,linetype=2,color="#2166AC")
  }
  p+ggplot2::facet_wrap(~variable,scales="free",ncol=ncol)+
    ggplot2::labs(x=NULL,y="Value",color=NULL,size=NULL)+out_theme(base_size)
}

#' @export
gg_out_index<-function(cells,base_size=12,ncol=NULL,point_size=1.6){
  has_time<-!is.null(cells$time)&&inherits(cells$time,c("Date","POSIXt"))
  cells$xx<-if(has_time) cells$time else cells$index
  cells<-cells[!is.na(cells$value_t),,drop=FALSE]
  cells<-cells[order(cells$variable,cells$group,cells$xx),,drop=FALSE]
  grouped<-length(unique(cells$group))>1
  p<-ggplot2::ggplot(cells,ggplot2::aes(x=xx,y=value_t))
  if(any(!is.na(cells$lower_t))){
    p<-p+ggplot2::geom_ribbon(ggplot2::aes(ymin=lower_t,ymax=upper_t,group=group),fill="#2166AC",alpha=0.12)
  }
  p<-p+if(grouped) ggplot2::geom_point(ggplot2::aes(color=group),size=point_size*0.7,alpha=0.6) else ggplot2::geom_point(color="gray45",size=point_size*0.7,alpha=0.6)
  fl<-cells[cells$flag,,drop=FALSE]
  if(nrow(fl)) p<-p+ggplot2::geom_point(data=fl,color="#D7301F",size=point_size*1.4,shape=21,stroke=1.1)
  p+ggplot2::facet_wrap(~variable,scales="free_y",ncol=ncol)+
    ggplot2::labs(x=if(has_time) "Time" else "Observation (order in the Datalist)",y="Value",color=if(grouped) "Group" else NULL,
                  caption="Shaded band: limits. Red circles: flagged values.")+out_theme(base_size)
}

#' @export
gg_out_hist<-function(cells,base_size=12,ncol=NULL){
  cells<-cells[!is.na(cells$value_t),,drop=FALSE]
  p<-ggplot2::ggplot(cells,ggplot2::aes(x=value_t))+
    ggplot2::geom_histogram(bins=30,fill="#77AADD",color="white")
  lim<-unique(cells[!is.na(cells$lower_t),c("variable","group","lower_t","upper_t")])
  if(nrow(lim)&&nrow(lim)==nrow(unique(cells[,c("variable","group")]))){
    p<-p+ggplot2::geom_vline(data=lim,ggplot2::aes(xintercept=lower_t),linetype=2,color="#2166AC")+
      ggplot2::geom_vline(data=lim,ggplot2::aes(xintercept=upper_t),linetype=2,color="#2166AC")
  }
  fl<-cells[cells$flag,,drop=FALSE]
  if(nrow(fl)) p<-p+ggplot2::geom_rug(data=fl,ggplot2::aes(x=value_t),color="#D7301F",length=ggplot2::unit(0.06,"npc"),linewidth=0.8)
  p+ggplot2::facet_wrap(~variable,scales="free",ncol=ncol)+
    ggplot2::labs(x="Value",y="Count",caption="Dashed lines: limits. Red marks: flagged values.")+out_theme(base_size)
}

# which observations concentrate the flags
#' @export
gg_out_heat<-function(flags,vars,max_obs=60,base_size=12){
  validate(need(!is.null(flags)&&nrow(flags)>0,"No flagged values."))
  cnt<-sort(table(flags$id),decreasing=TRUE)
  top<-names(cnt)[seq_len(min(max_obs,length(cnt)))]
  f<-flags[flags$id%in%top,,drop=FALSE]
  f$id<-factor(f$id,levels=rev(top))
  f$variable<-factor(as.character(f$variable),levels=vars)
  f$Side<-factor(f$side,levels=c("low","high"))
  ggplot2::ggplot(f,ggplot2::aes(x=variable,y=id,fill=Side))+
    ggplot2::geom_tile(color="white")+
    ggplot2::scale_fill_manual(values=c(low="#2166AC",high="#D7301F"),drop=FALSE)+
    ggplot2::scale_x_discrete(drop=FALSE)+
    ggplot2::labs(x=NULL,y="Observation",title=if(length(cnt)>max_obs) paste0("The ",max_obs," observations with most flags") else NULL)+
    out_theme(base_size)+ggplot2::theme(axis.text.x=ggplot2::element_text(angle=45,hjust=1))
}

#' @export
gg_out_count<-function(summ,base_size=12){
  s<-stats::aggregate(cbind(Low,High)~Variable,summ,sum)
  long<-data.frame(Variable=rep(s$Variable,2),Side=factor(rep(c("low","high"),each=nrow(s)),levels=c("low","high")),n=c(s$Low,s$High))
  long$Variable<-factor(long$Variable,levels=unique(summ$Variable))
  ggplot2::ggplot(long,ggplot2::aes(x=Variable,y=n,fill=Side))+
    ggplot2::geom_col()+
    ggplot2::scale_fill_manual(values=c(low="#2166AC",high="#D7301F"))+
    ggplot2::labs(x=NULL,y="Flagged values")+out_theme(base_size)+
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle=45,hjust=1))
}

#' @export
gg_out_maha<-function(obs,view="distance",base_size=12,point_size=1.6){
  validate(need(!is.null(obs)&&nrow(obs)>0,"The distances could not be computed."))
  obs$Status<-factor(ifelse(obs$flag,"Flagged","Kept"),levels=c("Kept","Flagged"))
  grouped<-length(unique(obs$group))>1
  if(identical(view,"qq")){
    obs<-do.call(rbind,lapply(split(obs,obs$group),function(s){
      s<-s[order(s$d2),,drop=FALSE]
      s$theoretical<-stats::qchisq(stats::ppoints(nrow(s)),df=s$p[1])
      s
    }))
    p<-ggplot2::ggplot(obs,ggplot2::aes(x=theoretical,y=d2))+
      ggplot2::geom_abline(slope=1,intercept=0,linetype=2,color="gray50")+
      ggplot2::geom_point(ggplot2::aes(color=Status),size=point_size)+
      ggplot2::labs(x="Chi-square quantiles",y="Squared Mahalanobis distance",caption="Points far above the line deviate from multivariate normality.")
  } else{
    has_time<-!is.null(obs$time)&&inherits(obs$time,c("Date","POSIXt"))
    obs$xx<-if(has_time) obs$time else obs$index
    p<-ggplot2::ggplot(obs,ggplot2::aes(x=xx,y=distance))+
      ggplot2::geom_hline(ggplot2::aes(yintercept=cutoff),linetype=2,color="#2166AC")+
      ggplot2::geom_point(ggplot2::aes(color=Status),size=point_size)+
      ggplot2::labs(x=if(has_time) "Time" else "Observation (order in the Datalist)",y="Mahalanobis distance",caption="Dashed line: chi-square cutoff.")
  }
  p<-p+ggplot2::scale_color_manual(values=c(Kept="gray45",Flagged="#D7301F"),drop=FALSE)+out_theme(base_size)
  if(grouped) p<-p+ggplot2::facet_wrap(~group,scales="free")
  p
}

# original vs treated values of the changed variables
#' @export
gg_out_compare<-function(before,after,vars,base_size=12,ncol=NULL){
  vars<-vars[vars%in%colnames(before)]
  long<-rbind(
    do.call(rbind,lapply(vars,function(v) data.frame(variable=v,Data="Original",value=before[[v]]))),
    do.call(rbind,lapply(vars,function(v) data.frame(variable=v,Data="Treated",value=if(v%in%colnames(after)) after[[v]] else NA)))
  )
  long<-long[!is.na(long$value),,drop=FALSE]
  long$variable<-factor(long$variable,levels=vars)
  long$Data<-factor(long$Data,levels=c("Original","Treated"))
  ggplot2::ggplot(long,ggplot2::aes(x=Data,y=value,fill=Data))+
    ggplot2::geom_boxplot(width=0.55,outlier.size=1)+
    ggplot2::scale_fill_manual(values=c(Original="gray85",Treated="#9ECAE1"),guide="none")+
    ggplot2::facet_wrap(~variable,scales="free_y",ncol=ncol)+
    ggplot2::labs(x=NULL,y="Value")+out_theme(base_size)
}
