
singles<-function(data){
if(sum(apply(data,2, is.integer), na.rm=T)==0){
  stop("This option requires a counting data. Please ulpload a new data set for this functionality")
}



  data0<-decostand(data,"pa", na.rm=T)
  remove<-colSums(data0, na.rm=T)==1
  if(sum(remove, na.rm=T)>0){return(  data[,-  which(remove)]) } else {return(data)}


}

pctRares<-function(data, pct=1){
  pct=pct/100
  remove<-colSums(data, na.rm=T)<(sum(data, na.rm=T)*pct)
  if(sum(remove, na.rm=T)>0){return(  data[,-  which(remove)]) } else {return(data)}
}

pctPrev<-function(data, pct)
{

  data0<-decostand(data,"pa", na.rm=T)
  remove<-colSums(data0, na.rm=T)<= round(nrow(data0)*pct)
  if(sum(remove, na.rm=T)>0){return(  data[,-  which(remove)]) } else {return(data)}


}

