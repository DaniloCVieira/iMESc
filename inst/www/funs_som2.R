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
qe_class<-function(m,somC,whatmap=NULL){
  if(is.null(whatmap)){
    whatmap<-names(m$data)
  }
  layers<-names(m$data)[names(m$data)%in%whatmap]
  qes<-sapply(layers,function(whatmap){
    intra_var(m,whatmap,somC)
  })
  # apply(qes,1,function(x) weighted.mean(x,m$distance.weights))
  qes
}
#' @export
screeplot_som<-function(m,k.max,hc_fun,hc_method, session = getDefaultReactiveDomain(),whatmap=NULL,use_weights=F) {
  t0<-Sys.time()
  if(is.null(whatmap)){
    whatmap<-names(m$codes)
  }
  if(length(m$data)==1){
    whatmap<-NULL
  }

  withProgress(min=2,max=k.max,session=session,{

    rel<-lapply(1:k.max,function(k) {

      incProgress(1,message=paste0(round(k/k.max,2)*100,'%'), session=session)
      message(k)
      somC_temp<-cutsom_new(m,k,hc_fun,hc_method,whatmap=whatmap,use_weights=F)
      if(k==1){ q<-colMeans(data.frame(as.list(
        qe_class(m,somC_temp,whatmap=whatmap)
      )))} else{
        q<-colMeans(qe_class(m,somC_temp,whatmap = whatmap))
      }
      weights<-rep(1,length(whatmap))
      if(isTRUE(use_weights)){
        weights<-m$user.weights
        pic_w<-names(m$codes)%in%whatmap
        weights<-weights[pic_w]
      }
      if(length(weights)==0){
        weights=1
      }

      qe<-weighted.mean(q,weights)
      data.frame(k=k,qe)
    })
  })

  t1<-Sys.time()
  message(t1-t0)
  data.frame(do.call(rbind,rel))
}



