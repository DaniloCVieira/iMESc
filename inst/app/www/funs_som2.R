## Copyright © 2023 [Danilo Candido Vieira]
## Licensed under the CC BY-NC-ND 4.0 license.

supersomQuality_whatmap<-function (som,whatmap)
{
  #it uses cbind

  codes<-som$codes[[whatmap]]
  traindat<-som$data[[whatmap]]

  if (is.null(som))
    return(NULL)
  ok.dist <- somDist(som)
  bmu <- som$unit.classif
  sqdist <- rowSums((traindat - codes[bmu, ])^2, na.rm = TRUE)
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
qe_class<-function(m,somC){
  qes<-sapply(names(m$data),function(whatmap){
    intra_var(m,whatmap,somC)
  })
  # apply(qes,1,function(x) weighted.mean(x,m$distance.weights))
  qes
}
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


