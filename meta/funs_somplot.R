
codes_corr_plot<-function(m,npic=10, indicate=c("var","cor"), col.arrow="gray80",cex.var=1, cex=1, pch=16, labels.ind=NULL,bg_palette="matlab.like2", factor.pal="gray", points=T,ncol=1,insetx=0,insety=0,alpha.legend=0.85){
  bmuvalues<-as.matrix(kohonen::unit.distances(m$grid,m$grid$toroidal))[,1]
  bmuvalues<-bmuvalues+seq(0.001,0.002,length.out=length(bmuvalues))
  maxdepth<-max(bmuvalues)
  colbmu <- getcolhabs(bg_palette,length(bmuvalues) )
  colcodes<-colbmu[cut(bmuvalues,breaks = bmuvalues )]
  colcodes[1]<-colbmu[1]
if(factor.pal%in%c("matlab.like2",'viridis', 'plasma',"Rushmore1","FantasticFox1","Blues","heat")){
  colfactors<-getcolhabs(factor.pal,nlevels(labels.ind))
  col<-colfactors[labels.ind]} else {
    colfactors<-getcolhabs(factor.pal,1)
    col<-colfactors
  }

  if(is.null(labels.ind)){
    colfactors<-getcolhabs(factor.pal,1)
    col<-colfactors

  }

  opar<-par(no.readonly=TRUE)
  shape="straight"
  indicate=match.arg(indicate,c("var","cor"))


  if(npic==0){
    par(mar=c(7,6,2,0))
    set.seed(1)
    if(isFALSE(points)){
      plot(m,"mapping",shape=shape, border="white", main="",keepMargins =T, bgcol=colcodes,codeRendering=F, labels=labels.ind, cex=cex, pch=pch, col=col)}else{

      plot(m,"mapping",shape=shape, border="white", main="",keepMargins =T, bgcol=colcodes,codeRendering=F, labels=NULL, cex=cex, pch=pch, col=col)
    }
  }


  if(npic>0){
    grid.size<-nrow(m$grid$pts)
    nb <- table(factor(m$unit.classif, levels=1:grid.size))
    CORMAP <- apply(do.call(cbind,m$codes),2,weighted.correlation,w=nb,grille=m)
    sigma2  <- sqrt(apply(do.call(cbind,m$codes),2,function(x,effectif){m2<-sum(effectif*(x- weighted.mean(x,effectif))^2)/(sum(effectif)-1)},effectif=nb))

    if(indicate=="cor"){
      indicadores<-names(sort(sigma2,decreasing=T))[1:npic]
      scores<-t(CORMAP)
      Xsp<-  scores[,1]
      Ysp<- scores[,2]
      A<- rowSums(abs(data.frame(S1=scores[names(which(Xsp<0&Ysp>0)),1],S2=scores[names(which(Xsp<0&Ysp>0)),2])))
      B<-rowSums(abs(data.frame(S1=scores[names(which(Xsp>0&Ysp>0)),1],S2=scores[names(which(Xsp>0&Ysp>0)),2])))
      C<-rowSums(abs(data.frame(S1=scores[names(which(Xsp<0&Ysp<0)),1],S2=scores[names(which(Xsp<0&Ysp<0)),2])))
      D<-rowSums(abs(data.frame(S1=scores[names(which(Xsp>0&Ysp<0)),1],S2=scores[names(which(Xsp>0&Ysp<0)),2])))
      Xsp1<-names(A)[order(A, decreasing = T)][1:npic]
      Xsp2<-names(B)[order(B, decreasing = T)][1:npic]
      Ysp1<-names(C)[order(C, decreasing = T)][1:npic]
      Ysp2<-names(D)[order(D, decreasing = T)][1:npic]
      indicadores<- unlist(lapply(data.frame(t(matrix(c(Xsp1,Xsp2,Ysp1,Ysp2),ncol=4))),cbind))
      indicadores= na.omit(indicadores[1:npic])

      result<-scores[indicadores,]
      colnames(result)<-c("cor.x", "cor.y")

    }
    if(indicate=="var") { indicadores<-na.omit(names(sort(sigma2,decreasing=T))[1:npic])
    result<-data.frame(sigma2[indicadores])
    colnames(result)<-"Variance"

    }
    bp=data.frame(na.omit(as.matrix(t(CORMAP))))
    par(mar=c(7,6,2,0))
    {
      set.seed(1)
      if(isFALSE(points)){ plot(m,"mapping",shape=shape, border="white", main="",keepMargins =T, bgcol=colcodes,codeRendering=F, labels=labels.ind, cex=cex, pch=pch, col=col)}else{
        plot(m,"mapping",shape=shape, border="white", main="",keepMargins =T, bgcol=colcodes,codeRendering=F, labels=NULL, cex=cex, pch=pch, col=col)
      }


      set.seed(NULL)
    }
    library("scales")
    bp[,1]<-  rescale(bp[,1], c(min(m$grid$pts[,1]), max(m$grid$pts[,1])))
    bp[,2]<-  rescale(bp[,2], c(min(m$grid$pts[,2]), max(m$grid$pts[,2])))
    boxtext(x =(bp[indicadores,1]), y = (bp[indicadores,2]), labels = colnames(CORMAP[,indicadores]), col.bg = adjustcolor("white", 0.5),  cex=cex.var, border.bg = col.arrow)
    if(factor.pal%in%c("matlab.like2",'viridis', 'plasma',"Rushmore1","FantasticFox1","Blues","heat")) {
      legend("topr",pch=pch,legend=levels(labels.ind), col=getcolhabs(factor.pal, nlevels(labels.ind)),border=NA, ncol=ncol, inset=c(insetx,insety),cex=cex, bg=adjustcolor('white',alpha.legend))

    }
    plotresult<-recordPlot()
    attr(plotresult,"indicadores")<-indicadores
    attr(plotresult,"result")<-result
    #plotind(m,indicadores)
    on.exit(par(opar),add=TRUE,after=FALSE)
    return(plotresult)

  }
}
pclus<-function(somC,  cex=1, pch=16, labels.ind=NULL,bg_palette="matlab.like2", factor.pal="gray", points=T,ncol=1,insetx=0,insety=0,alpha.legend=0.85){
  shape="straight"
  opar<-par(no.readonly=TRUE)
  m=somC$som.model
  som_cluster_adj=somC$som.hc
  col_vector=getcolhabs(bg_palette,somC$groups)
  if(factor.pal%in%c("matlab.like2",'viridis', 'plasma',"Rushmore1","FantasticFox1","Blues","heat")){
    colfactors<-getcolhabs(factor.pal,nlevels(labels.ind))
    col<-colfactors[labels.ind]} else {
      colfactors<-getcolhabs(factor.pal,1)
      col<-colfactors
    }

  set.seed(1)
  if(isFALSE(points)){
    plot(m, type="mapping", main = NULL, bgcol = col_vector[som_cluster_adj], pchs = pch,border="white",keepMargins =T,codeRendering=F,shape=shape,labels=labels.ind, cex=cex, col=col)
    add.cluster.boundaries(m, som_cluster_adj)}else{
      plot(m, type="mapping", main = NULL, bgcol = col_vector[som_cluster_adj], pchs = pch,border="white",keepMargins =T,codeRendering=F,shape=shape,labels=NULL, cex=cex, col=col)
      add.cluster.boundaries(m, som_cluster_adj)}
  if(factor.pal%in%c("matlab.like2",'viridis', 'plasma',"Rushmore1","FantasticFox1","Blues","heat")) {
    legend("topr",pch=pch,legend=levels(labels.ind), col=getcolhabs(factor.pal, nlevels(labels.ind)),border=NA, ncol=ncol, inset=c(insetx,insety),cex=cex, bg=adjustcolor('white',alpha.legend))

  }
  codes.plot<- recordPlot()
  on.exit(par(opar),add=TRUE,after=FALSE)
  return(codes.plot)

}




train.summary_function<-function(m)
{
  traindata <- data.frame(m$data[[1]])
  mean = round(mean(unlist(traindata)), 2)
  n.obs = nrow(traindata)
  n.variables = ncol(traindata)
  summ <- m$grid[-1]
  summ$neighbourhood.fct <- as.character(summ$neighbourhood.fct)
  summ <- do.call(rbind, summ)

  alpha = paste0(m$alpha, collapse = "; ")
  radius = paste0(round(m$radius, 3), collapse = "; ")
  user.weights = m$user.weights
  maxNA.fraction = m$maxNA.fraction
  dist.fcts = m$dist.fcts


  Parameters <-
    rbind(
      errors_som(m),
      n.obs,
      n.variables,
      summ,
      alpha,
      radius,
      user.weights,
      maxNA.fraction,
      dist.fcts

    )



  data.frame(Parameters)

}


errors_som<-function(m)
{
  aweerros<-aweSOM::somQuality(m,as.matrix(m$data[[1]]))[1:4]
  weights<-table(factor(m$unit.classif, levels=1:nrow(m$grid$pts)))
  aweerros$neu.uti<-  sum(weights==0)/nrow(m$grid$pts)
  return(round(do.call("rbind",aweerros),2))
}
pchanges<-function(m)
{
  opar<-par(no.readonly=TRUE)
  m$errorsom<-aweSOM::somQuality(m,as.matrix(m$data[[1]]))[1:4]
  weights<-table(factor(m$unit.classif, levels=1:nrow(m$grid$pts)))
  m$errorsom$neu.uti<-  sum(weights==0)/nrow(m$grid$pts)

  errossom<-round(unlist(m$errorsom),3)

  plot(m,"changes", col="red", keepMargins = TRUE)
  for( j in 1:ncol(m$changes)) {lines(m$changes[,j], col= rainbow(ncol(m$changes))[j])}
  #legend("bottoml", legend=c(paste("Quantization:",errossom[1]),paste("Topographic:",errossom[3]),paste("Explain_var:",errossom[2])), bty='n',cex=.7)
  pchanges <- recordPlot()
  on.exit(par(opar),add=TRUE,after=FALSE)
  return(pchanges)

}
pcodes<-function(somC,...)
{
  opar<-par(no.readonly=TRUE)
  sample.names<-gsub("Sd.*","",gsub(".*#","",rownames(somC$som.model$data[[1]])))

  m<-somC$som.model
  shape="straight"
  colhabs=somC$colhabs
  plot(somC$som.model, type="mapping", bgcol = somC$colhabs[somC$som.hc], pchs = 16,border="white",shape=shape,keepMargins =T, labels=sample.names,...)
  add.cluster.boundaries(m, somC$som.hc)
  pcodes<-recordPlot()
  on.exit(par(opar),add=TRUE,after=FALSE)
  return(pcodes)

}
phist<-function(data)
{
  opar<-par(no.readonly=TRUE)
  par(mar=c(5,5,5,1))
  hist(colSums(data, na.rm=T), main="")
  phist<-recordPlot()
  on.exit(par(opar),add=TRUE,after=FALSE)
  return(phist)
}
getGroups<-function(m,dist=NULL, type="som",group_method="kmeans",n.max=10){
  suppressWarnings({

    if(type=="som"){
      x<-kohonen::getCodes(m)
      x.dist=kohonen::object.distances(m,"codes")
    } else {
      x<-m
      x.dist=vegdist(m,dist)
    }

    indexes = c("kl", "ch", "hartigan", "ccc", "scott", "marriot", "trcovw", "tracew", "friedman", "rubin", "cindex", "db", "silhouette", "duda", "pseudot2", "beale", "ratkowsky", "ball", "ptbiserial", "gap", "frey", "mcclain", "gamma", "gplus", "tau", "dunn", "sdindex", "dindex", "sdbw")
    results_nb = list()
    safe_nb = safely(NbClust)
    times = list()
    withProgress(message = 'Calculation in progress',detail = 'This may take a while.... ', value = 0,
                 {
                   for(i in 1:length(indexes) ){
                     t = lubridate::now()
                     nb = safe_nb(as.matrix(x.dist),
                                  distance = "euclidean", method = group_method , min.nc = 2
                                  , max.nc = n.max
                                  , index = indexes[i])
                     results_nb[[i]] = nb$result$Best.nc[1]
                     incProgress(1/n.max)
                   }
                 }

    )

    votes<-table(unlist(results_nb))
    groups=as.numeric(names(votes)[which.max(votes)])
    res=list(votes,groups)
    return(res)
  })

}
getelbow<-function(m, k, type="som", method, dist="bray"){

  if(type=="som"){
    x<-kohonen::getCodes(m)
    x.dist=kohonen::object.distances(m,"codes")
  } else {
    #validate(need(anyNA(m)==F, "The selected data contains missing values. Please remove them in Upload>Transform."))
    x<-m
    x.dist=vegdist(m,dist)
  }
  ks=k
  x.clus <- hclust(x.dist, method=method)
  x.grps <- cutree(x.clus, 2:ks)
  TSS <- function(x, g) {
    sum(aggregate(x, by=list(g), function(x) sum(scale(x,
                                                       scale=FALSE)^2))[, -1])
  }

  TSS.all <- apply(x.grps, 2, function(g) TSS(x, g))
  res=data.frame(n_clusters=as.numeric(names(TSS.all)), TSS=TSS.all)

  return(res)



}
elbow_plot<-function(res, sugg=NULL)
{
  {par(mar=c(5,5,5,5))

    plot(res, type="n", ylab="Total within-cluster sum of squares", lwd=2, col="darkgreen", xlab="Number of Clusters", main="Elbow plot", xlim=c(2,nrow(res)),axes=F)
    axis(1)
    axis(2,col.axis="darkgreen",col="darkgreen")
    if(!is.null(sugg))
    {
      ws<-attr(sugg,"ws")
      points<-attr(sugg,"smw")
      abline(v=sugg, col='red', lty=2)
      legend("topr",legend=c(paste("Suggested breakpoint:",sugg),
                             paste("window size:", ws)), bty="n", cex=.8)



      par(new = TRUE)
      plot(points[,1],points[,2], type="l", col="darkcyan", ann=F, axes=F, xlim=c(2,nrow(res)), lty=2)
      axis(4,col.axis="darkcyan",col="darkcyan", lty=2)

      mtext("diss smw",4, line=3, col="darkcyan", crt=90)


    }

    par(new = TRUE)
    plot(res,type="l",lwd=3, col="darkgreen", ann=F, axes=F, xlim=c(2,nrow(res)))

  }

}
upbox<-function(x){
  updateBox(
    x,
    action = "update",
    options = list(
      collapsed = T
    )
  )
}
smw.root<-function(yo, w)
{

  if (w%%2 == 1) {
    stop("window size should be even")
  }
  diss <- NULL
  yo <- data.frame(yo)
  for (i in 1:(nrow(yo) - w + 1)) {
    wy.ord <- yo[i:(i + (w - 1)), ]
    half.a <- sum(wy.ord[1:(length(wy.ord)/2) ])
    half.b <- sum(wy.ord[-c(1:(length(wy.ord)/2))])
    d <- dist(rbind(half.a, half.b))
    diss[i] <- d
    k <- (w/2)
    for (i in 1:((nrow(yo) - w))) {
      k[i + 1] <- (w/2) + i
    }
  }
  result <- data.frame(positions = k, sampleID = rownames(yo)[k],
                       diss = diss)
  return(invisible(result))
}
smw_bp<-function(res,ws)
{

  yo<-res[2]
  smw<-smw.root(yo, ws)[,c(2,3)]
  sqs<-as.numeric(smw$diss>mean(smw$diss))
  re<-split(cbind(smw,sqs), cumsum(c(1, diff(sqs) !=0)))
  sq<-re[unlist(lapply(re, function(x) sum(x$sqs)!=0))][[1]]
  bp=sq[which.max(sq[,2]),1]

  groups<-re[unlist(lapply(re,function(x) sum(x$sig)!=0))]

  #bp=smw[which(smw$diss>mean(smw$diss))[1],1]

  attr(bp,"smw")<-smw
  attr(bp,"ws")<-ws
  return(bp)

}
piecewise<-function(res){
  yo<-res[2]
  rownames(yo)<-res[,1]

  half<-ceiling(nrow(yo)/2)


  if(half %%2==1){max=half+1} else{ max=half}

  ws=seq(2,max, by=2)


  {smw <- list()
    dptemp<-smw.root(yo, ws[1])[,c(2,3)]
    rownames(dptemp)<-dptemp[,1]
    dptemp[,1]<-NULL

    for (j in 2:length(ws)) {
      w1 <- ws[j]
      message("\n SMW analysis (", j, "/", length(ws), "); w =",
              w1, "\n")
      DPtable <- smw.root(yo, w1)
      rownames(DPtable)<-DPtable[,1]
      DPtable[,c(1,2)]<-NULL



      Dmean <- DPtable
      dptemp[rownames(Dmean),j]<-Dmean


    }
  }


  means<-apply(dptemp,1,mean,na.rm=T)
  plot(means)
  sigs<-mean(means)+sd(means)
  bp<-  as.numeric(names(which.max(means[means>sigs])))




  return(bp)

}
addbreaks<-function(sugg)
{
  abline(v=sugg, col='red', lty=2)
  legend("topr",legend=paste("Suggested breakpoint:",sugg), bty="n")

}
pUmatrix<-function(m)
{  opar<-par(no.readonly=TRUE)
shape="straight"
plot(m, type="dist.neighbours", main = "U-Matrix",border="white",shape=shape,keepMargins =T, palette.name=viridis)
pUmatrix<-recordPlot()
on.exit(par(opar),add=TRUE,after=FALSE)
return(pUmatrix)

}
pcounts<-function(m)
{  opar<-par(no.readonly=TRUE)
shape="straight"
plot(m, type="counts", main = "Counts",border="white",shape=shape,keepMargins =T)


pcounts<-recordPlot()
on.exit(par(opar),add=TRUE,after=FALSE)
return(pcounts)
}
coolBlueHotRed <- function(n, alpha = 1) {
  rainbow(n, end=4/6, alpha=alpha)[n:1]
}
pproperty<-function(m, main, pic)
{
  opar<-par(no.readonly=TRUE)
  shape="straight"
  plot(m,type="property",property=do.call(cbind,m$codes)[,pic],main=main,cex=0.5,shape=shape,keepMargins = TRUE, palette.name=coolBlueHotRed)
  pproperty<-recordPlot()
  on.exit(par(opar),add=TRUE,after=FALSE)
  return(pproperty)
}


pbox<-function(res, palette="viridis", coefic=1.5, lab_out=NULL ,cex.lab=1, lwd=1, main=paste0(colnames(res)[2],"~",colnames(res)[1]), ylim=range(na.omit(res[,2])), insetx=0, insety=0, show_outs=T)
{
  if(!is.null(lab_out)){show_outs=F}
  res<-data.frame(res)

  fac_o<-as.factor(res[,1])
  somprev <- as.factor(as.character(res[,1]))


  nlevels <- nlevels(somprev)
  colhabs =  getcolhabs(palette,nlevels(fac_o))
  names(colhabs)<-levels(fac_o)
  colhabs<-colhabs[as.character(levels(somprev))]
  boxout<- boxplot(
    res[, 2] ~ somprev,
    col = colhabs,
    ylab = colnames(res)[2],
    xlab = colnames(res)[1],
    las = 1,
    cex = cex.lab,
    bty = "n", plot=F, range=coefic
  )
  outliers<-boxout$out

  legend = levels(somprev)

  par(mar=c(5,5,5,10),fg="gray30", lwd=lwd)
  boxplot(
    res[, 2] ~ somprev,
    col = colhabs,
    ylab = colnames(res)[2],
    xlab = colnames(res)[1],
    las = 1,
    cex = 1.1,
    bty = "o",
    outline=show_outs, border=darken(colhabs, amount=0.5),pch=16,
    cex.lab = cex.lab,
    axes=F,main=main, ylim=ylim
  )
  axis(1, at=c(0.5,1:nlevels,nlevels+.5), labels=c("",1:nlevels,""), col="gray30",
       col.axis="gray30", cex.axis=cex.lab, lwd=lwd)
  axis(2, col="gray23",
       col.axis="gray30", cex.axis=cex.lab, lwd=lwd)

  if(length(boxout$out)>0){
    outliers<-which(res[,2] %in% c(boxout$out))

    if(length(outliers)>0){
      outs<-rownames(res)[outliers]
      if(is.data.frame(lab_out)){
        lab_outs<-as.character(lab_out[outs,])[order(as.numeric(lab_out[outs,]))]

        text(boxout$group,boxout$out, labels=as.character(lab_outs), col=  rep(darken(colhabs,0.5), table(factor(boxout$group, levels=levels(as.factor(as.numeric(somprev)))))), cex=cex.lab)
      }

    }
  }
  ncol_legend<-ceiling(length(legend)/10)

  if(palette%in%c("matlab.like2",'viridis', 'plasma',"Rushmore1","FantasticFox1","Blues","heat"))
  {legend("topr",
          legend = legend,
          col = colhabs,
          pch = 15,
          bty = "n",
          cex = cex.lab, ncol=ncol_legend, xpd=T, inset=c(insetx,insety), x.intersp=0.5
  )}


  plotbox<-recordPlot()
  attr(plotbox,"outliers")<-rownames(res)[outliers]
  return(plotbox)

}

