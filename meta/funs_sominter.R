

cutdata<-function(data,  method.hc="ward.D2", dist="bray", col=NULL)
{
  #validate(need(anyNA(data)==F, "The selected data contains missing values. Please remove them in Upload>Transform."))
  d<-vegdist(data, method=dist)
  hc<-hclust(d,method=method.hc)
  hc

  return(hc)
}

cutm<-function(m,  method.hc="ward.D2")
{
  codes<-kohonen::getCodes(m)
  d=kohonen::object.distances(m,"codes")
  hc<-hclust(d,method=method.hc)
  return(hc)
}




cutsom<-function(m,groups, members=NULL, method.hc="ward.D2", palette="matlab.like2")
{


  codes<-kohonen::getCodes(m)
  weights=table(factor(m$unit.classif, levels=1:nrow(codes)))
  if(is.null(members)==F){members=weights}
  d=kohonen::object.distances(m,"codes")
  hc<-hclust(d,method=method.hc,members=members)

  hcut<-cutree( hc, groups)
  newclass<-m$unit.classif
  for(i in 1:length(hcut)) {newclass[newclass==i]<-rep(hcut[i],sum(  newclass==i))}


  names(newclass)<-rownames(m$data[[1]])
  pred<-newclass
  colhabs<-getcolhabs(palette,groups)

  col_vector2=c(colors_bmu(m))
  res<-list(groups,pred,result=NULL, hcut )
  somC<-list(somC=pred, colhabs=colhabs,som.model=m,som.hc=hcut, groups=groups, colunits=col_vector2,cluster.result=NULL, hc.object=hc)
  class(somC)<-"somC"
  return(somC)

}

cutdata2<-function(data, groups, method.hc="ward.D2", dist="bray", palette="matlab.like2")
{
  #validate(need(anyNA(data)==F, "The selected data contains missing values. Please remove them in Upload>Transform."))
  d<-vegdist(data, method=dist)
  hc<-hclust(d,method=method.hc)
  pred<-hcut<-cutree( hc, groups)
  names(pred)<-rownames(data)
  colhabs<-getcolhabs(palette,groups)
  m=c()
  m[[1]]<-data
  somC<-list(somC=pred, colhabs=colhabs,som.model=m,som.hc=hcut, groups=groups,hc.object=hc)
  return(somC)
}


hc_plot<-function(somC, col=NULL)
{
  opar<-par(no.readonly=TRUE)
  library("gplots")
  library(dendextend)

  hc <- as.hclust(as.dendrogram(somC$hc.object))

  hc.dendo <- as.dendrogram(somC$hc.object)
  my_5_cluster <-cutree(hc, k=somC$groups)
  clust.cutree <- dendextend:::cutree(hc.dendo, k=somC$groups, order_clusters_as_data = FALSE)

  idx <- order(names(clust.cutree))
  clust.cutree <- clust.cutree[idx]
  df.merge <- merge(my_5_cluster,clust.cutree,by='row.names')
  df.merge.sorted <- df.merge[order(df.merge$y),]
  lbls<-unique(df.merge.sorted$x)
  if(is.null(col)){ color=somC$colhabs[lbls]
  } else {color=col}

  dend1 <- color_branches(hc.dendo, col=color,k = somC$groups, groupLabels = lbls,labels_colors)
  if(is.null(col)){
    colors_dend1<- somC$colhabs[my_5_cluster[ as.numeric(labels(dend1))]]
  } else {colors_dend1="gray80" }

  labels_colors(dend1)<-colors_dend1
  plot(dend1,main="Cluster Dendogram")

  phc <- recordPlot()
  on.exit(par(opar),add=TRUE,after=FALSE)
  return(phc)

}



Kdata<-function(x){
  res<-ceiling((5*sqrt(nrow(x)))/3)
  if(res>15){res<-15}
  res
}



Kmodel<-function(x){
  res<-ceiling(nrow(getCodes(x))/3)
  if(res>15){res<-15}
  res
}



topology<-function(df,dist="BrayCurtis") {
  df=data.frame(na.omit(df))
  if(length(attr(df,"scaled:scale"))<1)
  {
    df=scale(df)
  }
  #m<-supersom(as.matrix(df), grid=somgrid(2,2), rlen=1, dist.fcts=dist)
  df<-data.frame(df)*100
  N=floor(5*sqrt(nrow(df)))
  #ev <- eigen(object.distances(m,"data"))
  ev <- eigen(cov(as.matrix(df)))
    ratio_y<-ev$values[order(ev$values, decreasing = T)[2]]/abs(ev$values[order(ev$values, decreasing = T)[1]])
  ratio_x<-ev$values[order(ev$values, decreasing = T)[1]]/ev$values[order(ev$values, decreasing = T)[2]]
  y=floor(sqrt(N/ratio_y))
  x=ceiling(sqrt(N/ratio_x))
  res<-c(units=N,x=y,y=x)
  input<-res
  units<-input[1]
  gridtopo=input[2:3]
  Ydoble<-input[3]*2
  names(gridtopo)<-NULL
  prop<-gridtopo[1]/gridtopo[2]
  return(res)
}



weighted.correlation <- function(v,w,grille)
{

  x <- grille$grid$pts[,"x"]
  y <- grille$grid$pts[,"y"]
  mx <- weighted.mean(x,w)
  my <- weighted.mean(y,w)
  mv <- weighted.mean(v,w)
  numx <- sum(w*(x-mx)*(v-mv))
  denomx <- sqrt(sum(w*(x-mx)^2))*sqrt(sum(w*(v-mv)^2))
  numy <- sum(w*(y-my)*(v-mv))
  denomy <- sqrt(sum(w*(y-my)^2))*sqrt(sum(w*(v-mv)^2)) #correlation for the two axes
  res <- c(numx/denomx,numy/denomy)
  return(res)
}



colors_bmu<-function(m)
{
  fun_xy <- function(x, y){
    R <- (x+1)/2
    G <- (1-x)/2
    B <- (y+1)/2
    A <- 1- 0.5*exp(-(x^2+y^2)/0.2)
    rgb(R, G, B, A)}
  z <- outer(seq(-1,1,length=m$grid$xdim), seq(-1,1,length=m$grid$ydim), FUN = fun_xy)
  return(z)
}




boxtext <- function(x, y, labels = NA, col.text = NULL, col.bg = NA,
                    border.bg = NA, adj = 1, pos = 4, offset = 0,
                    padding = c(0.5, 0.5), cex = 1, font = par('font')){

  ## The Character expansion factro to be used:
  theCex <- par('cex')*cex

  ## Is y provided:
  if (missing(y)) y <- x

  ## Recycle coords if necessary:
  if (length(x) != length(y)){
    lx <- length(x)
    ly <- length(y)
    if (lx > ly){
      y <- rep(y, ceiling(lx/ly))[1:lx]
    } else {
      x <- rep(x, ceiling(ly/lx))[1:ly]
    }
  }
  strheight
  ## Width and height of text
  textHeight <- strheight(labels, cex = theCex, font = font)
  textWidth <- strwidth(labels, cex = theCex, font = font)

  ## Width of one character:
  charWidth <- strwidth("e", cex = theCex, font = font)

  ## Is 'adj' of length 1 or 2?
  if (!is.null(adj)){
    if (length(adj == 1)){
      adj <- c(adj[1], 0.5)
    }
  } else {
    adj <- c(0.5, 0.5)
  }

  ## Is 'pos' specified?
  if (!is.null(pos)){
    if (pos == 1){
      adj <- c(0.5, 1)
      offsetVec <- c(0, -offset*charWidth)
    } else if (pos == 2){
      adj <- c(1, 0.5)
      offsetVec <- c(-offset*charWidth, 0)
    } else if (pos == 3){
      adj <- c(0.5, 0)
      offsetVec <- c(0, offset*charWidth)
    } else if (pos == 4){
      adj <- c(0, 0.5)
      offsetVec <- c(offset*charWidth, 0)
    } else {
      stop('Invalid argument pos')
    }
  } else {
    offsetVec <- c(0, 0)
  }

  ## Padding for boxes:
  if (length(padding) == 1){
    padding <- c(padding[1], padding[1])
  }

  ## Midpoints for text:
  xMid <- x + (-adj[1] + 1/2)*textWidth + offsetVec[1]
  yMid <- y + (-adj[2] + 1/2)*textHeight + offsetVec[2]

  ## Draw rectangles:
  rectWidth <- textWidth + 2*padding[1]*charWidth
  rectHeight <- textHeight + 2*padding[2]*charWidth
  rect(xleft = xMid - rectWidth/2,
       ybottom = yMid - rectHeight/2,
       xright = xMid + rectWidth/2,
       ytop = yMid + rectHeight/2,
       col = col.bg, border = border.bg)

  ## Place the text:
  text(xMid, yMid, labels, col = col.text, cex = theCex, font = font,
       adj = c(0.5, 0.5))

  ## Return value:
  if (length(xMid) == 1){
    invisible(c(xMid - rectWidth/2, xMid + rectWidth/2, yMid - rectHeight/2,
                yMid + rectHeight/2))
  } else {
    invisible(cbind(xMid - rectWidth/2, xMid + rectWidth/2, yMid - rectHeight/2,
                    yMid + rectHeight/2))
  }
}


train.summary_fun<- function(m){
  {

    traindata <- data.frame(m$data[[1]])
    mean = round(mean(unlist(traindata)), 2)
    n.obs = nrow(traindata)
    n.variables = ncol(traindata)
    summ <- m$grid[-1]
    summ$neighbourhood.fct <- as.character(summ$neighbourhood.fct)
    summ <- do.call(rbind, summ)
    mode <-attr(m,"mode")
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
        dist.fcts,
        mode
      )



    data.frame(Parameters)

  }
}
