#' @export
plot_ridges<-function(data,fac,palette,newcolhabs,ncol=3, title="",base_size=11){
  col<-getcolhabs(newcolhabs,palette,nlevels(data$class))
  df<-data.frame(id=rownames(data),y=fac,data)
  df<-reshape2::melt(data,'class')
  ggplot(df, aes(x = value, y = class)) +
    ggridges::geom_density_ridges(aes(fill = class),show.legend = T) +
    scale_fill_manual(values = c(col))+
    ggtitle(NULL)+facet_wrap(~variable,ncol=ncol)+
    guides(fill=guide_legend(title=fac))+ggtitle(title)+theme(
      strip.text.x = element_text(size = base_size),
      strip.text.y = element_text(size = base_size),
      axis.text=element_text(size=base_size),
      axis.title=element_text(size=base_size),
      plot.title=element_text(size=base_size),
      plot.subtitle=element_text(size=base_size,face="italic"),
      legend.text=element_text(size=base_size),
      legend.title=element_text(size=base_size),
    )


}

#' @export

ggbox<-function(res,pal,violin=F,horiz=F,base_size=12,cex.axes=11,cex.lab=1,
                cex.main=1,xlab=colnames(res)[1],ylab=colnames(res)[2],main="",
                box_linecol="firebrick",box_alpha=0.7,newcolhabs,cex.label_panel=10,varwidth=F, linewidth=.8, theme='theme_bw', grid=T, background="white",xlab_rotate=0,ylab_rotate=0,nrow=NULL,ncol=2,box_title_font="italic",subtitle=NULL,cex.subtitle=10,              box_subtitle_font="plain") {

  #res0<-res
  #res$group<-NULL
  wrap=F
  if(is.na(nrow)){
    nrow=NULL
  }
  if(is.na(ncol)){
    ncol=NULL
  }
  ggr<-NULL
  if(ncol(res)>2){
    res2<-res
    colnames(res2)[1]<-c("x")
    if("group"%in%colnames(res2)){
      res2<-reshape2::melt(res2,c("x","group"))
      ggr<-res2$group
      res2$group<-NULL
    } else{
      res2<-reshape2::melt(res2,"x")

    }

    colnames(res2)[3]<-"y"
    res<-res2
    wrap=T

  } else{
    colnames(res)<-c("x","y")
  }

  res$group<-ggr
  coline<-box_linecol
  cols<-newcolhabs[[pal]](nlevels(res$x))
  cols<-colorspace::lighten(cols,box_alpha)
  p<-ggplot(res, aes(x=x, y=y, fill=x))
  if(isTRUE(violin)){
    p<-p+geom_violin(color=coline)
  } else{
    p<-p+stat_boxplot(geom='errorbar', linetype=1, width=0.3,color=coline)+
      geom_boxplot(fill="white")+  geom_boxplot(varwidth =varwidth,size=linewidth,color=coline)
  }
  p<-p+
    scale_fill_manual(values=cols)




  p<-switch(theme,
            'theme_grey'={p+theme_grey(base_size)},
            'theme_bw'={p+theme_bw(base_size)},
            'theme_linedraw'={p+theme_linedraw(base_size)},
            'theme_light'={p+theme_light(base_size)},
            'theme_dark'={p+theme_dark(base_size)},
            'theme_minimal'={p+theme_minimal(base_size)},
            'theme_classic'={p+theme_classic(base_size)},
            'theme_void'={p+theme_void(base_size)})


  if(isFALSE(grid)){
    p<-p+theme(panel.grid=element_blank())
  }
  #theme(panel.background=element_rect(fill=NA, color=background))

  p<-p +
    ggtitle(main,subtitle=subtitle) +
    xlab(xlab)+ylab(ylab)+ theme(
      legend.position="none",

      #panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"),
      strip.text.x = element_text(size = cex.label_panel,face=box_title_font),
      axis.line=element_line(),
      axis.text=element_text(size=cex.axes),
      axis.title=element_text(size=cex.lab),
      plot.title=element_text(size=cex.main,face=box_title_font),
      plot.subtitle=element_text(size=cex.subtitle,face=box_subtitle_font),
      axis.text.x = element_text(angle = xlab_rotate,vjust = .5, hjust = .5),
      axis.text.y = element_text(angle = ylab_rotate,vjust = .5, hjust = .5)

    )

  if(isTRUE(horiz)){
    p<-p+coord_flip()

  }
  p<-p+
    scale_y_continuous(labels = scales::label_number(big.mark = ",", decimal.mark = "."))

  if(!is.null(ggr)){
    p<-p+facet_wrap(~interaction(group,variable), scales = "free_y",nrow=nrow,ncol=ncol)
  } else{
    if(isTRUE(wrap)){

      p<-p+facet_wrap(~variable, scales = "free_y",nrow=nrow,ncol=ncol)
    }
  }



  p
}
#' @export
cordata_filter<-function(data,cor_method="pearson",cor_cutoff=0.75,cor_use='na.or.complete', ret="lower"){
  if(is.null(cor_cutoff))
    cor_cutoff<-1
  met<-match.arg(cor_method,c("pearson", "kendall", "spearman"))

  pic<-which(apply(data,2,function(x) var(x,na.rm=T))==0)
  if(length(pic)>0){
    datatemp<-data
    #datatemp[is.na(datatemp)]<-0
    datatemp[colnames(data)[pic]]<-NULL
    data<-datatemp
  }


  if(ret=="all"){
    cordata<-cor(data, use=cor_use,method =met)
    return(cordata)
  }

  if(cor_cutoff==1){
    cordata<-cor(data, use=cor_use,method =met)
    return(cordata)
  }

  cordata<-cor(data)
  pic<-caret::findCorrelation(cordata,
                       cutoff = cor_cutoff, # the absolute value of a correlation we'd deem as high
                       verbose = T,
                       names = F,
                       exact = T)




  if(ret=="lower"){
    if(!length(pic)>0){
      attr(cordata,"war")<-paste0("All correlations < =",cor_cutoff)
      return(cordata)
    }
    data.new<-data[,-pic]
  } else{
    if(!length(pic)>0){
      attr(cordata,"war")<-paste0("Note: All correlations > =",cor_cutoff)
      return(cordata)
    }
    data.new<-data[,pic]
  }

  cordata<-cor(data.new, use=cor_use,method =met)

  cordata

}
#' @export
i_corplot<-function(cordata,newcolhabs,cor_palette,cor_sepwidth_a,
                    cor_sepwidth_b,cor_notecex,cor_noteco,cor_na.color,
                    cor_sepcolor,cor_dendogram,
                    cor_scale,cor_Rowv,cor_Colv,cor_revC,
                    cor_na.rm,cor_labRow,cor_labCol,cor_cellnote,cor_density.info, margins=c(5,5)) {

  req(class(cordata)[1]=="matrix")
  sepwidth=c(cor_sepwidth_a,cor_sepwidth_b)

  # hmet=match.arg(cor_hclust_method,c('ward.D','ward.D2','single','complete','average','mcquitty','median','centroid'))
  # hdist<-match.arg(cor_distance,c('euclidean','bray','jaccard','hellinger'))
  dend<-match.arg(cor_dendogram,c("both","row","column","none"))
  sca_de<-match.arg(cor_scale,c("none","row", "column"))
  Rowv<-as.logical(match.arg(cor_Rowv,c('TRUE','FALSE')))
  Colv<-match.arg(cor_Colv,c('Rowv',T,F))
  revC<-as.logical(match.arg(cor_revC,c('TRUE','FALSE')))
  na.rm<-as.logical(match.arg(cor_na.rm,c('TRUE','FALSE')))

  labRow<-as.logical(match.arg(cor_labRow,c('TRUE','FALSE')))
  labCol<-as.logical(match.arg(cor_labCol,c('TRUE','FALSE')))

  labRow<-if(isTRUE(labRow)){
    labRow<-NULL
  } else{
    labRow<-NA
  }
  labCol<-if(isTRUE(labCol)){
    labCol<-NULL
  } else{
    labCol<-NA
  }
  cellnote<-as.logical(match.arg(cor_cellnote,c('TRUE','FALSE')))

  if(isTRUE(cellnote)){
    cellnote<-round(cordata,2)
  } else{
    cellnote<-matrix(rep("",length(cordata)), nrow(cordata), ncol(cordata))
  }


  #x11()

  ncex=cor_notecex*20/nrow(cordata)


  gplots::heatmap.2(cordata,
            Rowv=Rowv,
            Colv=Colv,
            margins = margins,
            na.rm=na.rm,
            revC=revC,
            dendrogram = dend,
            col=newcolhabs[[cor_palette]],
            labRow=labRow,
            labCol=labCol,
            sepcolor=cor_sepcolor,
            sepwidth=sepwidth,
            cellnote=cellnote,
            notecex=ncex,
            notecol=cor_noteco,
            na.color=cor_na.color,trace='none',
            density.info=cor_density.info,
            key.title = "Correlation",
            denscol = "black",
            linecol = "black")

}


#' @export
mergedatacol<-function(datalist,rm_dup=T){
  {

    to_merge<-datalist

    to_merge_fac<-lapply(datalist,function(x) attr(x,"factors"))
    mx <- which.max(do.call(c,lapply(to_merge,nrow)))
    newmerge<-data.frame(id=rownames(to_merge[[mx]]))
    rownames(newmerge)<-newmerge$id
    l1<-unlist(lapply(to_merge,function(x){
      x[rownames(newmerge),, drop=F]
    }),
    recursive = F)
    newdata<-data.frame(l1)

    rownames(newdata)<-rownames(to_merge[[mx]])
    colnames(newdata)<-c(do.call(c,lapply(to_merge,colnames)))

    if(isTRUE(rm_dup)) {
      if(any(duplicated(colnames(newdata)))){
        dup<-which(duplicated(colnames(newdata)))
        keep<-which.max(do.call(c,lapply(lapply(newdata[dup],na.omit),length)))
        fall<-dup[-keep]
        newdata<-newdata[colnames(newdata)[-fall]]
      }
    }



    mxfac <- which.max(do.call(c,lapply(to_merge_fac,nrow)))
    newmerge_fac<-data.frame(id=rownames(to_merge_fac[[mxfac]]))
    rownames(newmerge_fac)<-newmerge_fac$id
    l2<-unlist(lapply(to_merge_fac,function(x){
      x[rownames(newmerge_fac),, drop=F]
    }),
    recursive = F)
    newfac<-data.frame(l2)

    rownames(newfac)<-rownames(to_merge_fac[[mx]])
    colnames(newfac)<-c(do.call(c,lapply(to_merge_fac,colnames)))
    if(isTRUE(rm_dup)){
      if(any(duplicated(colnames(newfac)))){
        dup<-which(duplicated(colnames(newfac)))
        keep<-which.max(do.call(c,lapply(lapply(newfac[dup],na.omit),length)))
        fall<-dup[-keep]
        newfac<-newfac[colnames(newfac)[-fall]]
      }
    }

    newdata<-data_migrate(to_merge[[mx]],newdata,"")
    attr(newdata, "transf")=NULL
    attr(newdata,"factors")<-newfac[rownames(newdata),,drop=F]
    newdata
  }
}
#' @export
getcol_missing<-function(data){
  res0<-res<-which(is.na(data), arr.ind=TRUE)

  for(i in 1:nrow(res)){
    res0[i,1]<-rownames(data)[res[i,1]]
    res0[i,2]<-colnames(data)[res[i,2]]
  }
  colnames(res0)<-c("ID","Variable")
  rownames(res0)<-NULL
  res<-data.frame( table(res0[,2]))
  colnames(res)<-c("Variable","Missing")
  rownames(res)<-res[,1]
  res
}
#' @export
getrow_missing<-function(data){
  res0<-res<-which(is.na(data), arr.ind=TRUE)

  for(i in 1:ncol(res)){
    res0[i,1]<-rownames(data)[res[i,1]]
    res0[i,2]<-colnames(data)[res[i,2]]
  }
  colnames(res0)<-c("ID","Variable")

  res<-data.frame( table(rownames(res0)))
  colnames(res)<-c("Variable","Missing")
  rownames(res)<-res[,1]
  res
}







getXb<-function(x.ord,BPs){
  x.ord <- as.matrix(x.ord)
  n <- nrow(x.ord)
  k <- ncol(x.ord)
  bks <- c(0, BPs, nrow(x.ord))
  nBPs <- length(bks) - 1
  Xb <- matrix(0, ncol = nBPs * k, nrow = n)
  for (i in 1:(nBPs)) {
    Xb[(bks[i] + 1):bks[i + 1], ((k * (i - 1)) + 1):((k *
                                                        (i - 1)) + k)] <- x.ord[(bks[i] + 1):bks[i + 1],
                                                        ]
  }


  colnames(Xb)<-sapply(1:nBPs,function(x) paste0(colnames(x.ord),"_",x))
  set.seed(1)
  Xb <- jitter(Xb)
  Xb<-data.frame(Xb)
  rownames(Xb)<-rownames(x.ord)
  Xb
}
#' @export
pwRDA.source2<-function (x.ord, y.ord, BPs)
{

  x.ord<-as.matrix(x.ord)
  Xb<-getXb(x.ord,BPs)

  Xbc = scale(Xb, center = T, scale = F)
  y.ord<-data.frame(y.ord)
  x.ord<-data.frame(x.ord)


  rda.0 <- vegan::rda(y.ord~., data=x.ord)
  rda.pw <- vegan::rda(y.ord ~ ., Xb)

  Yc = scale(y.ord, center = T, scale = F)


  Y.avg = matrix(rep(apply(Yc, 2, mean), times = nrow(Yc)),
                 ncol = ncol(Yc), byrow = T)
  B.pw = solve(t(Xbc) %*% Xbc) %*% (t(Xbc) %*% Yc)
  coord <- rda.pw$CCA$biplot
  bew.bp <- t(cor(coord, t(cor(x.ord, Xb))))
  rda.pw$CCA$biplot <- bew.bp
  Ypred.pw = Xbc %*% B.pw
  Yres <- Yc - Ypred.pw
  TSS.pw = sum((Yc)^2)
  RSS.pw = sum((Ypred.pw - Y.avg)^2)
  r2.pw <- RSS.pw/TSS.pw
  n.pw <- nrow(Xbc)
  k.pw <- ncol(Xbc)
  Radj.pw <- 1 - ((1 - r2.pw) * ((n.pw - 1)/(n.pw - k.pw -
                                               1)))
  Xc = scale(jitter(as.matrix(x.ord)), center = T, scale = F)
  B.full = solve(t(Xc) %*% Xc) %*% (t(Xc) %*% Yc)
  Ypred.full = Xc %*% B.full
  Yres <- Yc - Ypred.full
  TSS.full = sum((Yc)^2)
  RSS.full = sum((Ypred.full - Y.avg)^2)
  r2.full <- RSS.full/TSS.full
  n.full <- nrow(Xc)
  k.full <- ncol(Xc)
  Radj.full <- 1 - ((1 - r2.full) * ((n.full - 1)/(n.full -
                                                     k.full - 1)))
  F.stat <- ((RSS.full - RSS.pw)/(k.full - k.pw))/(RSS.full/(n.pw -
                                                               k.full))
  dg1 <- k.pw - k.full
  dg2 <- n.pw - k.pw
  F.stat <- ((RSS.pw - RSS.full)/(dg1))/(RSS.pw/(dg2))
  p.value <- 1 - pf(F.stat, dg1, dg2, lower.tail = T)
  summ <- c(Radj.full = Radj.full, Radj.pw = Radj.pw, F.stat = F.stat,
            p.value = p.value)
  pw <- list(summ = summ, rda.0 = rda.0, rda.pw = rda.pw)
  class(pw) <- "pw"
  return(invisible(pw))
}
#' @export
pwRDA2<-function (x.ord, y.ord, BPs, n.rand = 99){
  x.ord <- as.matrix(x.ord)
  y.ord <- as.matrix(y.ord)
  if (is.null(rownames(x.ord))) {
    rownames(x.ord) <- 1:nrow(x.ord)
  }
  if (is.null(rownames(y.ord))) {
    rownames(y.ord) <- 1:nrow(y.ord)
  }
  if (is.null(colnames(x.ord))) {
    colnames(x.ord) <- 1:ncol(x.ord)
  }
  if (is.null(colnames(y.ord))) {
    colnames(y.ord) <- 1:ncol(y.ord)
  }
  R.boot <- NULL
  pw.Models <- pwRDA.source2(x.ord, y.ord, BPs)
  pw.obs <- pw.Models$summ
  obs <- pw.obs[2]
  rownames(x.ord) <- NULL
  rownames(y.ord) <- NULL

  withProgress(message = "Running...",
               min = 1,
               max = n.rand,
               {

                 for (b in 1:n.rand) {

                   sample <- sample(1:nrow(y.ord), replace = T)
                   suppressWarnings(comm.rand <- y.ord[sample, ])
                   suppressWarnings(new.x <- x.ord[sample, ])
                   R.boot[b] <- pwRDA.source2(new.x, comm.rand, BPs)$summ[2]

                   incProgress(1)
                 }
               })

  p.value <- pnorm(obs, mean = mean(R.boot), sd = sd(R.boot),
                   lower.tail = F)
  summ <- rbind(c(pw.obs[1], anova(pw.Models$rda.0)[1, 4]),
                c(pw.obs[2], p.value), c(pw.obs[3], pw.obs[4]))
  summ <- round(summ, 10)
  rownames(summ) <- c("FULL", "PW", "F")
  colnames(summ) <- c("Statistic", "P.value")

  pw.Models[[1]] <- summ
  class(pw.Models) <- "pw"
  return(invisible(pw.Models))
}

#' @export
smw.root2<-function (yo, w=50, dist="bray"){
  if (w%%2 == 1) {
    stop("window size should be even")
  }
  diss <- NULL
  yo <- data.frame(yo)
  nrow_yo=nrow(yo)
  yo<-data.frame(t(yo))
  i=1
  for (i in 1:(nrow_yo - w + 1)) {
    wy.ord <- yo[i:(i + (w - 1))]
    div<-length(wy.ord)/2
    half.a <- apply(wy.ord[1:(div)], 1, sum)
    half.b <- apply(wy.ord[-c(1:(div))], 1,sum)
    d <- vegan::vegdist(rbind(half.a, half.b), dist)
    diss[i] <- d

  }
  k <- (w/2)
  for (i in 1:((nrow_yo - w))) {
    k[i + 1] <- (w/2) + i
  }
  positions<-k

  result<-data.frame(positions =positions, sampleID = colnames(yo)[positions],
                     diss = diss)

  return(invisible(result))
}
prepare_factors<-function(factors, width=.8){
  df<-do.call(rbind,lapply(1:ncol(factors),function(i){
    x<-factors[,i]
    tt<-table(x)
    n=as.vector(tt)
    levels=names(tt)
    labels=paste0(levels)
    dd<-data.frame(factor=colnames(factors)[i],
                   level=levels,
                   nobs=as.vector(n),
                   labels=labels)

    dd
  }))
  df <- df[order(df$factor, -as.numeric(as.factor(df$level))),]
  df$factor<-factor(df$factor, levels=rev(colnames(factors)))
  df$position <- ave(df$nobs, df$factor, FUN = function(x){
    res<-cumsum(x)
    c(0,res[-length(res)])
  })
  df$label_position_top <- df$position
  li<-split(df,df$factor)
  newl<-new_limits(1:ncol(factors),width)
  ggfactors<-data.frame(do.call(rbind,lapply(1:length(li),function(i){
    x<-li[[i]]
    #x$pos_x<-newl[[1]][i]
    #x$pos_x2<-newl[[2]][i]
    x$pos_x<-i
    x$pos_x2<-i
    x
  })))
  ggfactors
}

gg_factors<-function(ggfactors, width=0.4,
                     xlab="Factors",
                     ylab='Number of Observations',
                     title="Observation Totals by Factor and Level",
                     base_size=12,
                     border_palette=viridis::turbo,
                     fill_palette=viridis::turbo,
                     pastel=0.4,
                     show_levels=T,
                     show_obs=T,
                     col_lev="lightsteelblue",
                     col_obs="lightcyan"){
  df<-ggfactors
  border_colors <- border_palette(256)
  fill_colors <- fill_palette(256)
  pastel_fill <- make_pastel(fill_colors, pastel)
  df$label_fill <- "lightcyan"
  df$nobs_fill <- "lightsteelblue"
  p<-ggplot(df, aes(x=factor, y=nobs)) +
    geom_bar(aes(fill=position, color=position), position="stack", stat="identity", show.legend=F, width=width) +
    scale_fill_gradientn(colors = pastel_fill, guide = "none") +
    scale_color_gradientn(colors = border_colors, guide = "none") +
    ylab(ylab) +
    xlab(xlab) +
    ggtitle(title) +
    ggnewscale::new_scale_fill()+
    theme_bw(base_size)
  lab_levels<-col_levels<-c()
  if(isTRUE(show_levels)) {
    req(length(col_lev)==1)
    p<-p + geom_label(
      aes(label = labels, y = label_position_top,x=pos_x2, fill = label_fill),
      label.r = unit(0, "lines"),
      label.size = 0,
      label.padding = unit(0.15, "lines"),
      hjust = 0,
      vjust = 0,
      show.legend = T
    )

    col_levels[ length(col_levels)+1]<-col_lev
    lab_levels[ length(lab_levels)+1]<-"Level"

  }
  if(isTRUE(show_obs)){
    req(length(col_obs)==1)
    p<-p +geom_label(
      aes(label = nobs, y = label_position_top,x=pos_x, fill = nobs_fill),
      label.r = unit(0, "lines"),
      label.size = 0,
      label.padding = unit(0.15, "lines"),
      hjust = 0,
      vjust = 1,
      show.legend = T
    )
    col_levels[length(col_levels)+1]<-col_obs
    lab_levels[length(lab_levels)+1]<-"Number of Observations"
  }
  if(isTRUE(show_obs)|isTRUE(show_levels)){
    p<-p+ scale_fill_manual(values = col_levels,
                            labels =lab_levels,
                            name = "")}

  p<-p+guides(fill = guide_legend(override.aes = list(label = "")))+ coord_flip()

  return(p)
}
#' @export
pmds<-function(mds_data,keytext=NULL,key=NULL,points=T, text=F,palette="black", cex.points=1, cex.text=1, pch=16, textcolor="gray",newcolhabs, pos=2, offset=0)
{
  if(!is.null(key)) {
    colkey<-getcolhabs(newcolhabs,palette, nlevels(key))
    col<-colkey[key]} else{col= getcolhabs(newcolhabs,palette, nrow(mds_data$points)) }
  opar<-par(no.readonly=TRUE)
  layout(matrix(c(1,2), nrow=1),widths = c(100,20))
  par(mar=c(5,5,4,1))
  plot(mds_data$points, pch=pch,  las=1, type="n", main="Multidimensional scaling")
  legend("topr",legend=c(paste("Stress:",round(mds_data$stress,2)), paste0("Dissimilarity:", "'",mds_data$distmethod,"'")),cex=.8, bty="n")
  if(isTRUE(points)){ points(mds_data$points, pch=pch, col=col, cex=cex.points)}

  if(isTRUE(text)){
    colkey2<-getcolhabs(newcolhabs,textcolor, nlevels(keytext))
    col2<-colkey2[keytext]
    text(mds_data$points, col=col2, labels=keytext, cex=cex.text, pos=pos, offset=offset)}
  if(!is.null(key)){
    par(mar=c(0,0,0,0))
    plot.new()
    colkey<-getcolhabs(newcolhabs,palette, nlevels(key))
    legend("center",pch=pch,col=colkey, legend=levels(key),  cex=.8,  bg="gray95", box.col="white",xpd=T, adj=0)
  }
  on.exit(par(opar),add=TRUE,after=FALSE)
  return(mds_data)


}
#' @export
ppca<-function(pca,key=NULL,keytext=NULL,points=T, text=NULL,palette="black", cex.points=1, cex.text=1, pch=16,textcolor="gray", biplot=T,newcolhabs, pos=2, offset=0) {

  {

    PCA = pca

    comps<-summary(PCA)

    exp_pc1<-paste("PC I (",round(comps$importance[2,1]*100,2),"%", ")", sep="")
    exp_pc2<-paste("PC II (",round(comps$importance[2,2]*100,2),"%", ")", sep="")

    choices = 1:2
    scale = 1
    scores= PCA$x
    lam = PCA$sdev[choices]
    n = nrow(scores)
    lam = lam * sqrt(n)
    x = t(t(scores[,choices])/ lam)
    y = t(t(PCA$rotation[,choices]) * lam)
    n = nrow(x)
    p = nrow(y)
    xlabs = 1L:n
    xlabs = as.character(xlabs)
    dimnames(x) = list(xlabs, dimnames(x)[[2L]])
    ylabs = dimnames(y)[[1L]]
    ylabs = as.character(ylabs)
    dimnames(y) <- list(ylabs, dimnames(y)[[2L]])
    unsigned.range = function(x) c(-abs(min(x, na.rm = TRUE)),
                                   abs(max(x, na.rm = TRUE)))
    rangx1 = unsigned.range(x[, 1L])
    rangx2 = unsigned.range(x[, 2L])
    rangy1 = unsigned.range(y[, 1L])
    rangy2 = unsigned.range(y[, 2L])
    xlim = ylim = rangx1 = rangx2 = range(rangx1, rangx2)
    ratio = max(rangy1/rangx1, rangy2/rangx2)



  }



  if(!is.null(key)) {
    colkey<-getcolhabs(newcolhabs,palette, nlevels(key))
    col<-colkey[key]} else{col= getcolhabs(newcolhabs,palette, nrow(x)) }
  opar<-par(no.readonly=TRUE)
  layout(matrix(c(1,2), nrow=1),widths = c(100,20))
  par(pty = "s",mar=c(5,5,5,1))
  plot(x, type = "n", xlim = xlim, ylim = ylim, las=1, xlab=exp_pc1, ylab=exp_pc2, main="Principal Component Analysis",col.sub="black", tck=0)
  abline(v=0, lty=2, col="gray")
  abline(h=0, lty=2, col="gray")

  if(isTRUE(points)){
    points(x, pch=pch, col=col, cex=cex.points)
  }
  if(isTRUE(text)){
    colkey2<-getcolhabs(newcolhabs,textcolor, nlevels(keytext))
    col2<-colkey2[keytext]
    text(x, col=col2, labels=keytext, cex=cex.text, pos=pos, offset=offset)
  }
  if(isTRUE(biplot)){

    par(new = TRUE)
    xlim = xlim * ratio*2
    ylim = ylim * ratio
    plot(y, axes = FALSE, type = "n",
         xlim = xlim,
         ylim = ylim, xlab = "", ylab = "")
    axis(3,padj=1, tck=-0.01); axis(4, las=1)
    boxtext(x =y[,1], y = y[,2], labels = rownames(y), col.bg = adjustcolor("white", 0.2),  cex=1, border.bg  ="gray80", pos=3)

    PCA$rotation[,1]*10
    #text(y, labels = ylabs, font=2, cex=.8, col=)
    arrow.len = 0.1
    arrows(0, 0, y[, 1L] * 0.8, y[, 2L] * 0.8,
           length = arrow.len, col = 2, lwd=1.5)

  }

  if(!is.null(key)) {
    par(mar=c(0,0,0,0))
    plot.new()
    colkey<-getcolhabs(newcolhabs,palette, nlevels(key))
    legend("center",pch=pch,col=colkey, legend=levels(key),  cex=.8,  bg="gray95", box.col="white",xpd=T, adj=0)
  }

  on.exit(par(opar),add=TRUE,after=FALSE)
  return(PCA)
}

#' @export
psummary<-function(data){
  nas=sum(is.na(unlist(data)))

  n=data.frame(rbind(Param=paste('Missing values:', nas)))
  a<-data.frame(rbind(Param=paste('nrow:', nrow(data)),paste('ncol:', ncol(data))))

  c<-data.frame(Param=
                  c("max:", "min:", "mean:","median:","var:", "sd:"),
                Value=c(max(data,na.rm = T), min(data,na.rm = T), mean(unlist(data),na.rm = T), median(unlist(data),na.rm = T), var(unlist(data),na.rm = T), sd(unlist(data),na.rm = T)))
  c$Value<-unlist(lapply(c[,2],round,3))
  ppsummary("-------------------")
  ppsummary(n)
  ppsummary("-------------------")
  ppsummary(a)
  ppsummary("-------------------")
  ppsummary(c)
  ppsummary("-------------------")
}

# ---- Compatibility of two Datalists used together (RDA, segRDA) ---------------------
# block: problems that stop the analysis; warn: information (e.g. observations left out).
#' @export
desc_xy_issues<-function(resp,expl,resp_name="Y",expl_name="X"){
  block<-character(0)
  warn<-character(0)
  if(is.null(resp)||is.null(expl)) return(list(block=block,warn=warn))
  if(identical(resp_name,expl_name)){
    block<-c(block,paste0("The same Datalist ('",resp_name,"') was chosen for both sides. Choose different Datalists for the response and the explanatory variables."))
    return(list(block=block,warn=warn))
  }
  common<-intersect(rownames(resp),rownames(expl))
  if(!length(common)){
    block<-c(block,paste0("'",resp_name,"' and '",expl_name,"' have no observation IDs in common. Choose Datalists that share the same observations."))
    return(list(block=block,warn=warn))
  }
  n_out<-length(union(setdiff(rownames(resp),common),setdiff(rownames(expl),common)))
  if(n_out>0){
    warn<-c(warn,paste0(n_out," observation(s) are present in only one of the Datalists and will be left out; the analysis uses the ",length(common)," observations in common."))
  }
  r<-resp[common,vapply(resp,is.numeric,logical(1)),drop=FALSE]
  e<-expl[common,vapply(expl,is.numeric,logical(1)),drop=FALSE]
  dup<-unlist(lapply(colnames(r),function(v){
    hits<-colnames(e)[vapply(e,function(col) isTRUE(all(col==r[[v]]|(is.na(col)&is.na(r[[v]])))),logical(1))]
    if(length(hits)) paste0("'",v,"'",if(!identical(hits,v)) paste0(" (= '",paste(hits,collapse="', '"),"')") else "")
  }))
  if(length(dup)){
    block<-c(block,paste0("Variables with identical values on both sides: ",paste(dup,collapse=", "),". A variable cannot explain itself; remove it from one of the Datalists."))
  }
  list(block=block,warn=warn)
}

#' @export
desc_xy_issues_ui<-function(res){
  if(is.null(res)||(!length(res$block)&&!length(res$warn))) return(NULL)
  div(
    if(length(res$block)) div(class="alert_warning",style="padding: 6px 10px; margin: 4px 0px; font-size: 12px;",
                              strong(icon("triangle-exclamation")," Check the setup:"),
                              tags$ul(style="margin: 2px 0px 0px 0px; padding-left: 18px;",lapply(res$block,tags$li))),
    if(length(res$warn)) div(style="padding: 4px 10px; font-size: 11px; color: #8a6d3b;",
                             icon("circle-info")," ",paste(res$warn,collapse=" "))
  )
}

# ---- Temporal structure helpers (Scatter plot and Temporal Descriptives) ---------------
# phase of a seasonal cycle and the cycle each observation belongs to
#' @export
desc_season_phase<-function(t,cycle){
  switch(cycle,
         hour=list(phase=format(t,"%H"),cycle_id=format(t,"%Y-%m-%d")),
         doy=list(phase=format(t,"%j"),cycle_id=format(t,"%Y")),
         month=list(phase=format(t,"%m"),cycle_id=format(t,"%Y")),
         season={
           m<-as.integer(format(t,"%m"))
           s<-c("DJF","DJF","MAM","MAM","MAM","JJA","JJA","JJA","SON","SON","SON","DJF")[m]
           # December belongs to the DJF of the following year
           list(phase=s,cycle_id=as.character(as.integer(format(t,"%Y"))+(m==12)))
         },
         stop("unknown cycle"))
}

# seasonal cycles that can be removed: the data must cover at least two cycles, with
# most phases (e.g. months) observed in two or more cycles (e.g. years)
#' @export
desc_season_cycles<-function(t){
  t<-t[!is.na(t)]
  opts<-c("Hour of day"="hour","Day of year"="doy","Month"="month","Season (DJF/MAM/JJA/SON)"="season")
  if(!length(t)||!inherits(t,c("Date","POSIXt"))) return(opts[0])
  if(!inherits(t,"POSIXt")) opts<-opts[opts!="hour"]
  ok<-vapply(opts,function(cy){
    ph<-desc_season_phase(t,cy)
    n_phase<-length(unique(ph$phase))
    if(n_phase<2||length(unique(ph$cycle_id))<2) return(FALSE)
    if(identical(cy,"doy")&&n_phase<=52) return(FALSE)
    ncyc<-tapply(ph$cycle_id,ph$phase,function(v) length(unique(v)))
    mean(ncyc>=2)>=0.8
  },logical(1))
  opts[ok]
}

# what the time axis looks like: steps, repetitions, spacing and removable cycles
#' @export
desc_time_structure<-function(t){
  t<-t[!is.na(t)]
  if(!length(t)||!inherits(t,c("Date","POSIXt"))) return(NULL)
  ut<-sort(unique(t))
  days<-if(inherits(t,"POSIXt")) as.numeric(difftime(ut,ut[1],units="days")) else as.numeric(ut-ut[1])
  dif<-diff(days)
  step<-if(length(dif)) stats::median(dif) else NA_real_
  regular<-length(dif)>1&&isTRUE(step>0)&&isTRUE(stats::sd(dif)/mean(dif)<0.2)
  reps<-as.vector(table(as.numeric(t)))
  list(n=length(t),n_steps=length(ut),reps_median=stats::median(reps),reps_max=max(reps),
       step_days=step,regular=regular,span_days=max(days),from=min(t),to=max(t),cycles=desc_season_cycles(t))
}

#' @export
desc_time_structure_text<-function(st){
  if(is.null(st)) return(NULL)
  step<-st$step_days
  step_txt<-if(is.na(step)) "-" else if(step<1) paste0(signif(step*24,3)," h") else if(step<60) paste0(signif(step,3)," days") else paste0(signif(step/30.44,3)," months")
  c(paste0(st$n_steps," time steps from ",format(st$from)," to ",format(st$to),"; ",if(st$regular) "regular" else "irregular"," spacing (median step ",step_txt,")."),
    if(st$reps_max>1) paste0("Repeated time steps: up to ",st$reps_max," observations per step (median ",st$reps_median,")."),
    if(length(st$cycles)) paste0("Removable seasonal cycles: ",paste(names(st$cycles),collapse=", "),".") else "No seasonal cycle is covered at least twice.")
}

# anomalies: value minus the mean of its phase (e.g. of its month)
#' @export
desc_deseason<-function(v,phase){
  v-stats::ave(v,phase,FUN=function(z) mean(z,na.rm=TRUE))
}

# lag-1 autocorrelation of residuals in time order (mean per time step when repeated)
# and the effective number of independent time steps
#' @export
desc_resid_acf<-function(res,time){
  ok<-!is.na(res)&!is.na(time)
  if(sum(ok)<3) return(list(r1=NA_real_,n_steps=sum(ok),n_eff=NA_real_))
  m<-tapply(res[ok],as.numeric(time[ok]),mean)
  n<-length(m)
  if(n<10) return(list(r1=NA_real_,n_steps=n,n_eff=NA_real_))
  r1<-stats::cor(m[-1],m[-n])
  n_eff<-if(is.na(r1)||r1<=0) n else max(3,n*(1-r1)/(1+r1))
  list(r1=r1,n_steps=n,n_eff=n_eff)
}

# ---- Trend and decomposition helpers (Temporal Descriptives) ---------------------------
# Mann-Kendall test (variance corrected for ties) and Sen's slope; t in time units
#' @export
desc_mann_kendall<-function(y,t){
  ok<-!is.na(y)&!is.na(t)
  y<-y[ok]
  t<-t[ok]
  o<-order(t)
  y<-y[o]
  t<-t[o]
  n<-length(y)
  na<-list(n=n,S=NA_real_,tau=NA_real_,p=NA_real_,sen=NA_real_,sen_intercept=NA_real_)
  if(n<4||n>3000) return(na)
  ij<-which(upper.tri(matrix(FALSE,n,n)),arr.ind=TRUE)
  i<-ij[,1]
  j<-ij[,2]
  S<-sum(sign(y[j]-y[i]))
  tp<-as.vector(table(y))
  varS<-(n*(n-1)*(2*n+5)-sum(tp*(tp-1)*(2*tp+5)))/18
  Z<-if(varS>0) (S-sign(S))/sqrt(varS) else 0
  dt<-t[j]-t[i]
  slopes<-(y[j]-y[i])[dt!=0]/dt[dt!=0]
  sen<-if(length(slopes)) stats::median(slopes) else NA_real_
  list(n=n,S=S,tau=S/(n*(n-1)/2),p=2*stats::pnorm(-abs(Z)),sen=sen,sen_intercept=stats::median(y-sen*t))
}

# OLS trend with the p-value corrected by the effective n (lag-1 autocorrelation of the
# residuals), plus Mann-Kendall / Sen. t: numeric time (days for dates); per: multiplier
# that converts the slope to the reported unit (e.g. 365.25 for per year)
#' @export
desc_trend_stats<-function(y,t,per=1){
  ok<-!is.na(y)&!is.na(t)
  y<-y[ok]
  t<-t[ok]
  n<-length(y)
  out<-data.frame(N=n,Slope=NA_real_,R2=NA_real_,P_value=NA_real_,Resid_lag1_r=NA_real_,n_eff=NA_real_,P_value_adj=NA_real_,
                  Sen_slope=NA_real_,MK_tau=NA_real_,MK_p=NA_real_)
  if(n<3||length(unique(t))<2) return(out)
  fit<-stats::lm(y~t)
  sm<-summary(fit)
  b<-unname(stats::coef(fit)[2])
  out$Slope<-b*per
  out$R2<-sm$r.squared
  out$P_value<-sm$coefficients[2,4]
  ac<-desc_resid_acf(stats::residuals(fit),t)
  out$Resid_lag1_r<-ac$r1
  out$n_eff<-ac$n_eff
  if(!is.na(ac$n_eff)&&ac$n_eff>2){
    se<-sm$coefficients[2,2]*sqrt((n-2)/(ac$n_eff-2))
    out$P_value_adj<-2*stats::pt(-abs(b/se),ac$n_eff-2)
  }
  mk<-desc_mann_kendall(y,t)
  out$Sen_slope<-mk$sen*per
  out$MK_tau<-mk$tau
  out$MK_p<-mk$p
  attr(out,"lines")<-list(intercept=unname(stats::coef(fit)[1]),slope=b,sen=mk$sen,sen_intercept=mk$sen_intercept)
  out
}

# STL decomposition of a regular series (one value per time step) for a seasonal cycle
#' @export
desc_stl<-function(time,y,cycle){
  o<-order(time)
  time<-time[o]
  y<-y[o]
  st<-desc_time_structure(time)
  if(is.null(st)) return("needs a date or date-time variable.")
  if(!isTRUE(st$regular)) return("needs regularly spaced time steps.")
  if(st$reps_max>1) return("needs one value per time step.")
  ut<-as.numeric(time)
  if(inherits(time,"POSIXt")) ut<-ut/86400
  if(max(diff(ut))>1.5*st$step_days) return("the series has gaps (missing time steps).")
  if(any(is.na(y))) return("the series has missing values.")
  ph<-desc_season_phase(time,cycle)
  cnt<-as.vector(table(ph$cycle_id))
  f<-if(length(cnt)>2) stats::median(cnt[-c(1,length(cnt))]) else max(cnt)
  f<-round(f)
  if(f<2) return("the cycle has less than two time steps.")
  if(length(y)<2*f+1) return("the series must cover at least two complete cycles.")
  dec<-tryCatch(stats::stl(stats::ts(y,frequency=f),s.window="periodic",robust=TRUE),error=function(e) conditionMessage(e))
  if(is.character(dec)) return(dec)
  comp<-dec$time.series
  tr<-as.numeric(comp[,"trend"])
  se<-as.numeric(comp[,"seasonal"])
  re<-as.numeric(comp[,"remainder"])
  strength<-function(a) max(0,1-stats::var(re)/stats::var(a+re))
  list(df=data.frame(time=rep(time,4),component=factor(rep(c("Observed","Trend","Seasonal","Remainder"),each=length(y)),levels=c("Observed","Trend","Seasonal","Remainder")),
                     value=c(y,tr,se,re)),
       stats=data.frame(N=length(y),Steps_per_cycle=f,Seasonal_amplitude=diff(range(se)),Trend_strength=strength(tr),Seasonal_strength=strength(se)))
}

# ---- Scatter plot (Descriptive tools, tab 11) ------------------------------------------

# attributes of a Datalist that can be used on an axis (time only on X)
#' @export
desc_scatter_attrs<-function(d,axis="x"){
  out<-character(0)
  if(is.null(d)) return(out)
  if(any(vapply(d,is.numeric,logical(1)))) out<-c(out,"Numeric-Attribute"="numeric")
  tt<-attr(d,"time")
  if(identical(axis,"x")&&!is.null(tt)&&ncol(tt)>0) out<-c(out,"Temporal-Attribute"="time")
  co<-attr(d,"coords")
  if(!is.null(co)&&ncol(co)>0&&any(vapply(as.data.frame(co),is.numeric,logical(1)))) out<-c(out,"Coords-Attribute"="coords")
  out
}

#' @export
desc_scatter_table<-function(d,attr_name){
  tab<-switch(attr_name,time=attr(d,"time"),coords=attr(d,"coords"),d)
  if(is.null(tab)) return(NULL)
  tab<-as.data.frame(tab)
  if(!identical(attr_name,"time")) tab<-tab[,vapply(tab,is.numeric,logical(1)),drop=FALSE]
  tab
}

# named vector (names = observation IDs); temporal columns are read as dates
#' @export
desc_scatter_vector<-function(tab,var,attr_name){
  v<-tab[[var]]
  if(identical(attr_name,"time")&&!inherits(v,c("Date","POSIXt"))&&!is.numeric(v)){
    g<-guess_time_settings(v)
    validate(need(g$type%in%c("date","datetime"),"The temporal column could not be read as dates. Format it in the Databank (Date)."))
    v<-convert_time_column(v,g$type,g$format,g$custom)
  }
  stats::setNames(v,rownames(tab))
}

# number of observation IDs each Datalist shares with ids
#' @export
desc_scatter_partners<-function(saved_data,ids){
  vapply(saved_data,function(d) sum(rownames(d)%in%ids),integer(1))
}

# x, y: named vectors (names = observation IDs), possibly from different Datalists;
# color: optional one-column data.frame (factor or numeric) with IDs as rownames;
# extra: optional data.frame of additional predictors (multiple regression), IDs as rownames;
# time: optional named vector of dates (IDs as names), used to remove a seasonal cycle
# (anomalies from the mean of each phase) and to check the autocorrelation of residuals.
#' @export
desc_scatter_data<-function(x,y,color=NULL,extra=NULL,time=NULL,cycle="none"){
  force(x)
  force(y)
  ids<-base::intersect(names(x),names(y))
  validate(need(length(ids)>0,"X and Y have no observation IDs in common. Choose Datalists that share the same observations."))
  if(!is.null(color)) ids<-base::intersect(ids,rownames(color))
  validate(need(length(ids)>0,"The coloring Datalist has none of the observations of X and Y."))
  df<-data.frame(id=ids,x=x[ids],y=y[ids],stringsAsFactors=FALSE)
  if(!is.null(color)) df$color<-color[ids,1]
  extra_names<-character(0)
  if(!is.null(extra)&&ncol(extra)>0){
    ids_e<-base::intersect(df$id,rownames(extra))
    df<-df[df$id%in%ids_e,,drop=FALSE]
    extra_names<-paste0("e",seq_len(ncol(extra)))
    for(i in seq_along(extra_names)) df[[extra_names[i]]]<-extra[df$id,i]
  }
  keep<-!is.na(df$x)&!is.na(df$y)
  for(e in extra_names) keep<-keep&!is.na(df[[e]])
  df<-df[keep,,drop=FALSE]
  validate(need(nrow(df)>1,"At least two observations with X and Y values are needed."))
  rownames(df)<-df$id
  x_time<-inherits(df$x,c("Date","POSIXt"))
  if(x_time) time<-df$x
  else if(!is.null(time)) time<-time[df$id]
  if(!is.null(time)) df$time<-time
  if(!is.null(cycle)&&!identical(cycle,"none")){
    validate(need(!is.null(df$time),"Removing a seasonal cycle needs a Temporal-Attribute."))
    df<-df[!is.na(df$time),,drop=FALSE]
    ph<-desc_season_phase(df$time,cycle)$phase
    # anomalies of every numeric variable (not of a temporal X)
    if(!x_time) df$x<-desc_deseason(df$x,ph)
    df$y<-desc_deseason(df$y,ph)
    for(e in extra_names) df[[e]]<-desc_deseason(df[[e]],ph)
  }
  # numeric version of X used by the models: days since the first date for temporal X
  if(inherits(df$x,"Date")){
    origin<-min(df$x)
    df$xn<-as.numeric(df$x-origin)
  } else if(inherits(df$x,"POSIXt")){
    origin<-min(df$x)
    df$xn<-as.numeric(difftime(df$x,origin,units="days"))
  } else{
    origin<-NULL
    df$xn<-as.numeric(df$x)
  }
  attr(df,"time_origin")<-origin
  attr(df,"n_left_out")<-length(union(names(x),names(y)))-nrow(df)
  attr(df,"color_name")<-if(!is.null(color)) colnames(color) else NULL
  attr(df,"extra_names")<-extra_names
  attr(df,"extra_labels")<-if(length(extra_names)) colnames(extra) else character(0)
  attr(df,"cycle")<-if(is.null(cycle)) "none" else cycle
  df
}

# groups used by models, aggregation and statistics: the color factor, if any
desc_scatter_group<-function(df){
  if(!is.null(df$color)&&is.factor(df$color)) droplevels(df$color) else factor(rep("All",nrow(df)))
}

# X classes used to summarise Y: calendar periods (temporal X) or equal-width bins
#' @export
desc_scatter_bin<-function(df,by="none",bins=10){
  if(identical(by,"none")||is.null(by)) return(df)
  if(inherits(df$x,"Date")){
    df$xb<-as.Date(cut(df$x,by))
  } else if(inherits(df$x,"POSIXt")){
    df$xb<-as.POSIXct(cut(df$x,by),tz=attr(df$x,"tzone")%||%"")
  } else{
    bins<-max(2,round(bins%||%10))
    br<-seq(min(df$x),max(df$x),length.out=bins+1)
    if(length(unique(br))<2) return(df)
    mid<-(br[-1]+br[-length(br)])/2
    df$xb<-mid[as.integer(cut(df$x,br,include.lowest=TRUE))]
  }
  df
}

# one row per X value/class and color group: mean of Y (and of the additional predictors);
# used to draw the means and, when the model is fitted on the means, as the model data
#' @export
desc_scatter_aggregate<-function(df,err="se"){
  df$group<-desc_scatter_group(df)
  df$xa<-if(!is.null(df$xb)) df$xb else df$x
  extra<-attr(df,"extra_names")
  if(is.null(extra)) extra<-character(0)
  origin<-attr(df,"time_origin")
  sm<-do.call(rbind,lapply(split(df,list(df$group,as.character(df$xa)),drop=TRUE),function(s){
    n<-nrow(s)
    sdv<-if(n>1) stats::sd(s$y) else NA_real_
    r<-data.frame(x=s$xa[1],group=as.character(s$group[1]),y=mean(s$y),n=n,sd=sdv,se=sdv/sqrt(n),stringsAsFactors=FALSE)
    for(e in extra) r[[e]]<-mean(s[[e]])
    r
  }))
  sm$group<-factor(sm$group,levels=levels(df$group))
  sm<-sm[order(sm$group,sm$x),,drop=FALSE]
  sm$err<-if(identical(err,"sd")) sm$sd else sm$se
  sm$mean<-sm$y
  xlab<-if(is.numeric(sm$x)) as.character(signif(sm$x,4)) else format(sm$x)
  sm$id<-if(nlevels(df$group)>1) paste0(sm$group," | ",xlab) else xlab
  if(!is.null(df$color)&&is.factor(df$color)) sm$color<-sm$group
  sm$xn<-if(inherits(sm$x,"Date")) as.numeric(sm$x-origin) else if(inherits(sm$x,"POSIXt")) as.numeric(difftime(sm$x,origin,units="days")) else as.numeric(sm$x)
  sm$w<-sm$n
  if(inherits(sm$x,c("Date","POSIXt"))) sm$time<-sm$x
  rownames(sm)<-NULL
  for(a in c("time_origin","color_name","extra_names","extra_labels","cycle")) attr(sm,a)<-attr(df,a)
  attr(sm,"n_raw")<-nrow(df)
  sm
}

#' @export
desc_scatter_model_choices<-c(
  "None"="none",
  "Linear"="lm",
  "Polynomial (2nd degree)"="poly2",
  "Polynomial (3rd degree)"="poly3",
  "Logarithmic"="log",
  "Exponential"="exp",
  "Power"="power",
  "Logistic"="logistic",
  "Gompertz"="gompertz",
  "Michaelis-Menten"="mm",
  "Asymptotic (exp. rise)"="asymp",
  "Smooth (loess)"="loess"
)
desc_scatter_model_help<-paste(
  "Linear: y = a + b x (+ c X2 + ... with additional predictors, multiple regression)",
  "Polynomial: y = a + b x + c x^2 (+ d x^3)",
  "Logarithmic: y = a + b ln(x), x > 0",
  "Exponential: y = a exp(b x)",
  "Power: y = a x^b, x > 0",
  "Logistic: y = Asym / (1 + exp((xmid - x)/scal))",
  "Gompertz: y = Asym exp(-b2 b3^x)",
  "Michaelis-Menten: y = Vm x / (K + x)",
  "Asymptotic: y = Asym + (R0 - Asym) exp(-exp(lrc) x)",
  "Loess: local smoother, no equation",
  "Linear and polynomial models are fitted by least squares (lm); the others by nonlinear least squares (nls). With temporal X, x is the number of days since the first date.",
  sep="<br>"
)
# models that accept additional predictors (multiple regression)
desc_scatter_multi<-c("lm","poly2","poly3")

desc_scatter_fit_one<-function(s,type,extra=character(0)){
  # w: weights (number of observations behind each mean); 1 for raw observations
  if(is.null(s$w)) s$w<-rep(1,nrow(s))
  ex<-if(length(extra)) paste0("+",paste(extra,collapse="+")) else ""
  f<-switch(type,
            lm=paste0("y~xn",ex),
            poly2=paste0("y~xn+I(xn^2)",ex),
            poly3=paste0("y~xn+I(xn^2)+I(xn^3)",ex),
            log="y~log(xn)",
            NULL)
  if(type%in%c("log","power")&&any(s$xn<=0)) stop("this model requires X > 0")
  if(!is.null(f)) return(stats::lm(stats::as.formula(f),data=s,weights=w))
  if(identical(type,"loess")){
    if(nrow(s)<6) stop("loess needs at least 6 observations")
    return(stats::loess(y~xn,data=s,weights=w))
  }
  ctrl<-stats::nls.control(maxiter=500,warnOnly=FALSE)
  switch(type,
         exp={
           st<-if(all(s$y>0)){
             cf<-stats::coef(stats::lm(log(y)~xn,data=s))
             list(a=exp(cf[[1]]),b=cf[[2]])
           } else list(a=mean(s$y),b=0)
           stats::nls(y~a*exp(b*xn),data=s,start=st,weights=w,control=ctrl)
         },
         power={
           st<-if(all(s$y>0)){
             cf<-stats::coef(stats::lm(log(y)~log(xn),data=s))
             list(a=exp(cf[[1]]),b=cf[[2]])
           } else list(a=mean(s$y),b=1)
           stats::nls(y~a*xn^b,data=s,start=st,weights=w,control=ctrl)
         },
         logistic=stats::nls(y~stats::SSlogis(xn,Asym,xmid,scal),data=s,weights=w,control=ctrl),
         gompertz=stats::nls(y~stats::SSgompertz(xn,Asym,b2,b3),data=s,weights=w,control=ctrl),
         mm=stats::nls(y~stats::SSmicmen(xn,Vm,K),data=s,weights=w,control=ctrl),
         asymp=stats::nls(y~stats::SSasymp(xn,Asym,R0,lrc),data=s,weights=w,control=ctrl),
         stop("unknown model"))
}

desc_scatter_equation<-function(fit,type,xlab="x",extra_labels=character(0)){
  if(identical(type,"loess")) return("loess smoother (no equation)")
  cf<-stats::coef(fit)
  f<-function(v) trimws(formatC(unname(v),digits=4,format="g"))
  term<-function(v,txt) paste0(if(v<0) " - " else " + ",f(abs(v)),txt)
  switch(type,
         lm=,poly2=,poly3={
           txt<-paste0("y = ",f(cf[1]))
           nm<-names(cf)[-1]
           lab<-nm
           lab[nm=="xn"]<-xlab
           lab[nm=="I(xn^2)"]<-paste0(xlab,"^2")
           lab[nm=="I(xn^3)"]<-paste0(xlab,"^3")
           for(i in seq_along(extra_labels)) lab[nm==paste0("e",i)]<-extra_labels[i]
           for(i in seq_along(nm)) txt<-paste0(txt,term(cf[i+1],paste0(" ",lab[i])))
           txt
         },
         log=paste0("y = ",f(cf[1]),term(cf[2],paste0(" ln(",xlab,")"))),
         exp=paste0("y = ",f(cf["a"])," exp(",f(cf["b"])," ",xlab,")"),
         power=paste0("y = ",f(cf["a"])," ",xlab,"^",f(cf["b"])),
         logistic=paste0("y = ",f(cf["Asym"])," / (1 + exp((",f(cf["xmid"])," - ",xlab,") / ",f(cf["scal"]),"))"),
         gompertz=paste0("y = ",f(cf["Asym"])," exp(-",f(cf["b2"])," ",f(cf["b3"]),"^",xlab,")"),
         mm=paste0("y = ",f(cf["Vm"])," ",xlab," / (",f(cf["K"])," + ",xlab,")"),
         asymp=paste0("y = ",f(cf["Asym"])," + (",f(cf["R0"])," - ",f(cf["Asym"]),") exp(-exp(",f(cf["lrc"]),") ",xlab,")"),
         "")
}

# fits the model in each color group; returns fits, curves, metrics and coefficients
#' @export
desc_scatter_fit<-function(df,type="none",band="confidence",level=0.95,xlab="x"){
  if(is.null(type)||identical(type,"none")) return(NULL)
  df$group<-desc_scatter_group(df)
  extra<-if(type%in%desc_scatter_multi) attr(df,"extra_names") else character(0)
  extra_labels<-if(length(extra)) attr(df,"extra_labels") else character(0)
  origin<-attr(df,"time_origin")
  eq_x<-if(is.null(origin)) "x" else "t"
  to_x<-function(xn){
    if(is.null(origin)) return(xn)
    if(inherits(origin,"Date")) origin+xn else origin+xn*86400
  }
  out<-lapply(split(df,df$group,drop=TRUE),function(s){
    s<-s[order(s$xn),,drop=FALSE]
    g<-as.character(s$group[1])
    fit<-tryCatch(suppressWarnings(desc_scatter_fit_one(s,type,extra)),error=function(e){
      msg<-conditionMessage(e)
      if(type%in%c("exp","power","logistic","gompertz","mm","asymp")&&!grepl("X > 0",msg,fixed=TRUE)) paste0("did not converge: ",msg) else msg
    })
    if(is.character(fit)) return(list(group=g,error=fit))
    fitted_v<-as.numeric(stats::fitted(fit))
    res<-s$y-fitted_v
    n<-nrow(s)
    p<-if(identical(type,"loess")) NA else length(stats::coef(fit))
    w<-if(is.null(s$w)) rep(1,n) else s$w
    r2<-1-sum(w*res^2)/sum(w*(s$y-stats::weighted.mean(s$y,w))^2)
    adj<-if(is.na(p)||n-p<=0) NA else 1-(1-r2)*(n-1)/(n-p)
    p_model<-NA
    if(inherits(fit,"lm")){
      fs<-summary(fit)$fstatistic
      if(!is.null(fs)) p_model<-stats::pf(fs[1],fs[2],fs[3],lower.tail=FALSE)
    }
    metrics<-data.frame(Group=g,n=n,Parameters=p,R2=r2,Adj_R2=adj,RMSE=sqrt(mean(res^2)),MAE=mean(abs(res)),
                        AIC=if(identical(type,"loess")) NA else stats::AIC(fit),
                        BIC=if(identical(type,"loess")) NA else stats::BIC(fit),
                        P_model=p_model,
                        stringsAsFactors=FALSE)
    if(!is.null(s$time)){
      ac<-desc_resid_acf(res,s$time)
      metrics$Time_steps<-ac$n_steps
      metrics$Resid_lag1_r<-ac$r1
      metrics$n_eff<-ac$n_eff
      # overall test with the effective number of independent time steps
      p_adj<-NA
      if(!is.na(p)&&p>1&&!is.na(ac$n_eff)&&ac$n_eff>p&&r2<1){
        Fs<-(r2/(p-1))/((1-r2)/(ac$n_eff-p))
        p_adj<-stats::pf(Fs,p-1,ac$n_eff-p,lower.tail=FALSE)
      }
      metrics$P_model_adj<-p_adj
    }
    metrics$Equation<-desc_scatter_equation(fit,type,eq_x,extra_labels)
    coefs<-NULL
    if(!identical(type,"loess")){
      cm<-suppressWarnings(summary(fit))$coefficients
      term<-rownames(cm)
      term[term=="xn"]<-eq_x
      term[term=="I(xn^2)"]<-paste0(eq_x,"^2")
      term[term=="I(xn^3)"]<-paste0(eq_x,"^3")
      for(i in seq_along(extra_labels)) term[term==paste0("e",i)]<-extra_labels[i]
      coefs<-data.frame(Group=g,Term=term,Estimate=cm[,1],Std_Error=cm[,2],Statistic=cm[,3],P_value=cm[,4],stringsAsFactors=FALSE)
    }
    # curve over the observed X range (other predictors at their mean)
    grid<-data.frame(xn=seq(min(s$xn),max(s$xn),length.out=200))
    for(e in extra) grid[[e]]<-mean(s[[e]])
    curve<-data.frame(xn=grid$xn,fit=NA_real_,lwr=NA_real_,upr=NA_real_)
    if(inherits(fit,"lm")){
      int<-if(identical(band,"prediction")) "prediction" else "confidence"
      pr<-suppressWarnings(stats::predict(fit,newdata=grid,interval=int,level=level))
      curve$fit<-pr[,"fit"]
      if(!identical(band,"none")){
        curve$lwr<-pr[,"lwr"]
        curve$upr<-pr[,"upr"]
      }
    } else if(inherits(fit,"loess")){
      pr<-stats::predict(fit,newdata=grid,se=TRUE)
      curve$fit<-as.numeric(pr$fit)
      if(!identical(band,"none")){
        q<-stats::qt((1+level)/2,pr$df)
        curve$lwr<-as.numeric(pr$fit-q*pr$se.fit)
        curve$upr<-as.numeric(pr$fit+q*pr$se.fit)
      }
    } else{
      curve$fit<-as.numeric(stats::predict(fit,newdata=grid))
    }
    curve$x<-to_x(curve$xn)
    curve$group<-g
    list(group=g,fit=fit,metrics=metrics,coefs=coefs,curve=curve,
         resid=data.frame(id=s$id,group=g,x=s$x,y=s$y,fitted=fitted_v,residual=res,stringsAsFactors=FALSE))
  })
  ok<-Filter(function(r) is.null(r$error),out)
  bad<-Filter(function(r) !is.null(r$error),out)
  notes<-character(0)
  if(length(bad)) notes<-c(notes,paste0("Model not fitted for ",paste0("'",vapply(bad,`[[`,"",'group'),"' (",vapply(bad,`[[`,"",'error'),")",collapse="; "),"."))
  if(length(extra)) notes<-c(notes,"Multiple regression: the curve on the scatter is drawn with the additional predictors at their mean; use Observed vs fitted to see the full model.")
  if(!identical(band,"none")&&!type%in%c("lm","poly2","poly3","log","loess")) notes<-c(notes,"Bands are only available for linear, polynomial, logarithmic and loess models.")
  if(identical(band,"prediction")&&identical(type,"loess")) notes<-c(notes,"Loess shows a confidence band.")
  if(!is.null(origin)) notes<-c(notes,paste0("t = days since ",format(origin),"."))
  r1<-unlist(lapply(ok,function(r) r$metrics$Resid_lag1_r))
  if(length(r1)&&any(r1>0.3,na.rm=TRUE)) notes<-c(notes,paste0("Residuals are autocorrelated in time (lag-1 r up to ",round(max(r1,na.rm=TRUE),2),"): P_model assumes independent observations and is optimistic; P_model_adj uses the effective number of independent time steps (n_eff)."))
  rb<-function(k) {v<-lapply(ok,`[[`,k); v<-Filter(Negate(is.null),v); if(length(v)) do.call(rbind,v) else NULL}
  res<-list(type=type,metrics=rb("metrics"),coefs=rb("coefs"),curve=rb("curve"),resid=rb("resid"),notes=notes,n_ok=length(ok))
  if(!is.null(res$coefs)) rownames(res$coefs)<-NULL
  if(!is.null(res$metrics)) rownames(res$metrics)<-NULL
  res
}

# correlation (and agreement, for the 1:1 comparison) of X and Y in each group
#' @export
desc_scatter_stats<-function(df,agreement=FALSE){
  df$group<-desc_scatter_group(df)
  is_num<-!inherits(df$x,c("Date","POSIXt"))
  out<-do.call(rbind,lapply(split(df,df$group,drop=TRUE),function(s){
    xn<-as.numeric(s$x)
    ok<-nrow(s)>=3&&stats::sd(xn)>0&&stats::sd(s$y)>0
    ct<-function(m) if(ok) suppressWarnings(stats::cor.test(xn,s$y,method=m,exact=FALSE)) else NULL
    pe<-ct("pearson")
    sp<-ct("spearman")
    ke<-ct("kendall")
    r<-data.frame(Group=as.character(s$group[1]),n=nrow(s),
                  Pearson_r=if(ok) unname(pe$estimate) else NA,Pearson_p=if(ok) pe$p.value else NA,
                  Spearman_rho=if(ok) unname(sp$estimate) else NA,Spearman_p=if(ok) sp$p.value else NA,
                  Kendall_tau=if(ok) unname(ke$estimate) else NA,Kendall_p=if(ok) ke$p.value else NA,
                  stringsAsFactors=FALSE)
    if(!is.null(s$time)&&ok){
      ac<-desc_resid_acf(stats::residuals(stats::lm(s$y~xn)),s$time)
      r$Resid_lag1_r<-ac$r1
      r$n_eff<-ac$n_eff
      r$Pearson_p_adj<-NA
      if(!is.na(ac$n_eff)&&ac$n_eff>2&&abs(r$Pearson_r)<1){
        tt<-r$Pearson_r*sqrt((ac$n_eff-2)/(1-r$Pearson_r^2))
        r$Pearson_p_adj<-2*stats::pt(-abs(tt),ac$n_eff-2)
      }
    }
    if(isTRUE(agreement)&&is_num){
      d<-s$y-s$x
      r$Bias_YminusX<-mean(d)
      r$MAE<-mean(abs(d))
      r$RMSE<-sqrt(mean(d^2))
    }
    r
  }))
  rownames(out)<-NULL
  out
}

#' @export
gg_desc_scatter<-function(df,fitres=NULL,agg="none",err="se",show_raw=TRUE,
                          one_to_one=FALSE,log_x=FALSE,log_y=FALSE,facet=FALSE,rug=FALSE,
                          labels="none",label_n=5,label_size=3.5,show_eq=FALSE,
                          colors=NULL,color_breaks=NULL,point_size=2,alpha=0.7,line_color="#05668D",fit_color="#B2182B",
                          theme="theme_bw",base_size=12,title="",xlab="X",ylab="Y",
                          legend.position="right",x_angle=0){
  has_color<-!is.null(df$color)
  color_num<-has_color&&is.numeric(df$color)
  df$group<-desc_scatter_group(df)
  grouped<-nlevels(df$group)>1
  color_title<-attr(df,"color_name")
  theme_fun<-switch(theme,theme_light=ggplot2::theme_light,theme_minimal=ggplot2::theme_minimal,theme_classic=ggplot2::theme_classic,theme_grey=ggplot2::theme_grey,ggplot2::theme_bw)
  pal_n<-function(n) if(is.null(colors)) grDevices::hcl.colors(n,"Dark 3") else colors(n)
  is_time<-inherits(df$x,c("Date","POSIXt"))
  log_x<-isTRUE(log_x)&&!is_time&&all(df$x>0)
  log_y<-isTRUE(log_y)&&all(df$y>0)

  p<-ggplot2::ggplot(df,ggplot2::aes(x=x,y=y))
  if(isTRUE(one_to_one)&&!is_time) p<-p+ggplot2::geom_abline(slope=1,intercept=0,linetype=2,color="gray50")

  # raw points
  if(identical(agg,"none")||isTRUE(show_raw)){
    a<-if(identical(agg,"none")) alpha else min(alpha,0.25)
    if(color_num){
      p<-p+ggplot2::geom_point(ggplot2::aes(color=color),size=point_size,alpha=a)+
        ggplot2::scale_color_gradientn(colours=if(is.null(colors)) grDevices::hcl.colors(256,"viridis") else colors(256),
                                       name=color_title,limits=range(c(df$color,color_breaks),na.rm=TRUE),
                                       breaks=if(is.null(color_breaks)) ggplot2::waiver() else color_breaks)
    } else if(grouped){
      p<-p+ggplot2::geom_point(ggplot2::aes(color=group),size=point_size,alpha=a)
    } else{
      p<-p+ggplot2::geom_point(color=line_color,size=point_size,alpha=a)
    }
  }
  if(isTRUE(rug)) p<-p+ggplot2::geom_rug(alpha=0.3,color="gray40",length=ggplot2::unit(0.015,"npc"))

  # mean +/- SE or SD of Y at each X value or X class (per group)
  if(!identical(agg,"none")){
    sm<-desc_scatter_aggregate(df,err)
    if(grouped&&!color_num){
      p<-p+ggplot2::geom_errorbar(data=sm,ggplot2::aes(x=x,ymin=mean-err,ymax=mean+err,color=group),inherit.aes=FALSE,width=0,na.rm=TRUE)+
        ggplot2::geom_line(data=sm,ggplot2::aes(x=x,y=mean,color=group,group=group),inherit.aes=FALSE)+
        ggplot2::geom_point(data=sm,ggplot2::aes(x=x,y=mean,color=group),inherit.aes=FALSE,size=point_size*1.2)
    } else{
      p<-p+ggplot2::geom_errorbar(data=sm,ggplot2::aes(x=x,ymin=mean-err,ymax=mean+err),inherit.aes=FALSE,width=0,color=line_color,na.rm=TRUE)+
        ggplot2::geom_line(data=sm,ggplot2::aes(x=x,y=mean),inherit.aes=FALSE,color=line_color)+
        ggplot2::geom_point(data=sm,ggplot2::aes(x=x,y=mean),inherit.aes=FALSE,color=line_color,size=point_size*1.2)
    }
    attr(p,"aggregated")<-sm
  }

  # fitted model (per group), with its confidence/prediction band
  cv<-if(!is.null(fitres)) fitres$curve else NULL
  if(!is.null(cv)&&nrow(cv)){
    cv$group<-factor(cv$group,levels=levels(df$group))
    if(log_y) cv<-cv[!is.na(cv$fit)&cv$fit>0,,drop=FALSE]
    has_band<-any(!is.na(cv$lwr))
    if(grouped&&!color_num){
      if(has_band) p<-p+ggplot2::geom_ribbon(data=cv,ggplot2::aes(x=x,ymin=lwr,ymax=upr,fill=group,group=group),inherit.aes=FALSE,alpha=0.15,na.rm=TRUE)
      p<-p+ggplot2::geom_line(data=cv,ggplot2::aes(x=x,y=fit,color=group,group=group),inherit.aes=FALSE,linewidth=0.9,na.rm=TRUE)
    } else{
      if(has_band) p<-p+ggplot2::geom_ribbon(data=cv,ggplot2::aes(x=x,ymin=lwr,ymax=upr),inherit.aes=FALSE,fill=fit_color,alpha=0.15,na.rm=TRUE)
      p<-p+ggplot2::geom_line(data=cv,ggplot2::aes(x=x,y=fit),inherit.aes=FALSE,color=fit_color,linewidth=0.9,na.rm=TRUE)
    }
    if(isTRUE(show_eq)&&!is.null(fitres$metrics)){
      m<-fitres$metrics
      txt<-paste0(if(grouped) paste0(m$Group,": ") else "",m$Equation,"   R2 = ",formatC(m$R2,digits=3,format="f"))
      p<-p+ggplot2::annotate("text",x=-Inf,y=Inf,hjust=-0.03,vjust=1.3,label=paste(txt,collapse="\n"),size=base_size/3.6,lineheight=1.1)
    }
  }

  # labels: all points, or the observations farthest from the model (or from a linear fit)
  if(!identical(labels,"none")){
    # with aggregation, the labels go to the means
    lab<-if(identical(agg,"none")) df else data.frame(id=sm$id,x=sm$x,y=sm$mean,stringsAsFactors=FALSE)
    if(identical(labels,"residuals")){
      rs<-fitres$resid
      if(is.null(rs)||!any(rs$id%in%lab$id)) rs<-data.frame(id=lab$id,residual=stats::residuals(stats::lm(y~as.numeric(x),data=lab)))
      top<-rs$id[order(-abs(rs$residual))][seq_len(min(label_n,nrow(rs)))]
      lab<-lab[lab$id%in%top,,drop=FALSE]
    }
    p<-p+ggrepel::geom_text_repel(data=lab,ggplot2::aes(x=x,y=y,label=id),inherit.aes=FALSE,size=label_size,max.overlaps=Inf,show.legend=FALSE,seed=1)
  }

  if(grouped&&!color_num){
    p<-p+ggplot2::scale_color_manual(values=pal_n(nlevels(df$group)),name=color_title,drop=FALSE)+
      ggplot2::scale_fill_manual(values=pal_n(nlevels(df$group)),name=color_title,drop=FALSE)
  }
  if(isTRUE(facet)&&grouped) p<-p+ggplot2::facet_wrap(~group)
  if(log_x) p<-p+ggplot2::scale_x_log10()
  if(log_y) p<-p+ggplot2::scale_y_log10()
  p<-p+ggplot2::labs(x=xlab,y=ylab,title=title)+theme_fun(base_size=base_size)+
    ggplot2::theme(legend.position=legend.position)
  if(!is.na(x_angle)&&x_angle>0) p<-p+ggplot2::theme(axis.text.x=ggplot2::element_text(angle=x_angle,hjust=1))
  p
}

# model diagnostics: observed vs fitted, residuals vs fitted, normal Q-Q
#' @export
gg_desc_scatter_diag<-function(fitres,view="obs_fit",colors=NULL,point_size=2,alpha=0.7,line_color="#05668D",
                               theme="theme_bw",base_size=12,title="",legend.position="right",color_title=NULL){
  r<-fitres$resid
  validate(need(!is.null(r)&&nrow(r)>0,"The model could not be fitted."))
  theme_fun<-switch(theme,theme_light=ggplot2::theme_light,theme_minimal=ggplot2::theme_minimal,theme_classic=ggplot2::theme_classic,theme_grey=ggplot2::theme_grey,ggplot2::theme_bw)
  r$group<-factor(r$group)
  grouped<-nlevels(r$group)>1
  pal_n<-function(n) if(is.null(colors)) grDevices::hcl.colors(n,"Dark 3") else colors(n)
  pts<-function(p,mapping){
    if(grouped) p+ggplot2::geom_point(mapping,size=point_size,alpha=alpha)+ggplot2::scale_color_manual(values=pal_n(nlevels(r$group)),name=color_title)
    else p+ggplot2::geom_point(mapping,color=line_color,size=point_size,alpha=alpha)
  }
  if(identical(view,"resid")){
    p<-ggplot2::ggplot(r)+ggplot2::geom_hline(yintercept=0,linetype=2,color="gray50")
    p<-pts(p,if(grouped) ggplot2::aes(x=fitted,y=residual,color=group) else ggplot2::aes(x=fitted,y=residual))
    p<-p+ggplot2::labs(x="Fitted",y="Residual")
  } else if(identical(view,"qq")){
    r<-do.call(rbind,lapply(split(r,r$group,drop=TRUE),function(s){
      s<-s[order(s$residual),,drop=FALSE]
      s$theoretical<-stats::qnorm(stats::ppoints(nrow(s)))
      s$std<-(s$residual-mean(s$residual))/stats::sd(s$residual)
      s
    }))
    p<-ggplot2::ggplot(r)+ggplot2::geom_abline(slope=1,intercept=0,linetype=2,color="gray50")
    p<-pts(p,if(grouped) ggplot2::aes(x=theoretical,y=std,color=group) else ggplot2::aes(x=theoretical,y=std))
    p<-p+ggplot2::labs(x="Theoretical quantiles",y="Standardized residuals")
  } else{
    p<-ggplot2::ggplot(r)+ggplot2::geom_abline(slope=1,intercept=0,linetype=2,color="gray50")
    p<-pts(p,if(grouped) ggplot2::aes(x=fitted,y=y,color=group) else ggplot2::aes(x=fitted,y=y))
    p<-p+ggplot2::labs(x="Fitted",y="Observed")
  }
  p+ggplot2::labs(title=title)+theme_fun(base_size=base_size)+ggplot2::theme(legend.position=legend.position)
}

# ---- Histogram (Descriptive tools, tab 12) -----------------------------------------------
# breaks of one variable: a rule for the number of bins, a fixed number or a fixed width
#' @export
desc_hist_breaks<-function(x,method="sturges",n=20,width=NULL){
  x<-x[is.finite(x)]
  r<-range(x)
  if(diff(r)==0) return(c(r[1]-0.5,r[1]+0.5))
  if(identical(method,"width")&&isTRUE(width>0)){
    br<-seq(floor(r[1]/width)*width,ceiling(r[2]/width)*width,by=width)
    if(max(br)<r[2]) br<-c(br,max(br)+width)
    if(length(br)<2) br<-c(br[1],br[1]+width)
    return(br)
  }
  if(identical(method,"n")){
    n<-max(1,round(n%||%20))
    return(seq(r[1],r[2],length.out=n+1))
  }
  k<-switch(method,fd=grDevices::nclass.FD(x),scott=grDevices::nclass.scott(x),grDevices::nclass.Sturges(x))
  br<-pretty(r,n=max(1,k))
  br
}

# rectangles of the histogram: one row per variable x group x bin; y in counts,
# density (each group integrates to 1) or percent (of each group)
#' @export
desc_hist_data<-function(long,method="sturges",n=20,width=NULL,ytype="count",position="overlay"){
  out<-lapply(split(long,long$variable,drop=TRUE),function(d){
    br<-desc_hist_breaks(d$value,method,n,width)
    groups<-levels(d$group)
    groups<-groups[groups%in%d$group]
    ng<-length(groups)
    r<-do.call(rbind,lapply(seq_along(groups),function(gi){
      v<-d$value[d$group==groups[gi]]
      h<-graphics::hist(v,breaks=br,plot=FALSE,include.lowest=TRUE,right=TRUE)
      w<-diff(br)
      y<-switch(ytype,density=h$counts/(length(v)*w),percent=100*h$counts/length(v),h$counts)
      data.frame(variable=d$variable[1],group=groups[gi],gi=gi,xmin=br[-length(br)],xmax=br[-1],count=h$counts,y=y,stringsAsFactors=FALSE)
    }))
    r$ymin<-0
    r$ymax<-r$y
    if(identical(position,"stack")&&ng>1){
      r<-r[order(r$xmin,r$gi),]
      r$ymax<-stats::ave(r$y,r$xmin,FUN=cumsum)
      r$ymin<-r$ymax-r$y
    }
    if(identical(position,"dodge")&&ng>1){
      w<-(r$xmax-r$xmin)/ng
      r$xmin<-r$xmin+(r$gi-1)*w
      r$xmax<-r$xmin+w
    }
    attr(r,"breaks")<-br
    r
  })
  res<-do.call(rbind,out)
  rownames(res)<-NULL
  attr(res,"breaks")<-lapply(out,attr,"breaks")
  res
}

# density / normal curves in the units of the y axis
#' @export
desc_hist_curves<-function(long,breaks,ytype="count",kind="density"){
  out<-lapply(split(long,list(long$variable,long$group),drop=TRUE),function(d){
    v<-d$value
    if(length(v)<2||stats::sd(v)==0) return(NULL)
    br<-breaks[[as.character(d$variable[1])]]
    w<-stats::median(diff(br))
    if(identical(kind,"normal")){
      xs<-seq(min(br),max(br),length.out=200)
      ys<-stats::dnorm(xs,mean(v),stats::sd(v))
    } else{
      dd<-stats::density(v)
      xs<-dd$x
      ys<-dd$y
    }
    k<-switch(ytype,density=1,percent=100*w,length(v)*w)
    data.frame(variable=d$variable[1],group=d$group[1],x=xs,y=ys*k,stringsAsFactors=FALSE)
  })
  out<-do.call(rbind,out)
  if(!is.null(out)) rownames(out)<-NULL
  out
}

# summary of each variable (and group)
#' @export
desc_hist_summary<-function(long,breaks){
  out<-lapply(split(long,list(long$variable,long$group),drop=TRUE),function(d){
    v<-d$value
    n<-length(v)
    m<-mean(v)
    s<-if(n>1) stats::sd(v) else NA_real_
    br<-breaks[[as.character(d$variable[1])]]
    sk<-if(isTRUE(s>0)) mean((v-m)^3)/s^3 else NA_real_
    ku<-if(isTRUE(s>0)) mean((v-m)^4)/s^4-3 else NA_real_
    sh<-if(n>=3&&n<=5000&&isTRUE(s>0)) stats::shapiro.test(v)$p.value else NA_real_
    data.frame(Variable=as.character(d$variable[1]),Group=as.character(d$group[1]),n=n,Mean=m,SD=s,Median=stats::median(v),
               IQR=stats::IQR(v),Min=min(v),Max=max(v),Skewness=sk,Excess_kurtosis=ku,Shapiro_p=sh,
               Bins=length(br)-1,Bin_width=stats::median(diff(br)),stringsAsFactors=FALSE)
  })
  out<-do.call(rbind,out)
  rownames(out)<-NULL
  out
}

#' @export
gg_desc_hist<-function(long,method="sturges",n=20,width=NULL,ytype="count",position="overlay",
                       density=FALSE,normal=FALSE,mean_line=FALSE,median_line=FALSE,rug=FALSE,
                       fill="#77AADD",border="black",alpha=0.8,colors=NULL,line_color="#B2182B",
                       theme="theme_bw",base_size=12,title="",xlab="Value",ylab=NULL,
                       ncol=NULL,free_y=TRUE,legend.position="right",x_angle=0,group_name="Group"){
  hd<-desc_hist_data(long,method,n,width,ytype,position)
  br<-attr(hd,"breaks")
  groups<-levels(long$group)
  grouped<-length(groups)>1
  theme_fun<-switch(theme,theme_light=ggplot2::theme_light,theme_minimal=ggplot2::theme_minimal,theme_classic=ggplot2::theme_classic,theme_grey=ggplot2::theme_grey,ggplot2::theme_bw)
  pal<-if(grouped) (if(is.null(colors)) grDevices::hcl.colors(length(groups),"Dark 3") else colors(length(groups))) else fill
  hd$group<-factor(hd$group,levels=groups)
  a<-if(grouped&&identical(position,"overlay")) min(alpha,0.5) else alpha
  p<-ggplot2::ggplot()
  if(grouped){
    p<-p+ggplot2::geom_rect(data=hd,ggplot2::aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=group),color=border,alpha=a,linewidth=0.2)+
      ggplot2::scale_fill_manual(values=pal,name=group_name,drop=FALSE)+
      ggplot2::scale_color_manual(values=pal,name=group_name,drop=FALSE,guide="none")
  } else{
    p<-p+ggplot2::geom_rect(data=hd,ggplot2::aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill=fill,color=border,alpha=a,linewidth=0.2)
  }
  # curves are drawn per group (not stacked)
  add_curve<-function(p,cv,lt){
    if(is.null(cv)||!nrow(cv)) return(p)
    cv$group<-factor(cv$group,levels=groups)
    if(grouped) p+ggplot2::geom_line(data=cv,ggplot2::aes(x=x,y=y,color=group),linetype=lt,linewidth=0.8)
    else p+ggplot2::geom_line(data=cv,ggplot2::aes(x=x,y=y),color=line_color,linetype=lt,linewidth=0.8)
  }
  if(isTRUE(density)) p<-add_curve(p,desc_hist_curves(long,br,ytype,"density"),"solid")
  if(isTRUE(normal)) p<-add_curve(p,desc_hist_curves(long,br,ytype,"normal"),"dashed")
  stat_lines<-function(p,fun,lt){
    st<-stats::aggregate(value~variable+group,long,fun)
    st$group<-factor(st$group,levels=groups)
    if(grouped) p+ggplot2::geom_vline(data=st,ggplot2::aes(xintercept=value,color=group),linetype=lt,linewidth=0.7)
    else p+ggplot2::geom_vline(data=st,ggplot2::aes(xintercept=value),color=line_color,linetype=lt,linewidth=0.7)
  }
  if(isTRUE(mean_line)) p<-stat_lines(p,mean,"solid")
  if(isTRUE(median_line)) p<-stat_lines(p,stats::median,"dotted")
  if(isTRUE(rug)){
    if(grouped) p<-p+ggplot2::geom_rug(data=long,ggplot2::aes(x=value,color=group),alpha=0.4,sides="b",inherit.aes=FALSE)
    else p<-p+ggplot2::geom_rug(data=long,ggplot2::aes(x=value),alpha=0.4,sides="b",inherit.aes=FALSE,color="gray30")
  }
  nvar<-length(unique(long$variable))
  scales<-if(isTRUE(free_y)) "free" else "free_x"
  if(identical(position,"facet")&&grouped){
    p<-p+ggplot2::facet_grid(group~variable,scales=scales)
  } else if(nvar>1){
    p<-p+ggplot2::facet_wrap(~variable,scales=scales,ncol=ncol)
  }
  ylab<-ylab%||%switch(ytype,density="Density",percent="Percent",
                       "Count")
  p<-p+ggplot2::labs(x=xlab,y=ylab,title=title)+theme_fun(base_size=base_size)+ggplot2::theme(legend.position=legend.position)
  if(!is.na(x_angle)&&x_angle>0) p<-p+ggplot2::theme(axis.text.x=ggplot2::element_text(angle=x_angle,hjust=1))
  attr(p,"breaks")<-br
  p
}
