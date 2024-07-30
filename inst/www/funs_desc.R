#' @export

ggbox<-function(res,pal,violin=F,horiz=F,base_size=12,cex.axes=1,cex.lab=1,
                cex.main=1,xlab=colnames(res)[1],ylab=colnames(res)[2],main="",
                box_linecol="firebrick",box_alpha=0.7,newcolhabs,cex.label_panel=10,varwidth=F, linewidth=.8, theme='theme_bw', grid=T, background="white",xlab_rotate=0,ylab_rotate=0,nrow=NULL,ncol=2,box_title_font="italic",subtitle=NULL,cex.subtitle=10,              box_subtitle_font="plain") {
  wrap=F
  if(is.na(nrow)){
    nrow=NULL
  }
  if(is.na(ncol)){
    ncol=NULL
  }
  if(ncol(res)>2){
    res2<-res
    colnames(res2)[1]<-c("x")
    res2<-reshape2::melt(res2,"x")
    colnames(res2)[3]<-"y"
    res<-res2
    wrap=T

  } else{
    colnames(res)<-c("x","y")
  }

  coline<-box_linecol
  cols<-newcolhabs[[pal]](nlevels(res$x))
  cols<-lighten(cols,box_alpha)
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

  if(isTRUE(wrap)){

    p<-p+facet_wrap(~variable, scales = "free_y",nrow=nrow,ncol=ncol)
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
  pic<-findCorrelation(cordata,
                       cutoff = cor_cutoff, # the absolute value of a correlation we'd deem as high
                       verbose = T,
                       names = F,
                       exact = T)




  if(ret=="lower"){
    if(!length(pic)>0){
      attr(cordata,"war")<-paste0("Note: All correlations < =",cor_cutoff)
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


  heatmap.2(cordata,
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
  pw.Models <- pwRDA.source(x.ord, y.ord, BPs)
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
                   R.boot[b] <- pwRDA.source(new.x, comm.rand, BPs)$summ[2]

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
    d <- vegdist(rbind(half.a, half.b), dist)
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
                     border_palette=turbo,
                     fill_palette=turbo,
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
    new_scale_fill()+
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
