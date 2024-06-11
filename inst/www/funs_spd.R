#' @export
pnb<-function (x, vars=NULL, n = 1000, legendplot = TRUE, lty=NULL, col=NULL, ylab = "Density",main = "Naive Bayes Plot", ...){
  if (is.null(vars)) {
    vars <- names(x$tables)}
  if (is.null(lty)) {
    lty <- seq(along = x$apriori)}
  if (is.null(col)) {
    col <- rainbow(length(x$apriori))}
  vars <- vars[is.element(vars, names(x$tables))]

  result<-list()
  if (length(vars)) {
    j=i=1
    for (j in seq(along = vars)) {
      dummy <- (x$tables[names(x$tables) == as.name(vars[j])])
      if (inherits(dummy[[1]], "matrix")) {
        dummy <- data.frame(dummy)
        plotvector <- seq(min(x$x[, vars[j]]), max(x$x[,
                                                       vars[j]]), len = n)
        pv <- matrix(0, nrow = nrow(dummy), ncol = n)
        for (i in 1:nrow(dummy)) pv[i, ] <- dnorm(plotvector,
                                                  mean = dummy[i, 1], sd = dummy[i, 2]) * x$apriori[i]
        plot(plotvector, pv[1, ], type = "l", lty = lty[1],
             ylim = c(0, max(pv)), xlab = vars[j], ylab = ylab,
             col = col[1], main = main)
        for (i in 2:nrow(dummy)) lines(plotvector, pv[i,
        ], lty = lty[i], col = col[i])
        if (legendplot)
          legend(min(plotvector), max(pv), legend = rownames(dummy),
                 lty = lty, col = col)
      }
      if (inherits(dummy[[1]], "table")) {
        mosaicplot(dummy[[1]], main = main)}
      if (inherits(dummy[[1]], "list")) {
        plotvector <- seq(min(x$x[, vars[j]]), max(x$x[,vars[j]]), len = n)
        pv <- matrix(0, nrow = length(dummy[[1]]), ncol = n)
        for (i in seq(along = dummy[[1]])) pv[i, ] <- klaR::dkernel(plotvector,
                                                                    kernel = dummy[[1]][[i]]) * x$apriori[i]
      }

      result[[vars[j]]]<-list(plotvector=plotvector,
                              pv=pv,
                              x_scaled=scales::rescale(plotvector,c(0,1)),
                              y_scaled=scales::rescale(pv,c(0,1)))

    }}
  nbden<-result
  recla2<-lapply(names(nbden),function(var) {
    x<-nbden[[var]]
    recla<-lapply(1:nrow(x$pv),function(i){
      data.frame(var=var,x=x$x_scaled,y=x$pv[i,],class=i)
    })
    (do.call(rbind,recla))

  })
  names(recla2)<-vars
  recla2

}



#' @export
gg_pairplot<-function(x,y,cols,xlab="",ylab="",title="",include.y=F,cor.size=1,varnames.size=1,points.size=1,axis.text.size=1,axis.title.size=1,plot.title.size=1,plot.subtitle.size=1,legend.text.size=1,legend.title.size=1,alpha.curve=.5, alpha.points=1, method = "pearson",round=3,title_corr=paste(method,"corr"),subtitle=paste("*** p< 0.001,","** p< 0.05 and","* p< 0.1"),switch = NULL,
                      upper="cor", pch=16){

  if(!is.null(y)){
    df=data.frame(x,y=y[,1])
    li<-split(x,y[,1])
    row1<-F
    if(any(sapply(li,nrow)==1)){
      row1=T
    }
  } else{
    df<-x
    row1<-F
  }



  col.curve<-adjustcolor(cols,alpha.curve)
  col.points<-adjustcolor(cols,alpha.points)

  cor.size=cor.size*3
  points.size=points.size*3
  axis.text.size=axis.text.size*10
  axis.title.size=axis.title.size*3
  plot.title.size=plot.title.size*10
  plot.subtitle.size=plot.subtitle.size*10
  legend.text.size=legend.text.size*10
  legend.title.size=legend.title.size*10
  varnames.size=varnames.size*10

  if(isTRUE(include.y)){
    columns=1:ncol(df)
    columnLabels=colnames(cbind(x,y))
  } else{
    columns=1:(ncol(df)-1)
    columnLabels=colnames(x)
  }
  if(is.null(y)){
    columns=1:ncol(x)
  }
  if(upper=="cor"){
    fun_upper<-list(continuous = function(data, mapping, ...) {
      if(isTRUE(row1)){
        mapping$colour<-NULL
      }
      if(is.null(y)){
        mapping$colour<-NULL
      }

      GGally::ggally_cor(data = df,
                 mapping = mapping,
                 size=cor.size,

                 method=method,
                 digits = round,
                 title = title_corr) + scale_colour_manual(values = cols)})

  } else{
    fun_upper<-"blank"
  }



  p<-ggpairs(df,
             columns =columns ,
             switch = switch,

             columnLabels=columnLabels,
             aes(colour = if(!is.null(y)){y} else{NULL}),
             upper =fun_upper,
             lower = list(continuous = function(data, mapping, ...) {
               if(is.null(y)){
                 mapping$colour<-NULL
               }
               GGally::ggally_points(data = df, mapping = mapping, size=points.size, shape=pch) + scale_colour_manual(values = col.points)}),
             diag = list(continuous = function(data, mapping, ...) {
               if(is.null(y)){
                 mapping$colour<-NULL
               }
               GGally::ggally_densityDiag(data = df, mapping = mapping) + scale_fill_manual(values = col.curve)}))

  p<-p+labs(title = title,
            tag = subtitle)
  p<-p+xlab(xlab)+ylab(ylab)

  p<- p+theme(
    plot.tag.position = "bottom",
    panel.grid.major = element_blank(),
    panel.background=element_rect(fill=NA, color="white"),
    panel.border = element_rect(fill=NA,color="black", linewidth=0.5, linetype="solid"),
    axis.line=element_line(),
    axis.text=element_text(size=axis.text.size),
    axis.title=element_text(size=axis.title.size),
    plot.title=element_text(size=plot.title.size),
    plot.tag=element_text(size=plot.subtitle.size,face="italic"),
    plot.subtitle=element_text(size=plot.subtitle.size,face="italic"),
    legend.text=element_text(size=legend.text.size),
    legend.title=element_text(size=legend.title.size),
    strip.text.x = element_text(size = varnames.size),
    strip.text.y = element_text(size = varnames.size)
  )



  if(fun_upper!="blank"){
    if(isTRUE(row1)){
      attr(p,"row1")<-"Warning: Due to insufficient data points in certain groups (fewer than two), the correlation per group could not be calculated. Only the overall correlation will be displayed"
    }

  }

  if(isTRUE(include.y)){
    for(i in 1:ncol(df)){
      p[i,ncol(df)] <- p[i,ncol(df)] +
        scale_fill_manual(values=col.points)

    }
    for(i in 1:ncol(df)){
      p[ncol(df),i] <- p[ncol(df),i] +
        scale_fill_manual(values=col.points)

    }
  }


  return(p)

}

#' @export
gg_pairplot2<-function(x,y,cols,xlab="",ylab="",title="",include.y=F,cor.size=1,varnames.size=1,points.size=1,axis.text.size=1,axis.title.size=1,plot.title.size=1,plot.subtitle.size=1,legend.text.size=1,legend.title.size=1,alpha.curve=.5, alpha.points=1, method = "pearson",round=3,title_corr=paste(method,"corr"),subtitle=paste("*** p< 0.001,","** p< 0.05 and","* p< 0.1"),
                      switch = NULL,
                      pch=16,
                      upper=c("corr","corr+group","blank"),
                      lower=c("points","points+group","blank"),
                      diag=c("density","density+group","hist","hist+group","blank")){
  diag<-match.arg(diag,c("density","density+group","hist","hist+group","blank"))
  lower<-match.arg(lower,c("points","points+group","blank"))
  upper<-match.arg(upper,c("corr","corr+group","blank"))

  if(!is.null(y)){
    df=data.frame(x,y=y[,1])
    li<-split(x,y[,1])
    row1<-F
    if(any(sapply(li,nrow)==1)){
      row1=T
    }
  } else{
    df<-x
    row1<-F
  }



  col.curve<-adjustcolor(cols,alpha.curve)
  col.points<-adjustcolor(cols,alpha.points)

  cor.size=cor.size*3
  points.size=points.size*3
  axis.text.size=axis.text.size*10
  axis.title.size=axis.title.size*3
  plot.title.size=plot.title.size*10
  plot.subtitle.size=plot.subtitle.size*10
  legend.text.size=legend.text.size*10
  legend.title.size=legend.title.size*10
  varnames.size=varnames.size*10

  if(isTRUE(include.y)){
    columns=1:ncol(df)
    columnLabels=colnames(cbind(x,y))
  } else{
    columns=1:(ncol(df)-1)
    columnLabels=colnames(x)
  }
  if(is.null(y)){
    columns=1:ncol(x)
  }
  if(upper%in%c("corr","corr+group")){
    fun_upper<-list(continuous = function(data, mapping, ...) {
      if(isTRUE(row1)){
        mapping$colour<-NULL
      }
      if(is.null(y)){
        mapping$colour<-NULL
      }
      if(upper=="corr"){
        mapping$colour<-NULL
        GGally::ggally_cor(data = df,
                   mapping = mapping,
                   size=cor.size,

                   method=method,
                   digits = round,
                   title = title_corr)
      } else{
        GGally::ggally_cor(data = df,
                   mapping = mapping,
                   size=cor.size,

                   method=method,
                   digits = round,
                   title = title_corr) + scale_colour_manual(values = cols)
      }

     })

  } else{
    fun_upper<-"blank"
  }

  if(lower%in%c("points","points+group")){
    fun_lower<-list(continuous = function(data, mapping, ...) {
      if(is.null(y)){
        mapping$colour<-NULL
      }
      if(lower=="points"){
        mapping$colour<-NULL

      }

      GGally::ggally_points(data = df, mapping = mapping, size=points.size, shape=pch) + scale_colour_manual(values = col.points)})

  } else{
    fun_lower<-"blank"
  }

  if(diag%in%c("density","density+group")){
    fun_diag<-list(continuous = function(data, mapping, ...) {
      if(is.null(y)){mapping$colour<-NULL}
      if(diag=="density"){
        mapping$colour<-NULL
        GGally::ggally_densityDiag(data = df, mapping = mapping)
      } else{
        GGally::ggally_densityDiag(data = df, mapping = mapping) +
          scale_fill_manual(values = col.curve)
      }
      })
  } else if(diag%in%c("hist","hist+group")) {
    fun_diag<-list(continuous = function(data, mapping, ...) {
      if(is.null(y)){mapping$colour<-NULL}
      if(diag=="hist"){
        mapping$colour<-NULL
        GGally::ggally_barDiag(data = df, mapping = mapping)
      } else{
        GGally::ggally_barDiag(data = df, mapping = mapping) +
          scale_fill_manual(values = col.curve)
      }
     })

  } else {
    fun_diag<-"blank"
  }



  p<-ggpairs(df,
             columns =columns ,
             switch = switch,

             columnLabels=columnLabels,
             aes(colour = if(!is.null(y)){y} else{NULL}),
             upper =fun_upper,
             lower = fun_lower,
             diag = fun_diag)

  p<-p+labs(title = title,
            tag = subtitle)
  p<-p+xlab(xlab)+ylab(ylab)

  p<- p+theme(
    plot.tag.position = "bottom",
    panel.grid.major = element_blank(),
    panel.background=element_rect(fill=NA, color="white"),
    panel.border = element_rect(fill=NA,color="black", linewidth=0.5, linetype="solid"),
    axis.line=element_line(),
    axis.text=element_text(size=axis.text.size),
    axis.title=element_text(size=axis.title.size),
    plot.title=element_text(size=plot.title.size),
    plot.tag=element_text(size=plot.subtitle.size,face="italic"),
    plot.subtitle=element_text(size=plot.subtitle.size,face="italic"),
    legend.text=element_text(size=legend.text.size),
    legend.title=element_text(size=legend.title.size),
    strip.text.x = element_text(size = varnames.size),
    strip.text.y = element_text(size = varnames.size)
  )



  if(fun_upper!="blank"){
    if(isTRUE(row1)){
      attr(p,"row1")<-"Warning: Due to insufficient data points in certain groups (fewer than two), the correlation per group could not be calculated. Only the overall correlation will be displayed"
    }

  }

  if(isTRUE(include.y)){
    for(i in 1:ncol(df)){
      p[i,ncol(df)] <- p[i,ncol(df)] +
        scale_fill_manual(values=col.points)

    }
    for(i in 1:ncol(df)){
      p[ncol(df),i] <- p[ncol(df),i] +
        scale_fill_manual(values=col.points)

    }
  }


  return(p)

}
#' @export


tree_func <- function(final_model,acc="",base_size=11,predall=predict(final_model,newdata,predict.all=T)$individual,
                      tree_num, newcolhabs=list(turbo=turbo),palette="turbo", round=2,
                      tree_type="dendrogram",
                      edge_type="link",repel=T) {

  size=base_size/3


  tree <- randomForest::getTree(final_model,
                                k = tree_num,
                                labelVar = TRUE) %>%
    tibble::rownames_to_column() %>%
    # make leaf split points to NA, so the 0s won't get plotted
    mutate(`split point` = ifelse(is.na(prediction), `split point`, NA))

  # prepare data frame for graph
  graph_frame <- data.frame(from = rep(tree$rowname, 2),
                            to = c(tree$`left daughter`, tree$`right daughter`))

  # convert to graph and delete the last node that we don't want to plot
  graph <- graph_from_data_frame(graph_frame) %>%
    delete_vertices("0")

  # set node labels
  igraph::V(graph)$node_label <- gsub("_", " ", as.character(tree$`split var`))
  igraph::V(graph)$leaf_label <- as.character(tree$prediction)
  igraph::V(graph)$split <- as.character(round(tree$`split point`, digits = round))

  if(tree_type=="dendrogram"){
    p<- ggraph::ggraph(graph, 'dendrogram')
  } else{
    p<- ggraph::ggraph(graph, layout = 'igraph', algorithm = 'tree')

  }

  if(edge_type=="link"){
    p<-p+geom_edge_link()
  } else{
    p<- p+geom_edge_diagonal(edge_width = 0.5, alpha =.4)

  }

  p<-p+
    geom_node_point() +
    geom_node_text(aes(label = node_label), vjust = "top",hjust="inward",na.rm = TRUE, repel = repel, color="darkblue", size=size) +
    geom_node_label(aes(label = split),  vjust = "top", na.rm = TRUE, fill = "white", size=size) +
    geom_node_label(aes(label = leaf_label, fill = leaf_label), na.rm = TRUE, size=size*1.2,
                    repel = repel, colour = "white", fontface = "bold", show.legend = FALSE)


  p<-p+labs(title=paste0("Tree:",tree_num),subtitle=  paste0(names(acc),":",acc, collapse="\n"))
  p<-p+scale_fill_manual(values=newcolhabs[[palette]](length(unique(na.omit(tree$prediction)))))

  p



}

#' @export
knn_pca<-function(m,plot_grid=T,grid_res=100,bg_alpha=0.8,newcolhabs,palette, scale=T, show_labels=T,base_size=11){
  knn<-m$finalModel
  traindata=getdata_model(m)
  testdata=getdata_model(m,"test")
  cols<-newcolhabs[[palette]](length(m$levels))
  if(isTRUE(scale)){
    traindata<-scale(traindata)
  }

  pca_train<-prcomp(traindata)
  traindata_pca<-data.frame(pca_train$x[,1:2])
  comps<-summary(pca_train)
  exp_pc1<-paste("PC I (",round(comps$importance[2,1]*100,2),"%", ")", sep="")
  exp_pc2<-paste("PC II (",round(comps$importance[2,2]*100,2),"%", ")", sep="")


  if(isTRUE(plot_grid)){
    kernfit<-if (is.factor(testdata)) {knn3(as.matrix(traindata_pca), testdata, k = knn$k)} else {
      knnreg(as.matrix(traindata_pca),testdata, k = knn$k)}
    PC1 <- seq(from = min(traindata_pca$PC1), to = max(traindata_pca$PC1), length = grid_res)
    PC2 <- seq(from = min(traindata_pca$PC2), to = max(traindata_pca$PC2), length = grid_res)
    x.grid <- expand.grid(PC1, PC2)
    # Get class predictions over grid
    pred <- predict(kernfit, newdata = x.grid, "class")

    xyz<-x.grid
    colnames(xyz)<-c("x","y")
    xyz$z<-pred
    predtrain<-predict(kernfit,traindata_pca,"class")
    predtrain<-predict(knn,traindata,"class")

    p<-ggplot(xyz)
    p<-p+ggplot2::geom_raster(aes(x=x,y=y, fill=z))

  } else{
    predtrain<-predict(knn,traindata,"class")
    xyz<-data.frame(traindata_pca,predtrain)
    colnames(xyz)<-c("x","y","z")
    p<-ggplot(xyz)
    # p<-p+ggplot2::geom_po(aes(x=x,y=y, fill=z))+scale_fill_manual(name="",values=cols)
  }

  {
    a<-diff(range(xyz$x))*.7
    ddd<-data.frame(pca_train$rotation[,1:2]*7)
    ddd$x<-ddd$y<-0
    ddd$label<-rownames(ddd)






  }

  {

    p<-p+geom_segment(data=ddd,aes(x=x,y=y,xend=PC1,yend=PC2))+geom_label(data=ddd,aes(x=PC1,y=PC2,label=label))+theme_bw(base_size = base_size)+xlab(exp_pc1)+ylab(exp_pc2)
    geopoint1<-data.frame(x=traindata_pca[,1],y=traindata_pca[,2],class=predtrain,fac=1:nrow(traindata_pca))
    geopoint2<-data.frame(x=traindata_pca[,1],y=traindata_pca[,2],class=testdata,fac=1:nrow(traindata_pca))
    geopoint<-rbind(geopoint1,geopoint2)

    li<-split(geopoint,geopoint$fac)
    x<-li[[1]]
    newdf<-data.frame(do.call(rbind,lapply(li,function(x){
      c(x=mean(x$x),y=mean(x$y),group=NA,table(x$class))
    }))
    )
    newdf$group<-1:nrow(newdf)
    colnames(newdf)[4:ncol(newdf)]<-levels(geopoint$class)
    p<-p+ geom_scatterpie(aes(x=x, y=y, group=group), data=newdf,
                          cols=colnames(newdf)[4:ncol(newdf)],
                          color='white',
                          alpha=.8,pie_scale =base_size/11)

    newdf2<-newdf
    newdf2<-id<-rownames(traindata)
    if(isTRUE(show_labels)){
    p<-p+geom_text(aes(x=x, y=y, label=rownames(traindata)), data=newdf)
    }
    p<-p+scale_fill_manual(name="",values=cols)+labs(subtitle="pie left: pred \npie right: obs")

  }
  p
}
#' @export
plot_pairs<-function(df, my_cols,base_size=1,...){
  graphics.off()
  args <- list(...)

  par(cex.axis=base_size)
  panel.cor <- function(x, y, method="pearson", digits=3, cex.cor=1.2, no.col=FALSE)
  {
    #usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1),cex.axis=base_size)
    r <- cor(x, y, method=method)
    ra <- cor.test(x, y, method=method)$p.value
    txt <- round(r, digits)
    prefix <- ""
    if(ra <= 0.1) prefix <- "."
    if(ra <= 0.05) prefix <- "*"
    if(ra <= 0.01) prefix <- "**"
    if(ra <= 0.001) prefix <- "***"
    if(no.col)  {
      color <- 1
      if(r < 0) { if(ra <= 0.001) sig <- 4 else sig <- 3 }
      else { if(ra <= 0.001) sig <- 2 else sig <- 1 }
    }  else  {
      sig <- 1
      if(ra <= 0.001) sig <- 2
      color <- 2
      if(r < 0) color <- 4
    }
    txt <- paste(txt, prefix, sep="\n")
    text(0.5, 0.5, txt, cex = cex.cor, font=sig, col=color)
  }


  ## Put histograms on the diagonal
  panel.hist <- function(x, base_size=1,no.col=FALSE, ...)
  {
    usr <- par("usr")
    par(usr = c(usr[1:2], 0, 1.5),cex.axis=base_size )
    his <- hist(x, plot=FALSE)
    breaks <- his$breaks; nB <- length(breaks)
    y <- his$counts
    y <- y/max(y)
    if(no.col){
      par(cex.axis=base_size)
      rect(breaks[-nB], 0, breaks[-1], y, col="gray", ...)}  else {
        par(cex.axis=base_size)
        rect(breaks[-nB], 0, breaks[-1], y, col="grey", ...)}
  }


  ## Add black lowess curves to scatter plots
  panel.smoothb <- function (x, y, col=my_cols, bg=NA,
                             cex=1, col.smooth="black", span=2/3, iter=3, ...) {

    par(cex.axis=base_size )
    points(x, y, pch=16, col=col, bg=bg, cex=cex)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok))
      par(cex.axis=base_size)
      lines(stats::lowess(x[ok], y[ok], f=span, iter=iter), col=col.smooth, ...)
  }
  par(cex.axis=base_size)
  graphics::pairs(df, lower.panel=panel.smoothb, upper.panel=panel.cor, diag.panel=panel.hist,...)
  p=recordPlot()
  p
}

#' @export
pd_predfun_ensemble<-function(em,newdata,i){
  modelist<-attr(em,"modelist")
  weis<-em$weis
  probs<-lapply(modelist,function(x) {
    res<-predict(x,newdata, type="prob")
    res[,i]

  })
  mean(apply(do.call(data.frame,probs),1,function(x) {weighted.mean(x,weis)}))
}
#' @export
ensemble_pardep<-function (object, pred.var, pred.grid, pred.fun, inv.link, ice,task, which.class, logit, train, progress, parallel,paropts,...){

  if (isTRUE(progress) && isTRUE(parallel)) {
    progress <- FALSE
    warning("progress bars are disabled whenever `parallel = TRUE`.",
            call. = FALSE, immediate. = TRUE)
  }
  `%pardo%` <- if (isTRUE(parallel)) {
    `%dopar%`
  }  else {
    `%do%`
  }
  if (is.null(pred.fun)) {
    if (task == "regression" && isFALSE(ice)) {
      if (isTRUE(progress)) {
        pb <- utils::txtProgressBar(min = 0, max = nrow(pred.grid),
                                    style = 3)
      }
      yhat <- foreach(i = seq_len(nrow(pred.grid)), .combine = "c",
                      .packages = paropts$.packages, .export = paropts$export) %pardo%
        {
          temp <- train
          temp[, pred.var] <- pred.grid[i, pred.var]
          preds <- mean(get_predictions(object, newdata = temp,
                                        inv.link = inv.link, ...), na.rm = TRUE)
          if (isTRUE(progress)) {
            utils::setTxtProgressBar(pb, value = i)
          }
          preds
        }
      res <- cbind(pred.grid, yhat = yhat)
    }
    if (task == "classification" && isFALSE(ice)) {
      if (isTRUE(progress)) {
        pb <- utils::txtProgressBar(min = 0, max = nrow(pred.grid),
                                    style = 3)
      }
      yhat <- foreach(i = seq_len(nrow(pred.grid)), .combine = "c",
                      .packages = paropts$.packages, .export = paropts$export) %pardo%
        {
          temp <- train
          temp[, pred.var] <- pred.grid[i, pred.var]
          preds <- mean(get_probs(object, newdata = temp,
                                  which.class = which.class, logit = logit,
                                  ...), na.rm = TRUE)
          if (isTRUE(progress)) {
            utils::setTxtProgressBar(pb, value = i)
          }
          preds
        }
      res <- cbind(pred.grid, yhat = yhat)
    }
    if (task == "regression" && isTRUE(ice)) {
      if (isTRUE(progress)) {
        pb <- utils::txtProgressBar(min = 0, max = nrow(pred.grid),
                                    style = 3)
      }
      yhat <- foreach(i = seq_len(nrow(pred.grid)), .combine = "c",
                      .packages = paropts$.packages, .export = paropts$export) %pardo%
        {
          temp <- train
          temp[, pred.var] <- pred.grid[i, pred.var]
          preds <- get_predictions(object, newdata = temp,
                                   inv.link = inv.link, ...)
          if (isTRUE(progress)) {
            utils::setTxtProgressBar(pb, value = i)
          }
          preds
        }
      grid.id <- rep(seq_len(nrow(pred.grid)), each = nrow(train))
      yhat.id <- rep(seq_len(nrow(train)), times = nrow(pred.grid))
      res <- data.frame(pred.grid[grid.id, ], yhat = yhat,
                        yhat.id = yhat.id)
      colnames(res) <- c(colnames(pred.grid), "yhat",
                         "yhat.id")
    }
    if (task == "classification" && isTRUE(ice)) {
      if (isTRUE(progress)) {
        pb <- utils::txtProgressBar(min = 0, max = nrow(pred.grid),
                                    style = 3)
      }
      yhat <- foreach(i = seq_len(nrow(pred.grid)), .combine = "c",
                      .packages = paropts$.packages, .export = paropts$export) %pardo%
        {
          temp <- train
          temp[, pred.var] <- pred.grid[i, pred.var]
          preds <- get_probs(object, newdata = temp,
                             which.class = which.class, logit = logit,
                             ...)
          if (isTRUE(progress)) {
            utils::setTxtProgressBar(pb, value = i)
          }
          preds
        }
      if (isTRUE(progress)) {
        utils::setTxtProgressBar(pb, value = i)
      }
      grid.id <- rep(seq_len(nrow(pred.grid)), each = nrow(train))
      yhat.id <- rep(seq_len(nrow(train)), times = nrow(pred.grid))
      res <- data.frame(pred.grid[grid.id, ], yhat = yhat,
                        yhat.id = yhat.id)
      colnames(res) <- c(colnames(pred.grid), "yhat",
                         "yhat.id")
    }
  }  else {
    if (isTRUE(progress)) {
      pb <- utils::txtProgressBar(min = 0, max = nrow(pred.grid),
                                  style = 3)
    }
    yhat <- foreach(i = seq_len(nrow(pred.grid)), .combine = "c",
                    .packages = paropts$.packages, .export = paropts$export) %pardo%
      {
        temp <- train
        temp[, pred.var] <- pred.grid[i, pred.var]
        preds <- pred.fun(object, newdata = temp, which.class)
        if (isTRUE(progress)) {
          utils::setTxtProgressBar(pb, value = i)
        }
        preds
      }
    len <- length(yhat)/nrow(pred.grid)
    if (len == 1L) {
      res <- cbind(pred.grid, yhat = yhat)
    }    else {
      grid.id <- rep(seq_len(nrow(pred.grid)), each = len)
      yhat.id <- if (is.null(names(yhat))) {
        rep(seq_len(len), times = nrow(pred.grid))
      }      else {
        names(yhat)
      }
      res <- data.frame(pred.grid[grid.id, ], yhat = yhat,
                        yhat.id = yhat.id)
      colnames(res) <- c(colnames(pred.grid), "yhat",
                         "yhat.id")
    }
  }
  if (isTRUE(progress)) {
    close(pb)
  }
  return(res)
}
#' @export
getmodelist_pd1<-function(saved_data){
  nb=lapply(saved_data, function(x) attr(x,"nb"))
  rf=lapply(saved_data, function(x) attr(x,"rf"))
  sgboost=lapply(saved_data, function(x)attr(x,"sgboost"))
  svm=lapply(saved_data, function(x) attr(x,"svm"))
  knn=lapply(saved_data, function(x) attr(x,"knn"))
  res<-do.call(c,c(nb,svm,knn,rf,sgboost))
  res_names<-names(do.call(c,lapply(c(nb,svm,knn,rf,sgboost),function(x)names(x))))
  for(i in 1:length(res)){
    attr(res[[i]],"Datalist")<-res_names[[i]]
  }

  pic<-unlist(lapply(res,function(x) class(x)[[1]]=="list"))
  if(length(which(pic))>0){
    re0<-unlist(res[pic],recursive = F)
    names(re0)<-names(res[pic])
    res[pic]<-re0}
  res

}

#' @export
getmodelist_pd2<-function(saved_data){
  res<- lapply(saved_data, function(x) attr(x,"ensemble"))
  res_names<-names(do.call(c,lapply(res,function(x)names(x))))
  res2<-unlist(res,recursive = F)
  for(i in 1:length(res2)){
    attr(res2[[i]],"Datalist")<-res_names[[i]]
  }
  res2

}
#' @export
unregister_dopar <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}
