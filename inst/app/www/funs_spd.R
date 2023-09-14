#args<-readRDS("args.rds")

#attach(args)

gg_pairplot<-function(x,y,cols,xlab="",ylab="",title="",include.y=F,cor.size=1,varnames.size=1,points.size=1,axis.text.size=1,axis.title.size=1,plot.title.size=1,plot.subtitle.size=1,legend.text.size=1,legend.title.size=1,alpha.curve=.5, alpha.points=1, method = "pearson",round=3,title_corr=paste(method,"corr"),subtitle=paste("*** p< 0.001,","** p< 0.05 and","* p< 0.1"),switch = NULL,
                      upper="cor", pch=16){
  require(GGally)
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

      ggally_cor(data = df,
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
               ggally_points(data = df, mapping = mapping, size=points.size, shape=pch) + scale_colour_manual(values = col.points)}),
             diag = list(continuous = function(data, mapping, ...) {
               if(is.null(y)){
                 mapping$colour<-NULL
               }
               ggally_densityDiag(data = df, mapping = mapping) + scale_fill_manual(values = col.curve)}))

  p<-p+labs(title = title,
            tag = subtitle)
  p<-p+xlab(xlab)+ylab(ylab)

  p<- p+theme(
    plot.tag.position = "bottom",
    panel.grid.major = element_blank(),
    panel.background=element_rect(fill=NA, color="white"),
    panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"),
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


gg_pairplot2<-function(x,y,cols,xlab="",ylab="",title="",include.y=F,cor.size=1,varnames.size=1,points.size=1,axis.text.size=1,axis.title.size=1,plot.title.size=1,plot.subtitle.size=1,legend.text.size=1,legend.title.size=1,alpha.curve=.5, alpha.points=1, method = "pearson",round=3,title_corr=paste(method,"corr"),subtitle=paste("*** p< 0.001,","** p< 0.05 and","* p< 0.1"),
                      switch = NULL,
                      pch=16,
                      upper=c("corr","corr+group","blank"),
                      lower=c("points","points+group","blank"),
                      diag=c("density","density+group","hist","hist+group","blank")){
  diag<-match.arg(diag,c("density","density+group","hist","hist+group","blank"))
  lower<-match.arg(lower,c("points","points+group","blank"))
  upper<-match.arg(upper,c("corr","corr+group","blank"))
  require(GGally)
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
        ggally_cor(data = df,
                   mapping = mapping,
                   size=cor.size,

                   method=method,
                   digits = round,
                   title = title_corr)
      } else{
        ggally_cor(data = df,
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

      ggally_points(data = df, mapping = mapping, size=points.size, shape=pch) + scale_colour_manual(values = col.points)})

  } else{
    fun_lower<-"blank"
  }

  if(diag%in%c("density","density+group")){
    fun_diag<-list(continuous = function(data, mapping, ...) {
      if(is.null(y)){mapping$colour<-NULL}
      if(diag=="density"){
        mapping$colour<-NULL
        ggally_densityDiag(data = df, mapping = mapping)
      } else{
        ggally_densityDiag(data = df, mapping = mapping) +
          scale_fill_manual(values = col.curve)
      }
      })
  } else if(diag%in%c("hist","hist+group")) {
    fun_diag<-list(continuous = function(data, mapping, ...) {
      if(is.null(y)){mapping$colour<-NULL}
      if(diag=="hist"){
        mapping$colour<-NULL
        ggally_barDiag(data = df, mapping = mapping)
      } else{
        ggally_barDiag(data = df, mapping = mapping) +
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
    panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"),
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

tree_func <- function(final_model,acc="",base_size=11,predall=predict(final_model,newdata,predict.all=T)$individual,
                      tree_num, newcolhabs=list(turbo=turbo),palette="turbo", round=2,
                      tree_type="dendrogram",
                      edge_type="link") {
  require('ggraph')

  require('igraph')
  # get tree by index
  size=base_size/2


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
  V(graph)$node_label <- gsub("_", " ", as.character(tree$`split var`))
  V(graph)$leaf_label <- as.character(tree$prediction)
  V(graph)$split <- as.character(round(tree$`split point`, digits = round))

  if(tree_type=="dendrogram"){
    p<- ggraph(graph, 'dendrogram')
  } else{
    p<- ggraph(graph, layout = 'igraph', algorithm = 'tree')

  }

  if(edge_type=="link"){
    p<-p+geom_edge_link()
  } else{
    p<- p+geom_edge_diagonal(edge_width = 0.5, alpha =.4)

  }

  p<-p+
    theme_bw(base_size = base_size) +
    geom_node_point() +
    geom_node_text(aes(label = node_label), vjust = "top",hjust="inward",na.rm = TRUE, repel = TRUE, color="darkblue", size=size) +
    geom_node_label(aes(label = split),  vjust = "top", na.rm = TRUE, fill = "white", size=size) +
    geom_node_label(aes(label = leaf_label, fill = leaf_label), na.rm = TRUE, size=size*1.2,
                    repel = TRUE, colour = "white", fontface = "bold", show.legend = FALSE)


  p<-p+labs(title=paste0("Tree:",tree_num),subtitle=  paste0(names(acc),":",acc, collapse="\n"))
  p<-p+scale_fill_manual(values=newcolhabs[[palette]](length(unique(na.omit(tree$prediction)))))
  p<-p+
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.background = element_rect(fill = "white"),
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 18))

p+theme(
  text=element_text(size=base_size),
  panel.grid.major = element_blank(),
  panel.background=element_rect(fill=NA, color="white"),
  panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"),

  axis.line=element_line(),
  axis.text=element_text(size=base_size),
  axis.title=element_text(size=base_size),
  plot.title=element_text(size=base_size),
  plot.subtitle=element_text(size=base_size),
  legend.text=element_text(size=base_size),
  legend.title=element_text(size=base_size)
)


}


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
