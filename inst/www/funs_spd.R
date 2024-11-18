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



  p<-GGally::ggpairs(df,
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



  p<-GGally::ggpairs(df,
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
                      tree_num, newcolhabs=list(turbo=viridis::turbo),palette="turbo", round=2,
                      tree_type="dendrogram",
                      edge_type="link",repel=T) {

  size=base_size/3


  tree <- randomForest::getTree(final_model,
                                k = tree_num,
                                labelVar = TRUE) %>%
    tibble::rownames_to_column() %>%
    # make leaf split points to NA, so the 0s won't get plotted
    dplyr::mutate(`split point` = ifelse(is.na(prediction), `split point`, NA))

  # prepare data frame for graph
  graph_frame <- data.frame(from = rep(tree$rowname, 2),
                            to = c(tree$`left daughter`, tree$`right daughter`))

  # convert to graph and delete the last node that we don't want to plot
  graph <- igraph::graph_from_data_frame(graph_frame) %>%
    igraph::delete_vertices("0")

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
    p<-p+ggraph::geom_edge_link()
  } else{
    p<- p+ggraph::geom_edge_diagonal(edge_width = 0.5, alpha =.4)

  }

  p<-p+
    ggraph::geom_node_point() +
    ggraph::geom_node_text(aes(label = node_label), vjust = "top",hjust="inward",na.rm = TRUE, repel = repel, color="darkblue", size=size) +
    ggraph::geom_node_label(aes(label = split),  vjust = "top", na.rm = TRUE, fill = "white", size=size) +
    ggraph::geom_node_label(aes(label = leaf_label, fill = leaf_label), na.rm = TRUE, size=size*1.2,
                    repel = repel, colour = "white", fontface = "bold", show.legend = FALSE)


  p<-p+labs(title=paste0("Tree:",tree_num),subtitle=  paste0(names(acc),":",acc, collapse="\n"))
  p<-p+scale_fill_manual(values=newcolhabs[[palette]](length(unique(na.omit(tree$prediction)))))

  p



}



